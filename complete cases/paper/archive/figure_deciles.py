import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pickle

# Load the imputed datasets
path_nhanes_dmbf_folder = "C:/Users/JGUO258/OneDrive - Emory/Papers/NHANES Body Fat Mass in Diabetes Cases"
with open(f"{path_nhanes_dmbf_folder}/working/cleaned/dbwse02_weighted df with complete cases.RDS", "rb") as f:
    # Note: This assumes the file is accessible - adjust if using different format
    pass

# For now, let's create a simpler version that works with plain data
# Load your datasets - adjust path as needed
import rpy2
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
pandas2ri.activate()

# Load data from R
from rpy2 import robjects as ro
ro.r('source(".Rprofile")')
ro.r('''
nhanes_svy_dfs <- readRDS(
  paste0(path_nhanes_dmbf_folder,
         "/working/cleaned/dbwse02_weighted df with complete cases.RDS")
)
''')

# Extract to pandas
ro.r('''
# Convert first imputation to data frame for Python
test_data <- nhanes_svy_dfs[[1]]$variables
''')
test_data = ro.r('test_data')
df = pandas2ri.rpy2py(test_data)

# Now compute means and quantiles
results = []

for female in [0, 1]:
    for dm in ['NoDM', 'PreDM', 'DM']:
        subset = df[(df['female'] == female) & (df['dm'] == dm)]
        
        if len(subset) == 0:
            continue
        
        bmi_values = subset['bmi'].dropna()
        
        # Mean
        mean_val = bmi_values.mean()
        se_val = bmi_values.sem()  # standard error
        
        results.append({
            'female': female,
            'dm': dm,
            'stat': 'mean',
            'prob': np.nan,
            'est': mean_val,
            'se': se_val
        })
        
        # Deciles (10th to 90th percentile)
        for p in np.arange(0.1, 1.0, 0.1):
            q_val = bmi_values.quantile(p)
            results.append({
                'female': female,
                'dm': dm,
                'stat': 'quantile',
                'prob': p,
                'est': q_val,
                'se': np.nan
            })

# Convert to dataframe
results_df = pd.DataFrame(results)

# Create plot
fig, ax = plt.subplots(figsize=(12, 6))

# Prepare data for plotting
sex_labels = {0: 'Male', 1: 'Female'}
dm_order = ['NoDM', 'PreDM', 'DM']
dm_labels = {'NoDM': 'No DM', 'PreDM': 'Pre-DM', 'DM': 'DM'}

mean_data = results_df[results_df['stat'] == 'mean'].copy()
quant_data = results_df[results_df['stat'] == 'quantile'].copy()

# Assign x positions
x_pos = 0
x_mapping = {}
group_labels = []
bar_positions = []

for female in [0, 1]:
    for dm in dm_order:
        group = f"{sex_labels[female]} | {dm_labels[dm]}"
        x_mapping[(female, dm)] = x_pos
        group_labels.append(group)
        bar_positions.append(x_pos)
        x_pos += 1

# Plot bars with error bars
mean_data['x'] = mean_data.apply(lambda r: x_mapping[(r['female'], r['dm'])], axis=1)
quant_data['x'] = quant_data.apply(lambda r: x_mapping[(r['female'], r['dm'])], axis=1)

# Bars for means
for idx, row in mean_data.iterrows():
    ax.bar(row['x'], row['est'], width=0.75, alpha=0.7, color='steelblue')
    # Error bars (95% CI)
    ci_width = 1.96 * row['se']
    ax.errorbar(row['x'], row['est'], yerr=ci_width, fmt='none', color='black', capsize=5, linewidth=0.5)

# Horizontal lines for deciles
for (female, dm), group_x in x_mapping.items():
    deciles = quant_data[(quant_data['female'] == female) & (quant_data['dm'] == dm)].sort_values('prob')
    for idx, row in deciles.iterrows():
        ax.plot([group_x - 0.35, group_x + 0.35], [row['est'], row['est']], 
                color='darkred', linewidth=0.6, alpha=0.8)

# Formatting
ax.set_xticks(bar_positions)
ax.set_xticklabels(group_labels, rotation=45, ha='right')
ax.set_ylabel('BMI', fontsize=12)
ax.set_xlabel('')
ax.grid(axis='y', alpha=0.3)

plt.tight_layout()
plt.savefig('figure_deciles.png', dpi=300, bbox_inches='tight')
plt.show()

print("Plot saved as figure_deciles.png")
