# NHANES Adiposity-Diabetes Analysis Pipeline Extension

## Overview

This document describes the **extended analysis pipeline** for the NHANES adiposity-diabetes study. The pipeline includes new modules for prevalence ratios, odds ratios, spline models, and advanced incremental value metrics (ROC, calibration, NRI/IDI, DCA).

---

## Pipeline Structure

### Core Data Preparation (Existing)
- **dbwse01_unweighted sample.R**: Prepare analytical dataset from raw NHANES data
- **dbwse_mulitiple imputation.R**: Multiple imputation using MICE (10 imputations)
- **dbwse02_weighted sample with complete cases.R**: Create survey-weighted design objects

### Descriptive Analyses (Existing)
- **dbwse03a-i**: Stratified descriptive statistics by DM status, sex, race, year
- **dbwse03g**: Adjusted means of body composition metrics using emmeans

### Primary Analyses (Existing)
- **dbwse04_fpg and a1c model evaluation.R**: Linear models for continuous outcomes
- **dbwse05_dm prediction evaluation.R**: Logistic models + AUC comparison

### **NEW: Extended Analyses**
- **dbwse06_prevalence_ratio_models.R**: Poisson regression with robust variance
- **dbwse07_odds_ratio_models.R**: Logistic regression for odds ratios
- **dbwse08_continuous_outcomes_with_splines.R**: Restricted cubic splines
- **dbwse09_incremental_value_advanced.R**: ROC, calibration, NRI/IDI, DCA

### Master Script
- **dbwse00_master_analysis.R**: Orchestrates entire pipeline with error handling

---

## Dataset Specifications

### Input Data
- **Source**: `path_nhanes_dmbf_folder/working/cleaned/dbwse02_weighted df with complete cases.RDS`
- **Structure**: List of 10 multiply-imputed survey design objects
- **Sample Size**: N = 5,168 adults with complete cases

### Key Variables

| Category | Variable Names | Description |
|----------|----------------|-------------|
| **Outcome** | `dm` | Diabetes status: "NoDM" / "PreDM" / "DM" |
| | `dm_binary` | Binary: 1 = DM, 0 = NoDM (excludes PreDM) |
| | `predm_binary` | Binary: 1 = PreDM, 0 = NoDM (excludes DM) |
| **Adiposity** | `bmi` | Body mass index (kg/m²) |
| | `fat_percentage` | Total body fat percentage (%) |
| | `visceral_fat` | Visceral adipose tissue (g) |
| | `waistcircumference` | Waist circumference (cm) |
| | `WHtR` | Waist-to-height ratio (calculated) |
| **Glycemic** | `fasting_glucose` | Fasting plasma glucose (mg/dL) |
| | `glycohemoglobin` | Hemoglobin A1c (%) |
| **Covariates** | `age` | Age (years) |
| | `female` | Sex (0 = male, 1 = female) |
| | `race_eth` | Race/ethnicity (5 categories) |
| **Survey Design** | `psu` | Primary sampling unit |
| | `pseudostratum` | Stratum identifier |
| | `nhanes2yweight` | Survey weight (mec2yweight/4) |

### Diabetes Classification Criteria
- **DM**: Self-reported diagnosis OR FPG ≥126 mg/dL OR A1c ≥6.5%
- **PreDM**: FPG 100-125 mg/dL OR A1c 5.7-6.4% (without DM)
- **NoDM**: Neither DM nor PreDM

---

## New Analysis Modules

### dbwse06: Prevalence Ratio Models

**Purpose**: Survey-weighted Poisson regression with robust variance to estimate prevalence ratios.

**Models Fitted**:
1. **Single variable models**: Base (age, sex, race) + each adiposity metric
2. **Incremental models**: BMI + each additional adiposity metric

**Outcomes**:
- DM vs NoDM
- PreDM vs NoDM

**Output Files**:
- `dbwse06_prevalence_ratios_single.csv`
- `dbwse06_prevalence_ratios_incremental.csv`

**Key Columns**:
- `PR`: Prevalence ratio
- `CI_lower`, `CI_upper`: 95% confidence intervals
- `p_value`: Statistical significance

---

### dbwse07: Odds Ratio Models

**Purpose**: Survey-weighted logistic regression to estimate odds ratios.

**Models Fitted**:
1. **Single variable models**: Base + each adiposity metric
2. **Incremental models**: BMI + each additional adiposity metric
3. **BMI coefficients**: OR for BMI when adjusted for other metrics

**Outcomes**:
- DM vs NoDM
- PreDM vs NoDM

**Output Files**:
- `dbwse07_odds_ratios_single.csv`
- `dbwse07_odds_ratios_incremental.csv`
- `dbwse07_odds_ratios_bmi_adjusted.csv`

**Key Columns**:
- `OR`: Odds ratio
- `CI_lower`, `CI_upper`: 95% confidence intervals
- `p_value`: Statistical significance

---

### dbwse08: Continuous Outcomes with Splines

**Purpose**: Test for non-linear relationships using restricted cubic splines.

**Approach**:
- Fit linear models and spline models (3-5 knots) for each adiposity metric
- Compare model fit: pseudo-R², AIC, likelihood ratio test
- Generate plots showing linear vs spline predictions

**Outcomes**:
- Fasting plasma glucose (FPG)
- Hemoglobin A1c

**Adiposity Metrics Tested**:
- BMI, fat%, visceral fat, waist circumference, WHtR

**Output Files**:
- `dbwse08_spline_models.csv`: Model comparison metrics
- `dbwse08_spline_plots.pdf`: Visual comparison of linear vs spline fits

**Key Metrics**:
- `p_value_nonlinearity`: Test of non-linear association
- `delta_r2`: Improvement in pseudo-R² with splines
- `delta_AIC`: Change in AIC (negative = spline better)

---

### dbwse09: Advanced Incremental Value Analysis

**Purpose**: Comprehensive evaluation of incremental predictive value beyond BMI.

#### Components

**1. AUC Analysis**
- Calculate AUC with 95% CI (bootstrap)
- DeLong test comparing BMI vs BMI + other metrics
- Overall and race-stratified AUCs

**2. Calibration**
- Observed vs predicted risk across deciles
- Visual calibration plots

**3. Net Reclassification Improvement (NRI) & Integrated Discrimination Improvement (IDI)**
- NRI: Proportion correctly reclassified with new metric
- IDI: Difference in discrimination slopes
- Risk categories: <10%, 10-20%, >20%

**4. Decision Curve Analysis (DCA)**
- Net benefit across risk thresholds
- Compare BMI vs BMI + other metrics
- Identify clinically useful threshold ranges

**Comparisons**:
- BMI vs BMI + Fat%
- BMI vs BMI + Visceral Fat
- BMI vs BMI + Waist Circumference
- BMI vs BMI + WHtR

**Outcomes**:
- DM vs NoDM
- PreDM vs NoDM

**Output Files**:
- `dbwse09_auc_comparison.csv`: Overall AUC results
- `dbwse09_auc_by_race.csv`: Race-stratified AUCs
- `dbwse09_nri_idi.csv`: NRI and IDI results
- `dbwse09_calibration_data.csv`: Calibration statistics
- `dbwse09_roc_calibration_dca.pdf`: Combined figure with all plots

**Figures Included**:
1. ROC curves (overall) - DM and PreDM
2. ROC by race/ethnicity
3. Calibration plots - DM and PreDM
4. Decision curves - DM and PreDM

---

## Running the Pipeline

### Option 1: Run Master Script (Recommended)

```r
# Run all new analyses
source("complete cases/analysis/dbwse00_master_analysis.R")
```

The master script:
- Executes modules in correct sequence
- Provides error handling and progress tracking
- Reports runtime and success/failure status
- Can skip existing analyses (set `run = FALSE`)

### Option 2: Run Individual Modules

```r
# Run modules individually
source("complete cases/analysis/dbwse06_prevalence_ratio_models.R")
source("complete cases/analysis/dbwse07_odds_ratio_models.R")
source("complete cases/analysis/dbwse08_continuous_outcomes_with_splines.R")
source("complete cases/analysis/dbwse09_incremental_value_advanced.R")
```

---

## Required R Packages

### Core Packages (Already Installed)
- `survey`: Survey-weighted analyses
- `tidyverse`: Data manipulation and visualization
- `broom`: Model tidying
- `pROC`: ROC analysis

### New Packages (May Need Installation)
```r
install.packages("rms")        # Restricted cubic splines
install.packages("nricens")    # NRI/IDI calculation
install.packages("dcurves")    # Decision curve analysis
```

---

## Output Organization

### CSV Tables
Location: `complete cases/analysis/`

**Prevalence Ratios**:
- `dbwse06_prevalence_ratios_single.csv`
- `dbwse06_prevalence_ratios_incremental.csv`

**Odds Ratios**:
- `dbwse07_odds_ratios_single.csv`
- `dbwse07_odds_ratios_incremental.csv`
- `dbwse07_odds_ratios_bmi_adjusted.csv`

**Spline Models**:
- `dbwse08_spline_models.csv`

**Incremental Value**:
- `dbwse09_auc_comparison.csv`
- `dbwse09_auc_by_race.csv`
- `dbwse09_nri_idi.csv`
- `dbwse09_calibration_data.csv`

### Figures
Location: `[path_nhanes_dmbf_folder]/figures/complete cases/`

- `dbwse08_spline_plots.pdf`
- `dbwse09_roc_calibration_dca.pdf`

---

## Quality Assurance

### Reproducibility
- All scripts source `.Rprofile` for consistent paths
- Random seeds documented (e.g., bootstrap: 1000 iterations)
- Survey design properly specified for all analyses

### Error Handling
- Master script catches and reports errors
- Individual scripts include progress messages
- Output files confirm successful completion

### Consistency Checks
- Sample sizes reported in all output tables
- Models use identical covariate adjustment
- Survey weights properly applied throughout

---

## Interpretation Guidelines

### Prevalence Ratios vs Odds Ratios
- **PR**: More interpretable when outcome is common (>10%)
- **OR**: Traditional approach, may overestimate associations
- Both reported for transparency

### Spline Models
- **p < 0.05 for non-linearity**: Consider spline model
- **Visual inspection**: Check clinical plausibility
- **AIC improvement**: >10 units suggests meaningful improvement

### Incremental Value Metrics
- **AUC**: 
  - Δ AUC > 0.01 = potentially meaningful
  - DeLong p < 0.05 = statistically significant
- **NRI**:
  - >0 = net improvement in classification
  - Focus on event NRI for clinical utility
- **IDI**:
  - >0.01 = potentially meaningful improvement
- **DCA**:
  - Net benefit > 0 = clinically useful
  - Compare across threshold range

---

## Citation and Contact

**Study**: NHANES 2011-2018 Body Fat Percentage and BMI Misclassification

**Pipeline Version**: 2.0 (February 2026)

**Analysis Extension**: Prevalence ratios, odds ratios, splines, and advanced discrimination metrics

For questions about the analysis pipeline, contact the study team.

---

## Change Log

**Version 2.0 (February 2026)**:
- Added dbwse06: Prevalence ratio models
- Added dbwse07: Odds ratio models  
- Added dbwse08: Continuous outcomes with splines
- Added dbwse09: Advanced incremental value (ROC, calibration, NRI/IDI, DCA)
- Created dbwse00: Master analysis script

**Version 1.0 (Original)**:
- Data preparation and descriptive analyses (dbwse01-03)
- Linear models for FPG and A1c (dbwse04)
- Logistic models and basic AUC analysis (dbwse05)
