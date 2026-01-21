rm(list=ls());gc();source(".Rprofile")

library(survey)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Set survey option to handle strata with only one PSU
options(survey.lonely.psu = "adjust")

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# Combine all imputed data into one dataset for analysis
combined_data <- bind_rows(lapply(nhanes_svy_dfs, function(x) x$variables))

# Create survey design from combined data
nhanes_all_svy <- svydesign(
  ids = ~psu,
  strata = ~pseudostratum,
  weights = ~nhanes2yweight,
  data = combined_data,
  nest = TRUE
)

# Define adiposity measurements
adiposity_vars <- c("bmi", "fat_percentage", "visceral_fat", "subcutaneous_fat", 
                    "waistcircumference", "WHtR")

adiposity_labels <- c(
  "bmi" = "BMI",
  "fat_percentage" = "Body Fat %",
  "visceral_fat" = "Visceral Fat",
  "subcutaneous_fat" = "Subcutaneous Fat",
  "waistcircumference" = "Waist Circumference",
  "WHtR" = "Waist-to-Height Ratio"
)

#------------------------------------------------------------------------------------
# Calculate percentiles by sex and ethnicity (primary analysis)
#------------------------------------------------------------------------------------

percentile_data_sex_race <- data.frame()

for (var in adiposity_vars) {
  for (sex in c(0, 1)) {
    for (race in unique(nhanes_all_svy$variables$race_eth)) {
      # Subset data
      subset_data <- nhanes_all_svy %>%
        subset(female == sex & race_eth == race)
      
      # Skip if subset has fewer than 10 observations
      if (nrow(subset_data$variables) < 10) next
      
      tryCatch({
        # Calculate percentiles using survey-weighted quantiles
        q <- svyquantile(as.formula(paste0("~", var)), subset_data, 
                         quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90), 
                         ci = FALSE, na.rm = TRUE)
        
        # Calculate mean
        mean_val <- svymean(as.formula(paste0("~", var)), subset_data, na.rm = TRUE)
        
        # Calculate min and max
        min_val <- min(subset_data$variables[[var]], na.rm = TRUE)
        max_val <- max(subset_data$variables[[var]], na.rm = TRUE)
        
        percentile_data_sex_race <- rbind(
          percentile_data_sex_race,
          data.frame(
            variable = var,
            sex = ifelse(sex == 1, "Female", "Male"),
            race = race,
            min = min_val,
            q10 = as.numeric(q[1]),
            q25 = as.numeric(q[2]),
            q50 = as.numeric(q[3]),
            q75 = as.numeric(q[4]),
            q90 = as.numeric(q[5]),
            max = max_val,
            mean = as.numeric(mean_val)
          )
        )
      }, error = function(e) {
        print(paste("Skipping", var, "for sex =", sex, "race =", race, "due to error:", e$message))
      })
    }
  }
}

print("Percentile data by sex and ethnicity:")
print(percentile_data_sex_race)

#------------------------------------------------------------------------------------
# Create plots for primary analysis (by sex and ethnicity)
#------------------------------------------------------------------------------------

create_percentile_plot <- function(data, var_name, var_label) {
  p <- ggplot(data, aes(x = interaction(sex, race), y = q50)) +
    # Min-Max range (light gray background)
    geom_segment(aes(xend = interaction(sex, race), y = min, yend = max), 
                 color = "lightgray", size = 3, lineend = "butt") +
    # IQR (darker gray)
    geom_segment(aes(xend = interaction(sex, race), y = q25, yend = q75), 
                 color = "darkgray", size = 3, lineend = "butt") +
    # Deciles (Q10, Q90)
    geom_point(aes(y = q10), shape = "|", size = 5, color = "black") +
    geom_point(aes(y = q90), shape = "|", size = 5, color = "black") +
    # Median (Q50)
    geom_point(aes(y = q50), shape = 21, size = 4, fill = "white", color = "black") +
    # Mean
    geom_point(aes(y = mean), shape = 23, size = 3, fill = "red", color = "red") +
    # Add text labels
    geom_text(aes(y = min, label = round(min, 1)), vjust = 1.5, size = 3) +
    geom_text(aes(y = max, label = round(max, 1)), vjust = -0.5, size = 3) +
    geom_text(aes(y = q10, label = round(q10, 1)), hjust = -0.5, size = 2.5) +
    geom_text(aes(y = q90, label = round(q90, 1)), hjust = -0.5, size = 2.5) +
    geom_text(aes(y = mean, label = paste0("mean=", round(mean, 1))), 
              hjust = -0.5, size = 2.5, color = "red") +
    coord_flip() +
    labs(title = var_label, x = "", y = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Create individual plots for each adiposity measure
plots_list <- list()
for (var in adiposity_vars) {
  var_data <- percentile_data_sex_race %>% filter(variable == var)
  plots_list[[var]] <- create_percentile_plot(var_data, var, adiposity_labels[var])
}

# Combine plots
combined_plot_sex_race <- do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))

ggsave("paper/figures_percentile of adiposity measurements by sex and race.pdf",
       combined_plot_sex_race, width = 14, height = 10, dpi = 300)

print("Primary analysis plot saved!")

#------------------------------------------------------------------------------------
# Calculate percentiles by sex, ethnicity, and glycemic status (secondary analysis)
#------------------------------------------------------------------------------------

percentile_data_sex_race_dm <- data.frame()

for (var in adiposity_vars) {
  for (sex in c(0, 1)) {
    for (race in unique(nhanes_all_svy$variables$race_eth)) {
      for (dm_status in c("NoDM", "PreDM", "DM")) {
        # Subset data
        subset_data <- nhanes_all_svy %>%
          subset(female == sex & race_eth == race & dm == dm_status)
        
        # Skip if subset is too small
        if (nrow(subset_data$variables) < 10) next
        
        tryCatch({
          # Calculate percentiles using survey-weighted quantiles
          q <- svyquantile(as.formula(paste0("~", var)), subset_data, 
                           quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90), 
                           ci = FALSE, na.rm = TRUE)
          
          # Calculate mean
          mean_val <- svymean(as.formula(paste0("~", var)), subset_data, na.rm = TRUE)
          
          # Calculate min and max
          min_val <- min(subset_data$variables[[var]], na.rm = TRUE)
          max_val <- max(subset_data$variables[[var]], na.rm = TRUE)
          
          percentile_data_sex_race_dm <- rbind(
            percentile_data_sex_race_dm,
            data.frame(
              variable = var,
              sex = ifelse(sex == 1, "Female", "Male"),
              race = race,
              dm = dm_status,
              min = min_val,
              q10 = as.numeric(q[1]),
              q25 = as.numeric(q[2]),
              q50 = as.numeric(q[3]),
              q75 = as.numeric(q[4]),
              q90 = as.numeric(q[5]),
              max = max_val,
              mean = as.numeric(mean_val)
            )
          )
        }, error = function(e) {
          print(paste("Skipping", var, "for sex =", sex, "race =", race, "dm =", dm_status, 
                      "due to error:", e$message))
        })
      }
    }
  }
}

print("Percentile data by sex, ethnicity, and glycemic status:")
print(percentile_data_sex_race_dm)

#------------------------------------------------------------------------------------
# Create plots for secondary analysis (by sex, ethnicity, and glycemic status)
#------------------------------------------------------------------------------------

create_percentile_plot_dm <- function(data, var_name, var_label) {
  p <- ggplot(data, aes(x = interaction(sex, race, dm), y = q50, color = dm)) +
    # Min-Max range
    geom_segment(aes(xend = interaction(sex, race, dm), y = min, yend = max), 
                 color = "lightgray", size = 2, lineend = "butt") +
    # IQR
    geom_segment(aes(xend = interaction(sex, race, dm), y = q25, yend = q75), 
                 size = 2.5, lineend = "butt") +
    # Deciles
    geom_point(aes(y = q10), shape = "|", size = 4) +
    geom_point(aes(y = q90), shape = "|", size = 4) +
    # Median
    geom_point(aes(y = q50), shape = 21, size = 3, fill = "white", color = "black") +
    # Mean
    geom_point(aes(y = mean), shape = 23, size = 2.5, fill = "yellow", color = "black") +
    coord_flip() +
    labs(title = var_label, x = "", y = "", color = "Glycemic Status") +
    scale_color_manual(values = c("NoDM" = "blue", "PreDM" = "orange", "DM" = "red")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  return(p)
}

# Create individual plots for each adiposity measure
plots_list_dm <- list()
for (var in adiposity_vars) {
  var_data <- percentile_data_sex_race_dm %>% filter(variable == var)
  if (nrow(var_data) > 0) {
    plots_list_dm[[var]] <- create_percentile_plot_dm(var_data, var, adiposity_labels[var])
  }
}

# Combine plots
combined_plot_sex_race_dm <- do.call(gridExtra::grid.arrange, c(plots_list_dm, ncol = 2))

ggsave("paper/figures_percentile of adiposity measurements by sex, race and glycemic status.pdf",
       combined_plot_sex_race_dm, width = 16, height = 12, dpi = 300)

print("Secondary analysis plot saved!")

# Save data tables
write_csv(percentile_data_sex_race, 
          "paper/percentile_adiposity_by_sex_race.csv")
write_csv(percentile_data_sex_race_dm, 
          "paper/percentile_adiposity_by_sex_race_glycemic_status.csv")

print("Analysis complete!")
