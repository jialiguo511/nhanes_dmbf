rm(list=ls());gc();source(".Rprofile")

library(survey)
library(srvyr)
library(tidyverse)
library(broom)

cat("\n=== dbwse05: Odds Ratio Models ===\n")
cat("Loading survey design objects...\n")

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# Use first imputation for model evaluation
nhanes_data <- nhanes_svy_dfs[[1]]$variables
nhanes_svy <- nhanes_svy_dfs[[1]]

# Check required columns
required_cols <- c("dm", "age", "female", "race_eth", "bmi", "fat_percentage", 
                   "visceral_fat", "waistcircumference", "WHtR", 
                   "fasting_glucose", "glycohemoglobin")
missing_cols <- setdiff(required_cols, names(nhanes_data))
if(length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse=", "))
}

cat("Standardizing adiposity metrics (z-scores using overall sample)...\n")

# Standardize adiposity metrics using OVERALL sample statistics
adiposity_vars <- c("bmi", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR")
for(var in adiposity_vars) {
  mean_val <- mean(nhanes_data[[var]], na.rm = TRUE)
  sd_val <- sd(nhanes_data[[var]], na.rm = TRUE)
  z_var <- paste0("z_", var)
  nhanes_data[[z_var]] <- (nhanes_data[[var]] - mean_val) / sd_val
}

# Update the survey design with standardized variables
nhanes_svy$variables <- nhanes_data

#------------------------------------------------------------------------------------
# Helper function to extract OR and 95% CI from logistic model
#------------------------------------------------------------------------------------

extract_or <- function(model, var_name, model_type, outcome_name) {
  # Get coefficients
  coef_summary <- summary(model)$coefficients
  
  if (var_name %in% rownames(coef_summary)) {
    log_or <- as.numeric(coef_summary[var_name, "Estimate"])
    se <- as.numeric(coef_summary[var_name, "Std. Error"])
    p_value <- as.numeric(coef_summary[var_name, "Pr(>|t|)"])
    
    # Calculate OR and 95% CI per 1-SD increase
    or_val <- exp(log_or)
    ci_lower <- exp(log_or - 1.96 * se)
    ci_upper <- exp(log_or + 1.96 * se)
    
    # Clean predictor name
    predictor_clean <- gsub("z_", "", var_name)
    
    return(data.frame(
      outcome = outcome_name,
      model_type = model_type,
      predictor = predictor_clean,
      OR = or_val,
      CI_low = ci_lower,
      CI_high = ci_upper,
      p = p_value,
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
}

#------------------------------------------------------------------------------------
# Outcome 1: DM vs NoDM - Odds Ratios
#------------------------------------------------------------------------------------

cat("\nAnalyzing DM vs NoDM...\n")

# Filter survey design directly and create dm_binary
nhanes_dm_svy <- nhanes_svy %>%
  dplyr::filter(dm %in% c("DM", "NoDM")) %>%
  mutate(dm_binary = ifelse(dm == "DM", 1, 0))

cat(sprintf("  Sample size: N = %d\n", nrow(nhanes_dm_svy$variables)))
cat("  Fitting single-variable models...\n")

or_dm_bmi <- svyglm(dm_binary ~ age + female + race_eth + z_bmi, 
                    design = nhanes_dm_svy, family = binomial())

or_dm_fat <- svyglm(dm_binary ~ age + female + race_eth + z_fat_percentage, 
                    design = nhanes_dm_svy, family = binomial())

or_dm_vf <- svyglm(dm_binary ~ age + female + race_eth + z_visceral_fat, 
                   design = nhanes_dm_svy, family = binomial())

or_dm_wc <- svyglm(dm_binary ~ age + female + race_eth + z_waistcircumference, 
                   design = nhanes_dm_svy, family = binomial())

or_dm_whtr <- svyglm(dm_binary ~ age + female + race_eth + z_WHtR, 
                     design = nhanes_dm_svy, family = binomial())

# Extract ORs for single variable models
or_dm_results_single <- bind_rows(
  extract_or(or_dm_bmi, "z_bmi", "single", "DM vs NoDM"),
  extract_or(or_dm_fat, "z_fat_percentage", "single", "DM vs NoDM"),
  extract_or(or_dm_vf, "z_visceral_fat", "single", "DM vs NoDM"),
  extract_or(or_dm_wc, "z_waistcircumference", "single", "DM vs NoDM"),
  extract_or(or_dm_whtr, "z_WHtR", "single", "DM vs NoDM")
) %>%
  mutate(n = nrow(nhanes_dm_svy$variables))

# Incremental models (z_BMI + other z-scored adiposity metrics)
cat("  Fitting incremental models (BMI + other metrics)...\n")

or_dm_bmi_fat <- svyglm(dm_binary ~ age + female + race_eth + z_bmi + z_fat_percentage, 
                        design = nhanes_dm_svy, family = binomial())

or_dm_bmi_vf <- svyglm(dm_binary ~ age + female + race_eth + z_bmi + z_visceral_fat, 
                       design = nhanes_dm_svy, family = binomial())

or_dm_bmi_wc <- svyglm(dm_binary ~ age + female + race_eth + z_bmi + z_waistcircumference, 
                       design = nhanes_dm_svy, family = binomial())

or_dm_bmi_whtr <- svyglm(dm_binary ~ age + female + race_eth + z_bmi + z_WHtR, 
                         design = nhanes_dm_svy, family = binomial())

# Extract ORs for incremental models (focusing on the added variable)
or_dm_results_incremental <- bind_rows(
  extract_or(or_dm_bmi_fat, "z_fat_percentage", "incremental", "DM vs NoDM"),
  extract_or(or_dm_bmi_vf, "z_visceral_fat", "incremental", "DM vs NoDM"),
  extract_or(or_dm_bmi_wc, "z_waistcircumference", "incremental", "DM vs NoDM"),
  extract_or(or_dm_bmi_whtr, "z_WHtR", "incremental", "DM vs NoDM")
) %>%
  mutate(n = nrow(nhanes_dm_svy$variables))

#------------------------------------------------------------------------------------
# Outcome 2: PreDM vs NoDM - Odds Ratios
#------------------------------------------------------------------------------------

cat("\nAnalyzing PreDM vs NoDM...\n")

# Filter survey design directly and create predm_binary
nhanes_predm_svy <- nhanes_svy %>%
  dplyr::filter(dm %in% c("PreDM", "NoDM")) %>%
  mutate(predm_binary = ifelse(dm == "PreDM", 1, 0))

cat(sprintf("  Sample size: N = %d\n", nrow(nhanes_predm_svy$variables)))
cat("  Fitting single-variable models...\n")

or_predm_bmi <- svyglm(predm_binary ~ age + female + race_eth + z_bmi, 
                       design = nhanes_predm_svy, family = binomial())

or_predm_fat <- svyglm(predm_binary ~ age + female + race_eth + z_fat_percentage, 
                       design = nhanes_predm_svy, family = binomial())

or_predm_vf <- svyglm(predm_binary ~ age + female + race_eth + z_visceral_fat, 
                      design = nhanes_predm_svy, family = binomial())

or_predm_wc <- svyglm(predm_binary ~ age + female + race_eth + z_waistcircumference, 
                      design = nhanes_predm_svy, family = binomial())

or_predm_whtr <- svyglm(predm_binary ~ age + female + race_eth + z_WHtR, 
                        design = nhanes_predm_svy, family = binomial())

# Extract ORs for single variable models
or_predm_results_single <- bind_rows(
  extract_or(or_predm_bmi, "z_bmi", "single", "PreDM vs NoDM"),
  extract_or(or_predm_fat, "z_fat_percentage", "single", "PreDM vs NoDM"),
  extract_or(or_predm_vf, "z_visceral_fat", "single", "PreDM vs NoDM"),
  extract_or(or_predm_wc, "z_waistcircumference", "single", "PreDM vs NoDM"),
  extract_or(or_predm_whtr, "z_WHtR", "single", "PreDM vs NoDM")
) %>%
  mutate(n = nrow(nhanes_predm_svy$variables))

# Incremental models (z_BMI + other z-scored adiposity metrics)
cat("  Fitting incremental models (BMI + other metrics)...\n")

or_predm_bmi_fat <- svyglm(predm_binary ~ age + female + race_eth + z_bmi + z_fat_percentage, 
                           design = nhanes_predm_svy, family = binomial())

or_predm_bmi_vf <- svyglm(predm_binary ~ age + female + race_eth + z_bmi + z_visceral_fat, 
                          design = nhanes_predm_svy, family = binomial())

or_predm_bmi_wc <- svyglm(predm_binary ~ age + female + race_eth + z_bmi + z_waistcircumference, 
                          design = nhanes_predm_svy, family = binomial())

or_predm_bmi_whtr <- svyglm(predm_binary ~ age + female + race_eth + z_bmi + z_WHtR, 
                            design = nhanes_predm_svy, family = binomial())

# Extract ORs for incremental models
or_predm_results_incremental <- bind_rows(
  extract_or(or_predm_bmi_fat, "z_fat_percentage", "incremental", "PreDM vs NoDM"),
  extract_or(or_predm_bmi_vf, "z_visceral_fat", "incremental", "PreDM vs NoDM"),
  extract_or(or_predm_bmi_wc, "z_waistcircumference", "incremental", "PreDM vs NoDM"),
  extract_or(or_predm_bmi_whtr, "z_WHtR", "incremental", "PreDM vs NoDM")
) %>%
  mutate(n = nrow(nhanes_predm_svy$variables))

#------------------------------------------------------------------------------------
# Combine and save results
#------------------------------------------------------------------------------------

cat("\nSaving results...\n")

# Combine all results
all_or_results <- bind_rows(
  or_dm_results_single,
  or_dm_results_incremental,
  or_predm_results_single,
  or_predm_results_incremental
) %>%
  mutate(N = n) %>%
  select(outcome, model_type, predictor, OR, CI_low, CI_high, p, N)

# Save combined results
all_or_results %>%
  write_csv(., "complete cases/analysis/dbwse05_odds_ratios.csv")

# Print summary
cat("\n=== Odds Ratios Summary (per 1-SD increase) ===\n")
print(all_or_results %>%
        mutate(across(where(is.numeric) & !N, ~round(., 3))))

cat("\n✓ Odds ratio analysis complete!\n")
cat("  Output file: complete cases/analysis/dbwse05_odds_ratios.csv\n")
cat(sprintf("  Total rows: %d\n", nrow(all_or_results)))
