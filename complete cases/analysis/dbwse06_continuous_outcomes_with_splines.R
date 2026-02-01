rm(list=ls());gc();source(".Rprofile")

library(survey)
library(srvyr)
library(tidyverse)
library(splines)

cat("\n=== dbwse06: Continuous Outcomes with Splines ===\n")
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
# Helper function to compare linear vs spline models
#------------------------------------------------------------------------------------

compare_linear_spline <- function(outcome_var, predictor_z, design, outcome_name, predictor_name, model_type) {
  
  # Linear model
  formula_linear <- as.formula(paste0(outcome_var, " ~ age + female + race_eth + ", predictor_z))
  model_linear <- svyglm(formula_linear, design = design)
  
  # Spline model (natural splines with df=4)
  formula_spline <- as.formula(paste0(outcome_var, " ~ age + female + race_eth + ns(", predictor_z, ", df=4)"))
  model_spline <- svyglm(formula_spline, design = design)
  
  # Extract linear coefficient
  coef_linear <- summary(model_linear)$coefficients
  if (predictor_z %in% rownames(coef_linear)) {
    beta_linear <- as.numeric(coef_linear[predictor_z, "Estimate"])
    se_linear <- as.numeric(coef_linear[predictor_z, "Std. Error"])
    p_linear <- as.numeric(coef_linear[predictor_z, "Pr(>|t|)"])
  } else {
    return(NULL)
  }
  
  # Likelihood ratio test for nonlinearity
  # Use deviance difference (approximation for survey designs)
  lr_stat <- model_linear$deviance - model_spline$deviance
  df_diff <- length(coef(model_spline)) - length(coef(model_linear))
  p_nonlinearity <- 1 - pchisq(lr_stat, df_diff)
  
  # Sample size
  n_obs <- nrow(design$variables)
  
  return(data.frame(
    outcome = outcome_name,
    model_type = model_type,
    predictor = predictor_name,
    beta_linear = beta_linear,
    SE_linear = se_linear,
    p_linear = p_linear,
    LR_statistic = lr_stat,
    LR_df = df_diff,
    p_nonlinearity = p_nonlinearity,
    N = n_obs,
    stringsAsFactors = FALSE
  ))
}

#------------------------------------------------------------------------------------
# Single-metric models: FPG outcome
#------------------------------------------------------------------------------------

cat("\nAnalyzing FPG outcome (single-metric models)...\n")

fpg_results_single <- bind_rows(
  compare_linear_spline("fasting_glucose", "z_bmi", nhanes_svy, 
                        "FPG", "bmi", "single"),
  compare_linear_spline("fasting_glucose", "z_fat_percentage", nhanes_svy, 
                        "FPG", "fat_percentage", "single"),
  compare_linear_spline("fasting_glucose", "z_visceral_fat", nhanes_svy, 
                        "FPG", "visceral_fat", "single"),
  compare_linear_spline("fasting_glucose", "z_waistcircumference", nhanes_svy, 
                        "FPG", "waistcircumference", "single"),
  compare_linear_spline("fasting_glucose", "z_WHtR", nhanes_svy, 
                        "FPG", "WHtR", "single")
)

#------------------------------------------------------------------------------------
# Single-metric models: A1c outcome
#------------------------------------------------------------------------------------

cat("Analyzing A1c outcome (single-metric models)...\n")

a1c_results_single <- bind_rows(
  compare_linear_spline("glycohemoglobin", "z_bmi", nhanes_svy, 
                        "A1c", "bmi", "single"),
  compare_linear_spline("glycohemoglobin", "z_fat_percentage", nhanes_svy, 
                        "A1c", "fat_percentage", "single"),
  compare_linear_spline("glycohemoglobin", "z_visceral_fat", nhanes_svy, 
                        "A1c", "visceral_fat", "single"),
  compare_linear_spline("glycohemoglobin", "z_waistcircumference", nhanes_svy, 
                        "A1c", "waistcircumference", "single"),
  compare_linear_spline("glycohemoglobin", "z_WHtR", nhanes_svy, 
                        "A1c", "WHtR", "single")
)

#------------------------------------------------------------------------------------
# Incremental models: FPG outcome (BMI + other metric)
#------------------------------------------------------------------------------------

cat("Analyzing FPG outcome (incremental models)...\n")

# Helper function for incremental spline models
compare_incremental_spline <- function(outcome_var, base_z, added_z, design, 
                                       outcome_name, added_predictor_name, model_type) {
  
  # Linear incremental model
  formula_linear <- as.formula(paste0(outcome_var, " ~ age + female + race_eth + ", 
                                      base_z, " + ", added_z))
  model_linear <- svyglm(formula_linear, design = design)
  
  # Spline incremental model (spline on added variable)
  formula_spline <- as.formula(paste0(outcome_var, " ~ age + female + race_eth + ", 
                                      base_z, " + ns(", added_z, ", df=4)"))
  model_spline <- svyglm(formula_spline, design = design)
  
  # Extract linear coefficient for added variable
  coef_linear <- summary(model_linear)$coefficients
  if (added_z %in% rownames(coef_linear)) {
    beta_linear <- as.numeric(coef_linear[added_z, "Estimate"])
    se_linear <- as.numeric(coef_linear[added_z, "Std. Error"])
    p_linear <- as.numeric(coef_linear[added_z, "Pr(>|t|)"])
  } else {
    return(NULL)
  }
  
  # LR test for nonlinearity
  lr_stat <- model_linear$deviance - model_spline$deviance
  df_diff <- length(coef(model_spline)) - length(coef(model_linear))
  p_nonlinearity <- 1 - pchisq(lr_stat, df_diff)
  
  n_obs <- nrow(design$variables)
  
  return(data.frame(
    outcome = outcome_name,
    model_type = model_type,
    predictor = added_predictor_name,
    beta_linear = beta_linear,
    SE_linear = se_linear,
    p_linear = p_linear,
    LR_statistic = lr_stat,
    LR_df = df_diff,
    p_nonlinearity = p_nonlinearity,
    N = n_obs,
    stringsAsFactors = FALSE
  ))
}

fpg_results_incremental <- bind_rows(
  compare_incremental_spline("fasting_glucose", "z_bmi", "z_fat_percentage", 
                             nhanes_svy, "FPG", "fat_percentage", "incremental"),
  compare_incremental_spline("fasting_glucose", "z_bmi", "z_visceral_fat", 
                             nhanes_svy, "FPG", "visceral_fat", "incremental"),
  compare_incremental_spline("fasting_glucose", "z_bmi", "z_waistcircumference", 
                             nhanes_svy, "FPG", "waistcircumference", "incremental"),
  compare_incremental_spline("fasting_glucose", "z_bmi", "z_WHtR", 
                             nhanes_svy, "FPG", "WHtR", "incremental")
)

#------------------------------------------------------------------------------------
# Incremental models: A1c outcome (BMI + other metric)
#------------------------------------------------------------------------------------

cat("Analyzing A1c outcome (incremental models)...\n")

a1c_results_incremental <- bind_rows(
  compare_incremental_spline("glycohemoglobin", "z_bmi", "z_fat_percentage", 
                             nhanes_svy, "A1c", "fat_percentage", "incremental"),
  compare_incremental_spline("glycohemoglobin", "z_bmi", "z_visceral_fat", 
                             nhanes_svy, "A1c", "visceral_fat", "incremental"),
  compare_incremental_spline("glycohemoglobin", "z_bmi", "z_waistcircumference", 
                             nhanes_svy, "A1c", "waistcircumference", "incremental"),
  compare_incremental_spline("glycohemoglobin", "z_bmi", "z_WHtR", 
                             nhanes_svy, "A1c", "WHtR", "incremental")
)

#------------------------------------------------------------------------------------
# Combine and save results
#------------------------------------------------------------------------------------

cat("\nSaving results...\n")

all_spline_results <- bind_rows(
  fpg_results_single,
  fpg_results_incremental,
  a1c_results_single,
  a1c_results_incremental
)

# Save results
all_spline_results %>%
  write_csv(., "complete cases/analysis/dbwse06_spline_models.csv")

# Print summary
cat("\n=== Spline Analysis Summary ===\n")
print(all_spline_results %>%
        mutate(across(where(is.numeric) & !c(N, LR_df), ~round(., 3))))

cat("\n✓ Spline analysis complete!\n")
cat("  Output file: complete cases/analysis/dbwse06_spline_models.csv\n")
cat(sprintf("  Total rows: %d\n", nrow(all_spline_results)))
