rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(tidyverse)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# Use first imputation for model evaluation
nhanes_data <- nhanes_svy_dfs[[1]]$variables

# Create survey design for model evaluation
nhanes_svy <- nhanes_svy_dfs[[1]]

#------------------------------------------------------------------------------------
# 1.1 Single Variable Models for Continuous Outcomes (FPG and A1c)
#------------------------------------------------------------------------------------

# ---- FPG Models ----
fpg_b0 <- svyglm(fasting_glucose ~ age + female + race_eth, design = nhanes_svy)
fpg_b1 <- svyglm(fasting_glucose ~ age + female + race_eth + bmi, design = nhanes_svy)
fpg_b2 <- svyglm(fasting_glucose ~ age + female + race_eth + fat_percentage, design = nhanes_svy)
fpg_b3 <- svyglm(fasting_glucose ~ age + female + race_eth + visceral_fat, design = nhanes_svy)
fpg_b4 <- svyglm(fasting_glucose ~ age + female + race_eth + WHtR, design = nhanes_svy)

# ---- A1c Models ----
a1c_b0 <- svyglm(glycohemoglobin ~ age + female + race_eth, design = nhanes_svy)
a1c_b1 <- svyglm(glycohemoglobin ~ age + female + race_eth + bmi, design = nhanes_svy)
a1c_b2 <- svyglm(glycohemoglobin ~ age + female + race_eth + fat_percentage, design = nhanes_svy)
a1c_b3 <- svyglm(glycohemoglobin ~ age + female + race_eth + visceral_fat, design = nhanes_svy)
a1c_b4 <- svyglm(glycohemoglobin ~ age + female + race_eth + WHtR, design = nhanes_svy)

# Function to extract model metrics
extract_metrics <- function(model, outcome_name, model_name) {
  # Get coefficients
  coef_list <- coef(model)
  
  # Extract key predictor (the body composition variable)
  body_comp_vars <- c("bmi", "fat_percentage", "visceral_fat", "WHtR")
  key_var_name <- NA
  key_coef <- NA
  key_se <- NA
  key_pval <- NA
  std_beta <- NA
  
  # Find the matching body composition variable
  for (var in body_comp_vars) {
    if (var %in% names(coef_list)) {
      key_var_name <- var
      key_coef <- as.numeric(coef_list[[var]])
      
      # Get standard error from model summary
      coef_summary <- summary(model)$coefficients
      if (var %in% rownames(coef_summary)) {
        key_se <- as.numeric(coef_summary[var, "Std. Error"])
        key_pval <- as.numeric(coef_summary[var, "Pr(>|t|)"])
      }
      
      # Calculate standardized beta
      sd_predictor <- sd(nhanes_data[[var]], na.rm = TRUE)
      sd_outcome <- sd(nhanes_data[[outcome_name]], na.rm = TRUE)
      std_beta <- as.numeric(key_coef) * as.numeric(sd_predictor) / as.numeric(sd_outcome)
      break
    }
  }
  
  # Extract AIC and BIC - be very explicit for survey models
  # AIC may return multiple values for survey models, so we take the first
  aic_raw <- AIC(model)
  if (length(aic_raw) > 1) {
    aic <- as.numeric(aic_raw[1])
  } else {
    aic <- as.numeric(aic_raw)
  }
  
  # Get number of observations
  n <- as.integer(length(model$residuals))
  
  # Calculate BIC: BIC = deviance + k*log(n)
  deviance_val <- as.numeric(model$deviance)
  k <- as.integer(length(coef(model)))
  bic <- as.numeric(deviance_val + k * log(n))
  
  # Calculate pseudo-R²
  null_deviance <- as.numeric(model$null.deviance)
  residual_deviance <- as.numeric(model$deviance)
  
  if (!is.null(null_deviance) && !is.null(residual_deviance)) {
    pseudo_r2 <- as.numeric(1 - (residual_deviance / null_deviance))
  } else {
    pseudo_r2 <- NA_real_
  }
  
  # Create result as a single-row data frame
  data.frame(
    outcome = outcome_name,
    model = model_name,
    key_variable = key_var_name,
    coefficient = key_coef,
    std_error = key_se,
    p_value = key_pval,
    standardized_beta = std_beta,
    pseudo_r2 = pseudo_r2,
    AIC = aic,
    BIC = bic,
    n = n,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Extract metrics for all models - store each result
m1 <- extract_metrics(fpg_b0, "fasting_glucose", "B0: Base")
m2 <- extract_metrics(fpg_b1, "fasting_glucose", "B1: + BMI")
m3 <- extract_metrics(fpg_b2, "fasting_glucose", "B2: + Fat%")
m4 <- extract_metrics(fpg_b3, "fasting_glucose", "B3: + VF")
m5 <- extract_metrics(fpg_b4, "fasting_glucose", "B4: + WHtR")
m6 <- extract_metrics(a1c_b0, "glycohemoglobin", "B0: Base")
m7 <- extract_metrics(a1c_b1, "glycohemoglobin", "B1: + BMI")
m8 <- extract_metrics(a1c_b2, "glycohemoglobin", "B2: + Fat%")
m9 <- extract_metrics(a1c_b3, "glycohemoglobin", "B3: + VF")
m10 <- extract_metrics(a1c_b4, "glycohemoglobin", "B4: + WHtR")

single_var_models <- as_tibble(bind_rows(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10))


single_var_models %>%
  write_csv(., "complete cases/analysis/dbwse04_single variable models.csv")


#------------------------------------------------------------------------------------
# 1.2 Incremental Value Models (BMI + other variables)
#------------------------------------------------------------------------------------

# ---- FPG Incremental Models ----
fpg_c1 <- svyglm(fasting_glucose ~ age + female + race_eth + bmi + fat_percentage, design = nhanes_svy)
fpg_c2 <- svyglm(fasting_glucose ~ age + female + race_eth + bmi + visceral_fat, design = nhanes_svy)
fpg_c3 <- svyglm(fasting_glucose ~ age + female + race_eth + bmi + WHtR, design = nhanes_svy)

# ---- A1c Incremental Models ----
a1c_c1 <- svyglm(glycohemoglobin ~ age + female + race_eth + bmi + fat_percentage, design = nhanes_svy)
a1c_c2 <- svyglm(glycohemoglobin ~ age + female + race_eth + bmi + visceral_fat, design = nhanes_svy)
a1c_c3 <- svyglm(glycohemoglobin ~ age + female + race_eth + bmi + WHtR, design = nhanes_svy)

# Function to extract incremental metrics
extract_incremental_metrics <- function(model_base, model_full, added_var, outcome_name, model_name) {
  
  # Get coefficients from full model
  coef_list_full <- coef(model_full)
  coef_summary_full <- summary(model_full)$coefficients
  
  # Extract added variable coefficient
  added_coef <- NA
  added_se <- NA
  added_pval <- NA
  std_beta_added <- NA
  
  if (added_var %in% names(coef_list_full)) {
    added_coef <- as.numeric(coef_list_full[[added_var]])
    
    if (added_var %in% rownames(coef_summary_full)) {
      added_se <- as.numeric(coef_summary_full[added_var, "Std. Error"])
      added_pval <- as.numeric(coef_summary_full[added_var, "Pr(>|t|)"])
    }
    
    # Calculate standardized beta
    sd_predictor <- sd(nhanes_data[[added_var]], na.rm = TRUE)
    sd_outcome <- sd(nhanes_data[[outcome_name]], na.rm = TRUE)
    std_beta_added <- as.numeric(added_coef) * as.numeric(sd_predictor) / as.numeric(sd_outcome)
  }
  
  # Calculate R² for both models
  null_deviance_base <- as.numeric(model_base$null.deviance)
  residual_deviance_base <- as.numeric(model_base$deviance)
  pseudo_r2_base <- as.numeric(1 - (residual_deviance_base / null_deviance_base))
  
  null_deviance_full <- as.numeric(model_full$null.deviance)
  residual_deviance_full <- as.numeric(model_full$deviance)
  pseudo_r2_full <- as.numeric(1 - (residual_deviance_full / null_deviance_full))
  
  delta_r2 <- as.numeric(pseudo_r2_full - pseudo_r2_base)
  
  # Extract AIC and BIC - be very explicit for survey models
  # AIC may return multiple values for survey models, so we take the first
  aic_base_raw <- AIC(model_base)
  aic_full_raw <- AIC(model_full)
  
  if (length(aic_base_raw) > 1) {
    aic_base <- as.numeric(aic_base_raw[1])
  } else {
    aic_base <- as.numeric(aic_base_raw)
  }
  
  if (length(aic_full_raw) > 1) {
    aic_full <- as.numeric(aic_full_raw[1])
  } else {
    aic_full <- as.numeric(aic_full_raw)
  }
  
  delta_aic <- as.numeric(aic_full - aic_base)
  
  # Get sample sizes
  n_base <- as.integer(length(model_base$residuals))
  n_full <- as.integer(length(model_full$residuals))
  
  # Calculate BIC manually using deviance
  deviance_base <- as.numeric(model_base$deviance)
  k_base <- as.integer(length(coef(model_base)))
  bic_base <- as.numeric(deviance_base + k_base * log(n_base))
  
  deviance_full <- as.numeric(model_full$deviance)
  k_full <- as.integer(length(coef(model_full)))
  bic_full <- as.numeric(deviance_full + k_full * log(n_full))
  
  delta_bic <- as.numeric(bic_full - bic_base)
  
  # Create result as a single-row data frame
  data.frame(
    outcome = outcome_name,
    model = model_name,
    added_variable = added_var,
    added_coefficient = added_coef,
    added_std_error = added_se,
    added_p_value = added_pval,
    added_standardized_beta = std_beta_added,
    pseudo_r2_base = pseudo_r2_base,
    pseudo_r2_full = pseudo_r2_full,
    delta_r2 = delta_r2,
    AIC_base = aic_base,
    AIC_full = aic_full,
    delta_AIC = delta_aic,
    BIC_base = bic_base,
    BIC_full = bic_full,
    delta_BIC = delta_bic,
    n = n_full,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Extract metrics for incremental models - store each result
c1 <- extract_incremental_metrics(fpg_b1, fpg_c1, "fat_percentage", "fasting_glucose", "C1: BMI + Fat%")
c2 <- extract_incremental_metrics(fpg_b1, fpg_c2, "visceral_fat", "fasting_glucose", "C2: BMI + VF")
c3 <- extract_incremental_metrics(fpg_b1, fpg_c3, "WHtR", "fasting_glucose", "C3: BMI + WHtR")
c4 <- extract_incremental_metrics(a1c_b1, a1c_c1, "fat_percentage", "glycohemoglobin", "C1: BMI + Fat%")
c5 <- extract_incremental_metrics(a1c_b1, a1c_c2, "visceral_fat", "glycohemoglobin", "C2: BMI + VF")
c6 <- extract_incremental_metrics(a1c_b1, a1c_c3, "WHtR", "glycohemoglobin", "C3: BMI + WHtR")

incremental_models <- as_tibble(bind_rows(c1, c2, c3, c4, c5, c6))


incremental_models %>%
  write_csv(., "complete cases/analysis/dbwse04_incremental value models.csv")


#------------------------------------------------------------------------------------
# Summary: Model Comparison Tables
#------------------------------------------------------------------------------------

# FPG Models Summary
fpg_summary <- single_var_models %>%
  dplyr::filter(outcome == "fasting_glucose") %>%
  select(model, key_variable, coefficient, standardized_beta, pseudo_r2, AIC, BIC, p_value) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print("\n=== FPG Model Comparison ===")
print(fpg_summary)

# A1c Models Summary
a1c_summary <- single_var_models %>%
  dplyr::filter(outcome == "glycohemoglobin") %>%
  select(model, key_variable, coefficient, standardized_beta, pseudo_r2, AIC, BIC, p_value) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print("\n=== A1c Model Comparison ===")
print(a1c_summary)

# Incremental models summary
fpg_incremental_summary <- incremental_models %>%
  dplyr::filter(outcome == "fasting_glucose") %>%
  select(model, added_variable, added_coefficient, added_standardized_beta, 
         delta_r2, delta_AIC, delta_BIC, added_p_value) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print("\n=== FPG Incremental Models Comparison ===")
print(fpg_incremental_summary)

a1c_incremental_summary <- incremental_models %>%
  dplyr::filter(outcome == "glycohemoglobin") %>%
  select(model, added_variable, added_coefficient, added_standardized_beta, 
         delta_r2, delta_AIC, delta_BIC, added_p_value) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print("\n=== A1c Incremental Models Comparison ===")
print(a1c_incremental_summary)
