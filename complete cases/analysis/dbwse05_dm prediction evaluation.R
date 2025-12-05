rm(list=ls());gc();source(".Rprofile")

library(survey)
library(tidyverse)
library(pROC)
library(broom)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# Use first imputation for model evaluation
nhanes_data <- nhanes_svy_dfs[[1]]$variables

# Create survey design
nhanes_svy <- nhanes_svy_dfs[[1]]

#------------------------------------------------------------------------------------
# Outcome 1: dm (1/0) - Remove preDM from sample
#------------------------------------------------------------------------------------

nhanes_dm <- nhanes_data %>%
  dplyr::filter(dm != "PreDM") %>%
  mutate(dm_binary = ifelse(dm == "DM", 1, 0))

# Create survey design for dm analysis
nhanes_dm_svy <- nhanes_dm %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG")

#------------------------------------------------------------------------------------
# 2.1 Single Variable Logistic Models for dm
#------------------------------------------------------------------------------------

# M0: Base model
m0_dm <- svyglm(dm_binary ~ age + female + race_eth, 
                   design = nhanes_dm_svy, family = binomial())

# M_BMI
m_bmi_dm <- svyglm(dm_binary ~ age + female + race_eth + bmi, 
                      design = nhanes_dm_svy, family = binomial())

# M_Fat%
m_fat_dm <- svyglm(dm_binary ~ age + female + race_eth + fat_percentage, 
                      design = nhanes_dm_svy, family = binomial())

# M_VF
m_vf_dm <- svyglm(dm_binary ~ age + female + race_eth + visceral_fat, 
                     design = nhanes_dm_svy, family = binomial())

# M_WHtR
m_whtr_dm <- svyglm(dm_binary ~ age + female + race_eth + WHtR, 
                       design = nhanes_dm_svy, family = binomial())

# Function to calculate AUC and 95% CI for survey designs
calculate_auc <- function(model, data, outcome) {
  # Get predicted probabilities
  pred_probs <- predict(model, newdata = data, type = "response")
  
  # Calculate AUC
  roc_obj <- roc(data[[outcome]], pred_probs, quiet = TRUE)
  auc_val <- as.numeric(roc_obj$auc)
  
  # Get 95% CI using bootstrap (1000 resamples)
  ci_val <- ci.auc(roc_obj, method = "bootstrap", boot.n = 1000, quiet = TRUE)
  
  return(list(
    auc = auc_val,
    auc_lower = as.numeric(ci_val[1]),
    auc_upper = as.numeric(ci_val[3]),
    ci_string = paste0(round(auc_val, 3), " (", round(ci_val[1], 3), "-", round(ci_val[3], 3), ")")
  ))
}

# Calculate AUC for single variable models - dm
auc_m0_dm <- calculate_auc(m0_dm, nhanes_dm, "dm_binary")
auc_m_bmi_dm <- calculate_auc(m_bmi_dm, nhanes_dm, "dm_binary")
auc_m_fat_dm <- calculate_auc(m_fat_dm, nhanes_dm, "dm_binary")
auc_m_vf_dm <- calculate_auc(m_vf_dm, nhanes_dm, "dm_binary")
auc_m_whtr_dm <- calculate_auc(m_whtr_dm, nhanes_dm, "dm_binary")

# Create single variable model comparison table - dm
single_var_auc_dm <- tibble::tibble(
  outcome = "dm",
  model = c("M0: Base", "M_BMI", "M_Fat%", "M_VF", "M_WHtR"),
  body_comp_variable = c(NA, "bmi", "fat_percentage", "visceral_fat", "WHtR"),
  AUC = c(auc_m0_dm$auc, auc_m_bmi_dm$auc, auc_m_fat_dm$auc, 
          auc_m_vf_dm$auc, auc_m_whtr_dm$auc),
  AUC_95CI_lower = c(auc_m0_dm$auc_lower, auc_m_bmi_dm$auc_lower, auc_m_fat_dm$auc_lower,
                     auc_m_vf_dm$auc_lower, auc_m_whtr_dm$auc_lower),
  AUC_95CI_upper = c(auc_m0_dm$auc_upper, auc_m_bmi_dm$auc_upper, auc_m_fat_dm$auc_upper,
                     auc_m_vf_dm$auc_upper, auc_m_whtr_dm$auc_upper),
  AUC_95CI_string = c(auc_m0_dm$ci_string, auc_m_bmi_dm$ci_string, auc_m_fat_dm$ci_string,
                      auc_m_vf_dm$ci_string, auc_m_whtr_dm$ci_string),
  n = nrow(nhanes_dm)
)


#------------------------------------------------------------------------------------
# 2.2 Incremental Value Models for dm (BMI + others)
#------------------------------------------------------------------------------------

# M_BMI_Fat
m_bmi_fat_dm <- svyglm(dm_binary ~ age + female + race_eth + bmi + fat_percentage, 
                          design = nhanes_dm_svy, family = binomial())

# M_BMI_VF
m_bmi_vf_dm <- svyglm(dm_binary ~ age + female + race_eth + bmi + visceral_fat, 
                         design = nhanes_dm_svy, family = binomial())

# M_BMI_WHtR
m_bmi_whtr_dm <- svyglm(dm_binary ~ age + female + race_eth + bmi + WHtR, 
                           design = nhanes_dm_svy, family = binomial())

# Calculate AUC for incremental models
auc_m_bmi_fat_dm <- calculate_auc(m_bmi_fat_dm, nhanes_dm, "dm_binary")
auc_m_bmi_vf_dm <- calculate_auc(m_bmi_vf_dm, nhanes_dm, "dm_binary")
auc_m_bmi_whtr_dm <- calculate_auc(m_bmi_whtr_dm, nhanes_dm, "dm_binary")

# DeLong test for AUC comparison
delong_test_dm <- function(model1, model2, data, outcome) {
  pred1 <- predict(model1, newdata = data, type = "response")
  pred2 <- predict(model2, newdata = data, type = "response")
  
  roc1 <- roc(data[[outcome]], pred1, quiet = TRUE)
  roc2 <- roc(data[[outcome]], pred2, quiet = TRUE)
  
  # Perform DeLong test
  test_result <- roc.test(roc1, roc2, method = "delong")
  
  return(list(
    p_value = test_result$p.value,
    auc1 = as.numeric(roc1$auc),
    auc2 = as.numeric(roc2$auc),
    auc_diff = as.numeric(roc2$auc) - as.numeric(roc1$auc)
  ))
}

delong_bmi_vs_bmi_fat_dm <- delong_test_dm(m_bmi_dm, m_bmi_fat_dm, nhanes_dm, "dm_binary")
delong_bmi_vs_bmi_vf_dm <- delong_test_dm(m_bmi_dm, m_bmi_vf_dm, nhanes_dm, "dm_binary")
delong_bmi_vs_bmi_whtr_dm <- delong_test_dm(m_bmi_dm, m_bmi_whtr_dm, nhanes_dm, "dm_binary")

# LR test for nested models
lr_test <- function(model_reduced, model_full) {
  # Get deviances
  dev_reduced <- model_reduced$deviance
  dev_full <- model_full$deviance
  
  # LR statistic
  lr_stat <- dev_reduced - dev_full
  
  # Degrees of freedom (difference in number of parameters)
  df <- length(coef(model_full)) - length(coef(model_reduced))
  
  # P-value
  p_value <- 1 - pchisq(lr_stat, df)
  
  return(list(
    lr_stat = lr_stat,
    df = df,
    p_value = p_value
  ))
}

lr_bmi_vs_bmi_fat_dm <- lr_test(m_bmi_dm, m_bmi_fat_dm)
lr_bmi_vs_bmi_vf_dm <- lr_test(m_bmi_dm, m_bmi_vf_dm)
lr_bmi_vs_bmi_whtr_dm <- lr_test(m_bmi_dm, m_bmi_whtr_dm)

# Create incremental model comparison table - dm
incremental_auc_dm <- tibble::tibble(
  outcome = "dm",
  model_pair = c("BMI vs BMI+Fat%", "BMI vs BMI+VF", "BMI vs BMI+WHtR"),
  added_variable = c("fat_percentage", "visceral_fat", "WHtR"),
  AUC_BMI = c(auc_m_bmi_dm$auc, auc_m_bmi_dm$auc, auc_m_bmi_dm$auc),
  AUC_BMI_plus = c(auc_m_bmi_fat_dm$auc, auc_m_bmi_vf_dm$auc, auc_m_bmi_whtr_dm$auc),
  delta_AUC = c(delong_bmi_vs_bmi_fat_dm$auc_diff, delong_bmi_vs_bmi_vf_dm$auc_diff, 
                delong_bmi_vs_bmi_whtr_dm$auc_diff),
  delong_p_value = c(delong_bmi_vs_bmi_fat_dm$p_value, delong_bmi_vs_bmi_vf_dm$p_value,
                     delong_bmi_vs_bmi_whtr_dm$p_value),
  lr_p_value = c(lr_bmi_vs_bmi_fat_dm$p_value, lr_bmi_vs_bmi_vf_dm$p_value,
                 lr_bmi_vs_bmi_whtr_dm$p_value),
  n = nrow(nhanes_dm)
)


#------------------------------------------------------------------------------------
# Outcome 2: preDM (1/0) - Remove newdm from sample
#------------------------------------------------------------------------------------

nhanes_predm <- nhanes_data %>%
  dplyr::filter(dm != "DM") %>%
  mutate(predm_binary = ifelse(dm == "PreDM", 1, 0))

# Create survey design for preDM analysis
nhanes_predm_svy <- nhanes_predm %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG")

#------------------------------------------------------------------------------------
# 2.1 Single Variable Logistic Models for preDM
#------------------------------------------------------------------------------------

# M0: Base model
m0_predm <- svyglm(predm_binary ~ age + female + race_eth, 
                   design = nhanes_predm_svy, family = binomial())

# M_BMI
m_bmi_predm <- svyglm(predm_binary ~ age + female + race_eth + bmi, 
                      design = nhanes_predm_svy, family = binomial())

# M_Fat%
m_fat_predm <- svyglm(predm_binary ~ age + female + race_eth + fat_percentage, 
                      design = nhanes_predm_svy, family = binomial())

# M_VF
m_vf_predm <- svyglm(predm_binary ~ age + female + race_eth + visceral_fat, 
                     design = nhanes_predm_svy, family = binomial())

# M_WHtR
m_whtr_predm <- svyglm(predm_binary ~ age + female + race_eth + WHtR, 
                       design = nhanes_predm_svy, family = binomial())

# Calculate AUC for single variable models - preDM
auc_m0_predm <- calculate_auc(m0_predm, nhanes_predm, "predm_binary")
auc_m_bmi_predm <- calculate_auc(m_bmi_predm, nhanes_predm, "predm_binary")
auc_m_fat_predm <- calculate_auc(m_fat_predm, nhanes_predm, "predm_binary")
auc_m_vf_predm <- calculate_auc(m_vf_predm, nhanes_predm, "predm_binary")
auc_m_whtr_predm <- calculate_auc(m_whtr_predm, nhanes_predm, "predm_binary")

# Create single variable model comparison table - preDM
single_var_auc_predm <- tibble::tibble(
  outcome = "preDM",
  model = c("M0: Base", "M_BMI", "M_Fat%", "M_VF", "M_WHtR"),
  body_comp_variable = c(NA, "bmi", "fat_percentage", "visceral_fat", "WHtR"),
  AUC = c(auc_m0_predm$auc, auc_m_bmi_predm$auc, auc_m_fat_predm$auc, 
          auc_m_vf_predm$auc, auc_m_whtr_predm$auc),
  AUC_95CI_lower = c(auc_m0_predm$auc_lower, auc_m_bmi_predm$auc_lower, auc_m_fat_predm$auc_lower,
                     auc_m_vf_predm$auc_lower, auc_m_whtr_predm$auc_lower),
  AUC_95CI_upper = c(auc_m0_predm$auc_upper, auc_m_bmi_predm$auc_upper, auc_m_fat_predm$auc_upper,
                     auc_m_vf_predm$auc_upper, auc_m_whtr_predm$auc_upper),
  AUC_95CI_string = c(auc_m0_predm$ci_string, auc_m_bmi_predm$ci_string, auc_m_fat_predm$ci_string,
                      auc_m_vf_predm$ci_string, auc_m_whtr_predm$ci_string),
  n = nrow(nhanes_predm)
)

#------------------------------------------------------------------------------------
# 2.2 Incremental Value Models for preDM (BMI + others)
#------------------------------------------------------------------------------------

# M_BMI_Fat
m_bmi_fat_predm <- svyglm(predm_binary ~ age + female + race_eth + bmi + fat_percentage, 
                          design = nhanes_predm_svy, family = binomial())

# M_BMI_VF
m_bmi_vf_predm <- svyglm(predm_binary ~ age + female + race_eth + bmi + visceral_fat, 
                         design = nhanes_predm_svy, family = binomial())

# M_BMI_WHtR
m_bmi_whtr_predm <- svyglm(predm_binary ~ age + female + race_eth + bmi + WHtR, 
                           design = nhanes_predm_svy, family = binomial())

# Calculate AUC for incremental models
auc_m_bmi_fat_predm <- calculate_auc(m_bmi_fat_predm, nhanes_predm, "predm_binary")
auc_m_bmi_vf_predm <- calculate_auc(m_bmi_vf_predm, nhanes_predm, "predm_binary")
auc_m_bmi_whtr_predm <- calculate_auc(m_bmi_whtr_predm, nhanes_predm, "predm_binary")

# DeLong test for AUC comparison
delong_bmi_vs_bmi_fat_predm <- delong_test_dm(m_bmi_predm, m_bmi_fat_predm, nhanes_predm, "predm_binary")
delong_bmi_vs_bmi_vf_predm <- delong_test_dm(m_bmi_predm, m_bmi_vf_predm, nhanes_predm, "predm_binary")
delong_bmi_vs_bmi_whtr_predm <- delong_test_dm(m_bmi_predm, m_bmi_whtr_predm, nhanes_predm, "predm_binary")

# LR test for nested models
lr_bmi_vs_bmi_fat_predm <- lr_test(m_bmi_predm, m_bmi_fat_predm)
lr_bmi_vs_bmi_vf_predm <- lr_test(m_bmi_predm, m_bmi_vf_predm)
lr_bmi_vs_bmi_whtr_predm <- lr_test(m_bmi_predm, m_bmi_whtr_predm)

# Create incremental model comparison table - preDM
incremental_auc_predm <- tibble::tibble(
  outcome = "preDM",
  model_pair = c("BMI vs BMI+Fat%", "BMI vs BMI+VF", "BMI vs BMI+WHtR"),
  added_variable = c("fat_percentage", "visceral_fat", "WHtR"),
  AUC_BMI = c(auc_m_bmi_predm$auc, auc_m_bmi_predm$auc, auc_m_bmi_predm$auc),
  AUC_BMI_plus = c(auc_m_bmi_fat_predm$auc, auc_m_bmi_vf_predm$auc, auc_m_bmi_whtr_predm$auc),
  delta_AUC = c(delong_bmi_vs_bmi_fat_predm$auc_diff, delong_bmi_vs_bmi_vf_predm$auc_diff, 
                delong_bmi_vs_bmi_whtr_predm$auc_diff),
  delong_p_value = c(delong_bmi_vs_bmi_fat_predm$p_value, delong_bmi_vs_bmi_vf_predm$p_value,
                     delong_bmi_vs_bmi_whtr_predm$p_value),
  lr_p_value = c(lr_bmi_vs_bmi_fat_predm$p_value, lr_bmi_vs_bmi_vf_predm$p_value,
                 lr_bmi_vs_bmi_whtr_predm$p_value),
  n = nrow(nhanes_predm)
)


#------------------------------------------------------------------------------------
# Summary tables for manuscript
#------------------------------------------------------------------------------------

# Combined single variable AUC comparison
all_single_var_auc <- bind_rows(single_var_auc_dm, single_var_auc_predm) %>%
  select(outcome, model, body_comp_variable, AUC_95CI_string, n)

# Combined incremental AUC comparison
all_incremental_auc <- bind_rows(incremental_auc_dm, incremental_auc_predm) %>%
  select(outcome, model_pair, delta_AUC, delong_p_value, lr_p_value, n)


all_single_var_auc %>%
  write_csv(., "complete cases/analysis/dbwse05_single variable auc.csv")

all_incremental_auc %>%
  write_csv(., "complete cases/analysis/dbwse05_incremental auc.csv")
