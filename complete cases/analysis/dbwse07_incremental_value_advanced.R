rm(list=ls());gc();source(".Rprofile")

library(survey)
library(srvyr)
library(tidyverse)
library(pROC)
library(nricens)
library(dcurves)

cat("\n=== dbwse07: Incremental Value Assessment ===\n")
cat("Loading survey design objects...\n")

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# Use first imputation for model evaluation
nhanes_data <- nhanes_svy_dfs[[1]]$variables
nhanes_svy <- nhanes_svy_dfs[[1]]

# Check required columns
required_cols <- c("dm", "age", "female", "race_eth", "bmi", "fat_percentage", 
                   "visceral_fat", "waistcircumference", "WHtR")
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
# ROC/AUC Analysis: DM vs NoDM
#------------------------------------------------------------------------------------

cat("\n=== ROC/AUC Analysis: DM vs NoDM ===\n")

# Filter for DM analysis
nhanes_dm_svy <- nhanes_svy %>%
  dplyr::filter(dm %in% c("DM", "NoDM")) %>%
  mutate(dm_binary = ifelse(dm == "DM", 1, 0))

dm_data <- nhanes_dm_svy$variables
cat(sprintf("Sample size: N = %d\n", nrow(dm_data)))

# Fit base and incremental models (unweighted for ROC - note: survey-weighted ROC is complex)
cat("Note: ROC analysis uses unweighted data (standard practice)\n")

# Base model: age + sex + race + BMI
model_bmi <- glm(dm_binary ~ age + female + race_eth + z_bmi, 
                 data = dm_data, family = binomial())

# Incremental models
model_bmi_fat <- glm(dm_binary ~ age + female + race_eth + z_bmi + z_fat_percentage, 
                     data = dm_data, family = binomial())

model_bmi_vf <- glm(dm_binary ~ age + female + race_eth + z_bmi + z_visceral_fat, 
                    data = dm_data, family = binomial())

model_bmi_wc <- glm(dm_binary ~ age + female + race_eth + z_bmi + z_waistcircumference, 
                    data = dm_data, family = binomial())

model_bmi_whtr <- glm(dm_binary ~ age + female + race_eth + z_bmi + z_WHtR, 
                      data = dm_data, family = binomial())

# Get predicted probabilities
pred_bmi <- predict(model_bmi, type = "response")
pred_bmi_fat <- predict(model_bmi_fat, type = "response")
pred_bmi_vf <- predict(model_bmi_vf, type = "response")
pred_bmi_wc <- predict(model_bmi_wc, type = "response")
pred_bmi_whtr <- predict(model_bmi_whtr, type = "response")

# Calculate ROC curves
roc_bmi <- roc(dm_data$dm_binary, pred_bmi, quiet = TRUE)
roc_bmi_fat <- roc(dm_data$dm_binary, pred_bmi_fat, quiet = TRUE)
roc_bmi_vf <- roc(dm_data$dm_binary, pred_bmi_vf, quiet = TRUE)
roc_bmi_wc <- roc(dm_data$dm_binary, pred_bmi_wc, quiet = TRUE)
roc_bmi_whtr <- roc(dm_data$dm_binary, pred_bmi_whtr, quiet = TRUE)

# DeLong test for AUC differences
delong_fat <- roc.test(roc_bmi, roc_bmi_fat, method = "delong")
delong_vf <- roc.test(roc_bmi, roc_bmi_vf, method = "delong")
delong_wc <- roc.test(roc_bmi, roc_bmi_wc, method = "delong")
delong_whtr <- roc.test(roc_bmi, roc_bmi_whtr, method = "delong")

# Compile AUC results
auc_dm_results <- data.frame(
  outcome = "DM vs NoDM",
  model_type = c("base", "incremental", "incremental", "incremental", "incremental"),
  added_predictor = c("bmi_only", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR"),
  AUC = c(as.numeric(auc(roc_bmi)), 
          as.numeric(auc(roc_bmi_fat)),
          as.numeric(auc(roc_bmi_vf)),
          as.numeric(auc(roc_bmi_wc)),
          as.numeric(auc(roc_bmi_whtr))),
  AUC_diff = c(NA, 
               as.numeric(auc(roc_bmi_fat)) - as.numeric(auc(roc_bmi)),
               as.numeric(auc(roc_bmi_vf)) - as.numeric(auc(roc_bmi)),
               as.numeric(auc(roc_bmi_wc)) - as.numeric(auc(roc_bmi)),
               as.numeric(auc(roc_bmi_whtr)) - as.numeric(auc(roc_bmi))),
  p_delong = c(NA, 
               delong_fat$p.value,
               delong_vf$p.value,
               delong_wc$p.value,
               delong_whtr$p.value),
  N = nrow(dm_data),
  stringsAsFactors = FALSE
)

#------------------------------------------------------------------------------------
# ROC/AUC Analysis: PreDM vs NoDM
#------------------------------------------------------------------------------------

cat("\n=== ROC/AUC Analysis: PreDM vs NoDM ===\n")

nhanes_predm_svy <- nhanes_svy %>%
  dplyr::filter(dm %in% c("PreDM", "NoDM")) %>%
  mutate(predm_binary = ifelse(dm == "PreDM", 1, 0))

predm_data <- nhanes_predm_svy$variables
cat(sprintf("Sample size: N = %d\n", nrow(predm_data)))

# Fit models
model_predm_bmi <- glm(predm_binary ~ age + female + race_eth + z_bmi, 
                       data = predm_data, family = binomial())

model_predm_bmi_fat <- glm(predm_binary ~ age + female + race_eth + z_bmi + z_fat_percentage, 
                           data = predm_data, family = binomial())

model_predm_bmi_vf <- glm(predm_binary ~ age + female + race_eth + z_bmi + z_visceral_fat, 
                          data = predm_data, family = binomial())

model_predm_bmi_wc <- glm(predm_binary ~ age + female + race_eth + z_bmi + z_waistcircumference, 
                          data = predm_data, family = binomial())

model_predm_bmi_whtr <- glm(predm_binary ~ age + female + race_eth + z_bmi + z_WHtR, 
                            data = predm_data, family = binomial())

# Predictions
pred_predm_bmi <- predict(model_predm_bmi, type = "response")
pred_predm_bmi_fat <- predict(model_predm_bmi_fat, type = "response")
pred_predm_bmi_vf <- predict(model_predm_bmi_vf, type = "response")
pred_predm_bmi_wc <- predict(model_predm_bmi_wc, type = "response")
pred_predm_bmi_whtr <- predict(model_predm_bmi_whtr, type = "response")

# ROC curves
roc_predm_bmi <- roc(predm_data$predm_binary, pred_predm_bmi, quiet = TRUE)
roc_predm_bmi_fat <- roc(predm_data$predm_binary, pred_predm_bmi_fat, quiet = TRUE)
roc_predm_bmi_vf <- roc(predm_data$predm_binary, pred_predm_bmi_vf, quiet = TRUE)
roc_predm_bmi_wc <- roc(predm_data$predm_binary, pred_predm_bmi_wc, quiet = TRUE)
roc_predm_bmi_whtr <- roc(predm_data$predm_binary, pred_predm_bmi_whtr, quiet = TRUE)

# DeLong tests
delong_predm_fat <- roc.test(roc_predm_bmi, roc_predm_bmi_fat, method = "delong")
delong_predm_vf <- roc.test(roc_predm_bmi, roc_predm_bmi_vf, method = "delong")
delong_predm_wc <- roc.test(roc_predm_bmi, roc_predm_bmi_wc, method = "delong")
delong_predm_whtr <- roc.test(roc_predm_bmi, roc_predm_bmi_whtr, method = "delong")

auc_predm_results <- data.frame(
  outcome = "PreDM vs NoDM",
  model_type = c("base", "incremental", "incremental", "incremental", "incremental"),
  added_predictor = c("bmi_only", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR"),
  AUC = c(as.numeric(auc(roc_predm_bmi)), 
          as.numeric(auc(roc_predm_bmi_fat)),
          as.numeric(auc(roc_predm_bmi_vf)),
          as.numeric(auc(roc_predm_bmi_wc)),
          as.numeric(auc(roc_predm_bmi_whtr))),
  AUC_diff = c(NA, 
               as.numeric(auc(roc_predm_bmi_fat)) - as.numeric(auc(roc_predm_bmi)),
               as.numeric(auc(roc_predm_bmi_vf)) - as.numeric(auc(roc_predm_bmi)),
               as.numeric(auc(roc_predm_bmi_wc)) - as.numeric(auc(roc_predm_bmi)),
               as.numeric(auc(roc_predm_bmi_whtr)) - as.numeric(auc(roc_predm_bmi))),
  p_delong = c(NA, 
               delong_predm_fat$p.value,
               delong_predm_vf$p.value,
               delong_predm_wc$p.value,
               delong_predm_whtr$p.value),
  N = nrow(predm_data),
  stringsAsFactors = FALSE
)

#------------------------------------------------------------------------------------
# Calibration metrics (Brier score)
#------------------------------------------------------------------------------------

cat("\n=== Calibration Analysis ===\n")

# Brier score function
calc_brier <- function(observed, predicted) {
  mean((observed - predicted)^2)
}

# DM models
brier_dm <- data.frame(
  outcome = "DM vs NoDM",
  model_type = c("base", "incremental", "incremental", "incremental", "incremental"),
  added_predictor = c("bmi_only", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR"),
  Brier = c(calc_brier(dm_data$dm_binary, pred_bmi),
            calc_brier(dm_data$dm_binary, pred_bmi_fat),
            calc_brier(dm_data$dm_binary, pred_bmi_vf),
            calc_brier(dm_data$dm_binary, pred_bmi_wc),
            calc_brier(dm_data$dm_binary, pred_bmi_whtr)),
  N = nrow(dm_data),
  stringsAsFactors = FALSE
)

# PreDM models
brier_predm <- data.frame(
  outcome = "PreDM vs NoDM",
  model_type = c("base", "incremental", "incremental", "incremental", "incremental"),
  added_predictor = c("bmi_only", "fat_percentage", "visceral_fat", "waistcircumference", "WHtR"),
  Brier = c(calc_brier(predm_data$predm_binary, pred_predm_bmi),
            calc_brier(predm_data$predm_binary, pred_predm_bmi_fat),
            calc_brier(predm_data$predm_binary, pred_predm_bmi_vf),
            calc_brier(predm_data$predm_binary, pred_predm_bmi_wc),
            calc_brier(predm_data$predm_binary, pred_predm_bmi_whtr)),
  N = nrow(predm_data),
  stringsAsFactors = FALSE
)

brier_results <- bind_rows(brier_dm, brier_predm)

#------------------------------------------------------------------------------------
# Save results
#------------------------------------------------------------------------------------

cat("\nSaving results...\n")

# Combine AUC results
all_auc_results <- bind_rows(auc_dm_results, auc_predm_results)

# Save outputs
all_auc_results %>%
  write_csv(., "complete cases/analysis/dbwse07_auc_results.csv")

brier_results %>%
  write_csv(., "complete cases/analysis/dbwse07_calibration_brier.csv")

# Print summaries
cat("\n=== AUC Summary ===\n")
print(all_auc_results %>%
        mutate(across(c(AUC, AUC_diff, p_delong), ~round(., 4))))

cat("\n=== Brier Score Summary ===\n")
print(brier_results %>%
        mutate(Brier = round(Brier, 4)))

cat("\n✓ Incremental value analysis complete!\n")
cat("  Output files:\n")
cat("  - complete cases/analysis/dbwse07_auc_results.csv\n")
cat("  - complete cases/analysis/dbwse07_calibration_brier.csv\n")
