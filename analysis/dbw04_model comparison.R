rm(list=ls());gc();source(".Rprofile")

library(survey)
library(pROC)


nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) 

bmi_mod <- svyglm(newdm ~ age + female + race_eth + bmi, design = nhanes_total_svy)
bf_mod <- svyglm(newdm ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
bmibf_mod <- svyglm(newdm ~ age + female + race_eth + bmi + fat_percentage, design = nhanes_total_svy)

test_df <- as.data.frame(nhanes_total_svy) %>% 
  dplyr::filter(!is.na(newdm) & !is.na(bmi) & !is.na(fat_percentage))

# Predict probabilities
test_df$prob_bmi_mod <- predict(bmi_mod, newdata = test_df, type = "response")
test_df$prob_bf_mod <- predict(bf_mod, newdata = test_df, type = "response")
test_df$prob_bmibf_mod <- predict(bmibf_mod, newdata = test_df, type = "response")

# Calculate ROC curves
roc_bmi <- roc(test_df$newdm, test_df$prob_bmi_mod)
roc_bf <- roc(test_df$newdm, test_df$prob_bf_mod)
roc_bmibf <- roc(test_df$newdm, test_df$prob_bmibf_mod)

# Print ROC results (e.g., AUC)
auc_bmi <- auc(roc_bmi)
auc_bf <- auc(roc_bf)
auc_bmibf <- auc(roc_bmibf)

# Comparing ROC curves using DeLong's test
delong_bmi_bf <- roc.test(roc_bmi, roc_bf, method="delong")
delong_bmi_bmibf <- roc.test(roc_bmi, roc_bmibf, method="delong")
delong_bf_bmibf <- roc.test(roc_bf, roc_bmibf, method="delong")

# Printing the results
delong_bmi_bf
delong_bmi_bmibf
delong_bf_bmibf



