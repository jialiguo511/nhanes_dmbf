rm(list=ls());gc();source(".Rprofile")

library(survey)
library(pROC)
nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS")) 


for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]
  
  bmi_mod <- svyglm(newdm ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  bf_mod <- svyglm(newdm ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  bmibf_mod <- svyglm(newdm ~ age + female + race_eth + bmi + fat_percentage, design = nhanes_total_svy)
  
  test_df <- as.data.frame(nhanes_total_svy) %>% 
    dplyr::filter(!is.na(newdm) & !is.na(bmi) & !is.na(fat_percentage))
  
  test_df$prob_bmi_mod <- predict(bmi_mod, newdata = test_df, type = "response")
  test_df$prob_bf_mod <- predict(bf_mod, newdata = test_df, type = "response")
  test_df$prob_bmibf_mod <- predict(bmibf_mod, newdata = test_df, type = "response")
  
  # Calculate ROC curves
  roc_bmi <- roc(test_df$newdm, test_df$prob_bmi_mod)
  roc_bf <- roc(test_df$newdm, test_df$prob_bf_mod)
  roc_bmibf <- roc(test_df$newdm, test_df$prob_bmibf_mod)
  
  # Print ROC results (e.g., AUC)
  auc_bmi[[i]] <- auc(roc_bmi)
  auc_bf[[i]] <- auc(roc_bf)
  auc_bmibf[[i]] <- auc(roc_bmibf)
  
  delong_bmi_bf[[i]] <- roc.test(roc_bmi, roc_bf, method="delong")
  delong_bmi_bmibf[[i]] <- roc.test(roc_bmi, roc_bmibf, method="delong")
  delong_bf_bmibf[[i]] <- roc.test(roc_bf, roc_bmibf, method="delong")
  
  
}

auc_bmi <- mean(unlist(auc_bmi))
auc_bf <- mean(unlist(auc_bf))
auc_bmibf <- mean(unlist(auc_bmibf))

delong_bmi_bf[[1]]
delong_bmi_bmibf[[1]]
delong_bf_bmibf[[1]]
