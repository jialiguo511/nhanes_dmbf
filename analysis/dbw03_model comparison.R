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



## Correlations----------------------

auc_sbp_bmi <- list()
auc_sbp_bf <- list()
auc_dbp_bmi <- list()
auc_dbp_bf <- list()

auc_hdl_bmi <- list()
auc_hdl_bf <- list()
auc_tcl_bmi <- list()
auc_tcl_bf <- list()

delong_sbp_bmi_bf <- list()
delong_dbp_bmi_bf <- list()

delong_hdl_bmi_bf <- list()
delong_tcl_bmi_bf <- list()

for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]] %>% 
    mutate(sbp = case_when(sbp>=140 ~ 1,
                           TRUE ~ 0),
           dbp = case_when(dbp>=90 ~ 1,
                           TRUE ~ 0),
           hdl = case_when(hdl>=35 ~ 1,
                           TRUE ~ 0),
           total_cholesterol = case_when(total_cholesterol>=200 ~ 1,
                           TRUE ~ 0))
  
  sbp_bmi = svyglm(sbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  sbp_bf = svyglm(sbp ~ age + female + race_eth + fat_percentage + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  
  dbp_bmi = svyglm(dbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  dbp_bf = svyglm(dbp ~ age + female + race_eth + fat_percentage + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  
  hdl_bmi = svyglm(hdl ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  hdl_bf = svyglm(hdl ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  tcl_bmi = svyglm(total_cholesterol ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  tcl_bf = svyglm(total_cholesterol ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  
  test_df <- as.data.frame(nhanes_total_svy) %>% 
    dplyr::filter(!is.na(bmi) & !is.na(fat_percentage))
  
  test_df$prob_sbp_bmi <- predict(sbp_bmi, newdata = test_df, type = "response")
  test_df$prob_sbp_bf <- predict(sbp_bf, newdata = test_df, type = "response")
  test_df$prob_dbp_bmi <- predict(dbp_bmi, newdata = test_df, type = "response")
  test_df$prob_dbp_bf <- predict(dbp_bf, newdata = test_df, type = "response")
  
  test_df$prob_hdl_bmi <- predict(hdl_bmi, newdata = test_df, type = "response")
  test_df$prob_hdl_bf <- predict(hdl_bf, newdata = test_df, type = "response")
  test_df$prob_tcl_bmi <- predict(tcl_bmi, newdata = test_df, type = "response")
  test_df$prob_tcl_bf <- predict(tcl_bf, newdata = test_df, type = "response")
  
  # Calculate ROC curves
  roc_sbp_bmi <- roc(test_df$sbp, test_df$prob_sbp_bmi)
  roc_sbp_bf <- roc(test_df$sbp, test_df$prob_sbp_bf)
  roc_dbp_bmi <- roc(test_df$dbp, test_df$prob_dbp_bmi)
  roc_dbp_bf <- roc(test_df$dbp, test_df$prob_dbp_bf)
  
  roc_hdl_bmi <- roc(test_df$hdl, test_df$prob_hdl_bmi)
  roc_hdl_bf <- roc(test_df$hdl, test_df$prob_hdl_bf)
  roc_tcl_bmi <- roc(test_df$total_cholesterol, test_df$prob_tcl_bmi)
  roc_tcl_bf <- roc(test_df$total_cholesterol, test_df$prob_tcl_bf)
  
  
  # Print ROC results (e.g., AUC)
  auc_sbp_bmi[[i]] <- auc(roc_sbp_bmi)
  auc_sbp_bf[[i]] <- auc(roc_sbp_bf)
  auc_dbp_bmi[[i]] <- auc(roc_dbp_bmi)
  auc_dbp_bf[[i]] <- auc(roc_dbp_bf)
  
  auc_hdl_bmi[[i]] <- auc(roc_hdl_bmi)
  auc_hdl_bf[[i]] <- auc(roc_hdl_bf)
  auc_tcl_bmi[[i]] <- auc(roc_tcl_bmi)
  auc_tcl_bf[[i]] <- auc(roc_tcl_bf)
  
  delong_sbp_bmi_bf[[i]] <- roc.test(roc_sbp_bmi, roc_sbp_bf, method="delong")
  delong_dbp_bmi_bf[[i]] <- roc.test(roc_dbp_bmi, roc_dbp_bf, method="delong")
  
  delong_hdl_bmi_bf[[i]] <- roc.test(roc_hdl_bmi, roc_hdl_bf, method="delong")
  delong_tcl_bmi_bf[[i]] <- roc.test(roc_tcl_bmi, roc_tcl_bf, method="delong")
  
  
}

auc_sbp_bmi <- mean(unlist(auc_sbp_bmi))
auc_sbp_bf <- mean(unlist(auc_sbp_bf))
auc_dbp_bmi <- mean(unlist(auc_dbp_bmi))
auc_dbp_bf <- mean(unlist(auc_dbp_bf))

auc_hdl_bmi <- mean(unlist(auc_hdl_bmi))
auc_hdl_bf <- mean(unlist(auc_hdl_bf))
auc_tcl_bmi <- mean(unlist(auc_tcl_bmi))
auc_tcl_bf <- mean(unlist(auc_tcl_bf))

delong_sbp_bmi_bf[[1]]
delong_dbp_bmi_bf[[2]]
delong_hdl_bmi_bf[[1]]
delong_tcl_bmi_bf[[1]]
