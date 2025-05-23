rm(list=ls());gc();source(".Rprofile")

nhanes_20112018 <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_unweighted sample.RDS"))

colnames(nhanes_20112018)

library(mice)

continuous_vars <- c("urine_albumin", "urine_creatinine", "alt", "ast", "bun",
                     "cpk", "serum_glucose", "ldh", "uric_acid", "serum_creatinine", "weight", 
                     "height", "bmi", "waistcircumference", "htn_age", "systolic1", "systolic2", 
                     "systolic3", "systolic4", "diastolic1", "diastolic2", "diastolic3",
                     "diastolic4", "poverty", "a1c", "fasting_length_hr", 
                      "hdl", "insulin_level", "glucose2h",
                     "ogtt_time", "total_cholesterol", "ldl", "triglyceride", 
                     "fat_percentage", "total_fat", "total_lean", "eGFR")

proportion_vars <- c("dm_insulin_taking","htn_med_told","htn_med_taking","chol_med_told",
                     "chol_med_taking")

grouped_vars <- c("immigrant", "education", "marital")

id_vars <- c("respondentid", "psu", "pseudostratum", "intweight", "mec2yweight", "dm_age", "year",
             "insured", "insured_private", "insured_medicare", "insured_medicaid", "dm_family_history",
             "dm_doc_told", "glycohemoglobin", "fasting_glucose",
             # no missing values
             "race_eth", "race", "gender", "female", "age")



before_imputation <- nhanes_20112018  %>% 
  dplyr::select(
    any_of(id_vars),
    any_of(continuous_vars),
    any_of(proportion_vars),
    any_of(grouped_vars)
  )

mi_null <- mice(before_imputation, maxit = 0)

method = mi_null$method
# method[method == "pmm"] <- "rf" # Takes too long
method[proportion_vars] <- "logreg"
method[grouped_vars] <- "polyreg"
method[id_vars] <- ""

pred = mi_null$predictorMatrix

# Corrected below --------
pred[id_vars,] <- 0
pred[,id_vars] <- 0


mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_nhanes_dmbf_folder, "/working/cleaned/mi_dfs.RDS"))

