rm(list=ls());gc();source(".Rprofile")

library(survey)
library(mice)

nhanes_20112012 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20112012.rds"))
nhanes_20132014 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20132014.rds"))
nhanes_20152016 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20152016.rds"))
nhanes_20172018 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20172018.rds"))


nhanes_20112018 <- bind_rows(
  nhanes_20112012 %>% 
    mutate(year = "2011-2012"),
  nhanes_20132014 %>% 
    mutate(year = "2013-2014"),
  nhanes_20152016 %>% 
    mutate(year = "2015-2016"),
  nhanes_20172018 %>% 
    mutate(year = "2017-2018")) %>% 
  # step 1: non-pregnant adult
  dplyr::filter(age >= 18) %>% 
  dplyr::filter(pregnant == 2 | pregnant == 3 | is.na(pregnant)) %>% 
  # step 2: exclude bmi outliers
  dplyr::filter(bmi>=15 & bmi<=65) %>% 
  # step 3: self-reported for certain: yes/no
  dplyr::filter(dm_doc_told==1 | dm_doc_told==2) %>% 
  # step 4: complete cases: excluding missing FPG, A1c
  dplyr::filter(!is.na(fasting_glucose) & !is.na(glycohemoglobin)) %>% 
  dplyr::select(-c(citizenship, hhincome, pregnant)) %>% 
  mutate(insured_private = case_when(insured_private == 14 ~ 1,
                                     TRUE ~ 0),
         insured_medicare = case_when(insured_medicare == 15 ~ 1,
                                      TRUE ~ 0),
         insured_medicaid = case_when(insured_medicaid == 17 ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(female = case_when(gender == 2 ~ 1,
                            TRUE ~ 0),
         race_eth = case_when(race == 3 ~ "NH White",
                              race == 4 ~ "NH Black",
                              race == 6 ~ "Asian",
                              race == 1 | race == 2 ~ "Hispanic",
                              race == 7 ~ "Other Race",
                              TRUE ~ NA_character_),
         education = case_when(education == 1 | education == 2 ~ "Less than high school", 
                               education == 3 ~ "High school only", 
                               education == 4 | education == 5 ~ "Any college and above",
                               education == 7 | education == 9 ~ "Refused or don't know",
                               TRUE ~ NA_character_),
         immigrant = case_when(immigrant == 1 ~ "Born in 50 US states or Washington, DC",
                               immigrant == 2 ~ "Other",
                               immigrant == 77 | immigrant == 99  ~ "Refused or don't know",
                               TRUE ~ NA_character_),
         marital = case_when(marital == 1 ~ "married", 
                             marital == 5 | marital == 6 ~"never married",
                             is.na(marital) ~ NA_character_,
                             TRUE ~ "previously married"),
         dm_doc_told = case_when(dm_doc_told == 1 ~ 1,
                                 dm_doc_told == 2 ~ 0,
                                 TRUE ~ NA_real_),
         dm_insulin_taking = case_when(dm_insulin_taking == 1 ~ 1,
                                       dm_insulin_taking == 2 ~ 0,
                                       TRUE ~ NA_real_),
         htn_med_told = case_when(htn_med_told == 1 ~ 1,
                                  htn_med_told == 2 ~ 0,
                                  TRUE ~ NA_real_),
         htn_med_taking = case_when(htn_med_taking == 1 ~ 1,
                                    htn_med_taking == 2 ~ 0,
                                    TRUE ~ NA_real_),
         chol_med_told = case_when(chol_med_told == 1 ~ 1,
                                   chol_med_told == 2 ~ 0,
                                   TRUE ~ NA_real_),
         chol_med_taking = case_when(chol_med_taking == 1 ~ 1,
                                     chol_med_taking == 2 ~ 0,
                                     TRUE ~ NA_real_)
         ) %>% 
  
  mutate(immigrant = as.factor(immigrant),
         education = as.factor(education),
         marital = as.factor(marital),
         dm_doc_told = as.factor(dm_doc_told),
         dm_insulin_taking = as.factor(dm_insulin_taking),
         htn_med_told = as.factor(htn_med_told),
         htn_med_taking = as.factor(htn_med_taking),
         chol_med_told = as.factor(chol_med_told),
         chol_med_taking = as.factor(chol_med_taking)) 

colnames(nhanes_20112018)


continuous_vars <- c("urine_albumin", "urine_creatinine", "alt", "ast", "bun",
                     "cpk", "serum_glucose", "ldh", "uric_acid", "serum_creatinine", "weight", 
                     "height", "bmi", "waistcircumference", "htn_age", "systolic1", "systolic2", 
                     "systolic3", "systolic4", "diastolic1", "diastolic2", "diastolic3",
                     "diastolic4", "poverty", "a1c", "fasting_length_hr", 
                     "glycohemoglobin", "fasting_glucose", "hdl", "insulin_level", "glucose2h",
                     "ogtt_time", "total_cholesterol", "ldl", "triglyceride", 
                     "fat_percentage", "total_fat", "total_lean", "eGFR")

proportion_vars <- c("dm_insulin_taking","htn_med_told","htn_med_taking","chol_med_told",
                     "chol_med_taking")

grouped_vars <- c("immigrant", "education", "marital")

id_vars <- c("respondentid", "psu", "pseudostratum", "intweight", "mec2yweight", "dm_age", "year",
             "insured", "insured_private", "insured_medicare", "insured_medicaid", "dm_family_history",
             "dm_doc_told",
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

