rm(list=ls());gc();source(".Rprofile")

nhanes_20112012 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20112012.rds"))
nhanes_20132014 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20132014.rds"))
nhanes_20152016 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20152016.rds"))
nhanes_20172018 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20172018.rds"))


nhanes_total <- bind_rows(
  nhanes_20112012 %>% 
    mutate(year = "2011-2012"),
  nhanes_20132014 %>% 
    mutate(year = "2013-2014"),
  nhanes_20152016 %>% 
    mutate(year = "2015-2016"),
  nhanes_20172018 %>% 
    mutate(year = "2017-2018")) %>% 
  # N= 32,656
  # step 1: non-pregnant adult, N = 22,188
  dplyr::filter(age >= 18) %>% 
  dplyr::filter(pregnant == 2 | pregnant == 3 | is.na(pregnant)) %>% 
  # exclude bmi outliers
  dplyr::filter(bmi>=15 & bmi<=65) %>% 
  # step 2: self-reported for certain: yes/no, N = 21,606
  dplyr::filter(dm_doc_told==1 | dm_doc_told==2) %>% 
  # step 3: complete cases: excluding missing FPG, A1c, N = 9,910
  dplyr::filter(!is.na(fasting_glucose) & !is.na(glycohemoglobin)) %>% 
  # step 4: exclude unavailable DEXA, N = 5,168
  dplyr::filter(!is.na(visceral_fat)) %>%
  dplyr::filter(!is.na(fat_percentage)) %>% 
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
         chol_med_taking = as.factor(chol_med_taking)) %>% 
  # set outliers to NA
  mutate(visceral_fat = ifelse(visceral_fat > 1500, NA, visceral_fat),
         subcutaneous_fat = ifelse(subcutaneous_fat > 4000, NA, subcutaneous_fat))
  

saveRDS(nhanes_total, paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse01_unweighted sample.RDS"))

