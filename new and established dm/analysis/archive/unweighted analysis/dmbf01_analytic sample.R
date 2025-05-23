rm(list=ls());gc();source(".Rprofile")

nhanes_20112012 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20112012.rds"))
nhanes_20132014 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20132014.rds"))
nhanes_20152016 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20152016.rds"))
nhanes_20172018 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20172018.rds"))


source_df <- bind_rows(
  nhanes_20112012 %>% 
    mutate(year = "2011-2012"),
  nhanes_20132014 %>% 
    mutate(year = "2013-2014"),
  nhanes_20152016 %>% 
    mutate(year = "2015-2016"),
  nhanes_20172018 %>% 
    mutate(year = "2017-2018")) %>% 
  dplyr::filter(age >= 18) %>% 
  mutate(dm = case_when(dm_doc_told == 1 | glycohemoglobin >= 6.5 | fasting_glucose >= 126 ~ 1,
                        TRUE ~ 0)) %>% 
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            TRUE ~ ""),
         race_eth = case_when(race == 3 ~ "NH White",
                              race == 4 ~ "NH Black",
                              race == 6 ~ "Asian",
                              race == 1 | race == 2 ~ "Hispanic",
                          TRUE ~ "Other Race"),
         education = case_when(education == 1 | education == 2 ~ "Less than high school", 
                               education == 3 ~ "High school only", 
                               education == 4 |  education == 5 ~ "Any college and above",
                               TRUE ~ ""),
         # family income-to-poverty ratio (FIPR) (https://www.cdc.gov/nchs/data/series/sr_02/sr02_161.pdf)
         # 0.00–0.99 = Below poverty; 1.00 and above = At or above poverty.  
         fipr = case_when(poverty < 1 ~ "below poverty", 
                          poverty == 1 ~ "at poverty", 
                          poverty > 1 ~ "above poverty"),
         marital = case_when(marital == 1 ~ "married", 
                             marital == 5 | marital == 6 ~"never married",
                             is.na(marital) ~ "",
                             TRUE ~ "previously married"),
         immigrant = case_when(immigrant == 1 ~ "Born in 50 US states or Washington, DC",
                               immigrant == 2 ~ "Other",
                               TRUE ~ ""),
         insurance = case_when(insured == 1 ~ "Covered by health insurance",
                               insured == 2 ~ "No health insurance",
                               TRUE ~ ""),
         insurance_type = case_when(insured_private == 1 ~ "Privately Insured",
                                    insured_medicare == 1 ~ "Insured by Medicare",
                                    insured_medicaid == 1 ~ "Insured by Medicaid",
                                    TRUE ~ ""),
         bmi_category = case_when(bmi<18.5 ~ "<18.5",
                                  bmi>=18.5 & bmi<25 ~ "18.5-24.9",
                                  bmi>=25 & bmi<30 ~ "25-29.9",
                                  bmi>=30 & bmi<40 ~ "30-39.9",
                                  TRUE ~ ">= 40"),
         sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
         dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE)) %>% 
  dplyr::filter(pregnant == 2 | pregnant == 3 | is.na(pregnant)) %>% 
  dplyr::select(-c(citizenship, race, poverty, hhincome, pregnant))

saveRDS(source_df, paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds"))

