rm(list=ls());gc();source(".Rprofile")

#------------------------------------------------------------------------------------------------------------------------------------------------
### Survey - NHANES ###
#--------------------------------------------------------------------------------------------------------------
years <- c("20112012", "20132014", "20152016", "20172018")

process_nhanes_data <- function(years, path) {
  list_of_data <- list()
  
  for (year in years) {
    file_path <- paste0(path, "/working/cleaned/nhanes_", year, ".rds")
    data <- readRDS(file_path)
    data <- mutate(data, year = paste0(substr(year, 1, 4), "-", substr(year, 5, 8)))
    list_of_data[[year]] <- data
  }
  
  source_df <- bind_rows(list_of_data)
  
  return(source_df)
}

nhanes_data <- process_nhanes_data(years, path_nhanes_ckm_folder) %>% 
  mutate(dm = case_when(dm_doc_told == 1 | glycohemoglobin >= 6.5 | fasting_glucose >= 126 ~ 1,
                        TRUE ~ 0)) 

#-------------------------------------------------------------------------------------------
# Identify all DM (either dm_age is not NA OR (dm_age is NA, HbA1c >= 6.5))
# N = 4036
nhanes_dm_all <- nhanes_data %>% 
  group_by(respondentid) %>% 
  dplyr::filter((!is.na(dm_age) | 
                   is.na(dm_age) & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
  ungroup()

# Identify diagnosed DM, N = 3074
nhanes_dm_diag <- nhanes_dm_all %>% 
  group_by(respondentid) %>%
  dplyr::filter(dm_doc_told == 1) %>%
  ungroup()

# Among diagnosed DM, duration <= 1 year, N = 323
nhanes_dm_newdiag <- nhanes_dm_diag %>% 
  group_by(respondentid) %>% 
  dplyr::filter(!is.na(dm_age) & !is.na(age) & 
                  (age - dm_age) >= 0 & 
                  (age - dm_age) <= 1) %>%
  ungroup()

# Identify undiagnosed DM based on A1c. Set dm_age = current age, N = 966
nhanes_dm_undiag <- nhanes_data %>%
  group_by(respondentid) %>% 
  dplyr::filter((is.na(dm_age) & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
  ungroup() %>% 
  mutate(dm_age = age)


# Exclude all DM from all HRS to get no DM, N = 28620
nhanes_ndm <- nhanes_data %>% 
  dplyr::filter(!respondentid %in% nhanes_dm_all$respondentid) 


# distinct(respondentid) %>%
# nrow()

### NHANES Newly diagnosed (duration <= 1y + undiagnosed) dm: 1289 ###

#-------------------------------------------------------------------------
# Total sample (no T2D + new T2D), N = 29909, obs = 29909
nhanes_total <- bind_rows(nhanes_dm_newdiag,
                          nhanes_dm_undiag,
                          nhanes_ndm) %>% 
  mutate(newdm = case_when(respondentid %in% nhanes_ndm$respondentid ~ 0,
                       TRUE ~ 1))

#--------------------------------------------------------------------------------------------------------------------
source_df <- nhanes_total %>% 
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

# saveRDS(source_df, paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds"))


