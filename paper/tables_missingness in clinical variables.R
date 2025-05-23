rm(list=ls());gc();source(".Rprofile")

nhanes_raw <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_unweighted sample.RDS"))

# missingness in total--------------------------------------

missing_summary <- nhanes_raw %>%
  select(systolic1, systolic2, systolic3, systolic4,
         diastolic1, diastolic2, diastolic3, diastolic4,
         waistcircumference, fasting_glucose, triglyceride,
         bmi, fat_percentage, glycohemoglobin, ldl, hdl, 
         total_cholesterol, visceral_fat, subcutaneous_fat) %>%
  rename(fatpercentage = fat_percentage, 
         totalcholesterol = total_cholesterol,
         fastingglucose = fasting_glucose,
         visceralfat = visceral_fat,
         subcutaneousfat = subcutaneous_fat) %>% 
  mutate(sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
         dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE)) %>% 
  summarise(across(everything(), list(n_missing = ~sum(is.na(.)),
                                      prop_missing = ~mean(is.na(.)) * 100)))

# Convert the data to a more readable format (long format)
missing_summary_long <- pivot_longer(missing_summary, cols = everything(), 
                                     names_to = c("variable", ".value"), 
                                     names_sep = "_")


# missingness by dm group--------------------------------------

analytic_df <- nhanes_raw %>% 
  mutate(
    # family income-to-poverty ratio (FIPR) (https://www.cdc.gov/nchs/data/series/sr_02/sr02_161.pdf)
    # 0.00–0.99 = Below poverty; 1.00 and above = At or above poverty.  
    fipr = case_when(poverty < 1 ~ "below poverty", 
                     poverty == 1 ~ "at poverty", 
                     poverty > 1 ~ "above poverty"),
    fipr = as.factor(fipr),
    
    insurance = case_when(insured == 1 ~ "Covered by health insurance",
                          insured == 2 ~ "No health insurance",
                          TRUE ~ ""),
    insurance_type = case_when(insured_private == 1 ~ "Privately Insured",
                               insured_medicare == 1 ~ "Insured by Medicare",
                               insured_medicaid == 1 ~ "Insured by Medicaid",
                               TRUE ~ ""),
    sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
    dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE),
    year_broad = case_when(year == '2011-2012' | year == '2013-2014' ~ '2011-2014',
                           TRUE ~ '2015-2018'),
    WHtR = waistcircumference/height,
    
    dm_age = ifelse(dm_age > 100, NA, dm_age)
  ) %>% 
  #   # Refer: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf >> Page 22 onwards
  mutate(nhanes2yweight = mec2yweight/4) 



# Newly diagnosed DM, duration <= 1 year, N = 146
nhanes_dm_newdiag <- analytic_df %>% 
  group_by(respondentid) %>% 
  dplyr::filter(dm_doc_told == 1) %>% 
  dplyr::filter(age - dm_age >= 0 & age - dm_age <= 1) %>%
  ungroup()

# Established diagnosed DM, duration > 1 year, N = 1,235
nhanes_dm_olddiag <- analytic_df %>% 
  group_by(respondentid) %>% 
  dplyr::filter(dm_doc_told == 1) %>% 
  dplyr::filter(age - dm_age > 1) %>%
  ungroup()

# Identify undiagnosed DM based on A1c. Set dm_age = current age, N = 514
nhanes_dm_undiag <- analytic_df %>%
  group_by(respondentid) %>% 
  dplyr::filter(dm_doc_told == 0) %>% 
  dplyr::filter(glycohemoglobin >= 6.5 | fasting_glucose >= 126) %>%
  ungroup() %>% 
  mutate(dm_age = age)

# NoDM, N = 3,682
nhanes_nodm <- analytic_df %>%
  group_by(respondentid) %>% 
  dplyr::filter(dm_doc_told == 0) %>% 
  dplyr::filter(glycohemoglobin < 5.7 & fasting_glucose < 100) %>%
  ungroup() 


# Total sample, N = obs = 9,910
nhanes_total <- analytic_df %>%
  mutate(dm = case_when(
    respondentid %in% nhanes_dm_newdiag$respondentid ~ "DM",
    respondentid %in% nhanes_dm_undiag$respondentid ~ "DM",
    respondentid %in% nhanes_dm_olddiag$respondentid ~ "DM",
    respondentid %in% nhanes_nodm$respondentid ~ "NoDM",
    TRUE ~ "PreDM")
  ) 


missing_summary_grouped <- nhanes_total %>%
  select(dm, waistcircumference, fasting_glucose, triglyceride, sbp, dbp,
         bmi, fat_percentage, glycohemoglobin, ldl, hdl, total_cholesterol,
         visceral_fat, subcutaneous_fat) %>%
  rename(fatpercentage = fat_percentage, 
         totalcholesterol = total_cholesterol,
         fastingglucose = fasting_glucose,
         visceralfat = visceral_fat,
         subcutaneousfat = subcutaneous_fat) %>% 
  group_by(dm) %>%
  summarise(across(waistcircumference:subcutaneousfat, list(n_missing = ~sum(is.na(.)),
                                                     prop_missing = ~mean(is.na(.)) * 100), .names = "{.col}_{.fn}"),
            .groups = 'drop')

# Convert the data to a more readable format (long format)
missing_summary_grouped_long <- pivot_longer(missing_summary_grouped, 
                                             cols = -dm, 
                                             names_to = c("variable", ".value"), 
                                             names_sep = "_")


