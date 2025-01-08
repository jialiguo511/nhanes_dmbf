rm(list=ls());gc();source(".Rprofile")

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
  dplyr::filter(age >= 18) %>% 
  # mutate(dm = case_when(dm_doc_told == 1 | glycohemoglobin >= 6.5 | fasting_glucose >= 126 ~ "diabetes",
  #                       TRUE ~ "non-diabetes")) %>% 
  mutate(female = case_when(gender == 2 ~ 1,
                            TRUE ~ 0),
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
         sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
         dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE),
         year_broad = case_when(year == '2011-2012' | year == '2013-2014' ~ '2011-2014',
                                TRUE ~ '2015-2018')) %>% 
  dplyr::filter(pregnant == 2 | pregnant == 3 | is.na(pregnant)) %>% 
  dplyr::select(-c(citizenship, poverty, hhincome, pregnant)) %>% 
  mutate(nhanes2yweight = mec2yweight/4) 

#   # Refer: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf >> Page 22 onwards
# check weighted total sample size N
sum(nhanes_20112018$nhanes2yweight)
#-------------------------------------------------------------------------------------------
# Identify all DM (either dm_age is not NA OR (dm_age is NA, HbA1c >= 6.5))
# N = 3981
nhanes_dm_all <- nhanes_20112018 %>% 
  group_by(respondentid) %>% 
  dplyr::filter((!is.na(dm_age) | 
                   is.na(dm_age) & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
  ungroup()

# Identify diagnosed DM, N = 3032
nhanes_dm_diag <- nhanes_dm_all %>% 
  group_by(respondentid) %>%
  dplyr::filter(dm_doc_told == 1) %>%
  ungroup()

# 4 ppl with diagnosed dm but missing dm_age --> diagnosis >1y
sum(is.na(nhanes_dm_diag$dm_age))

# Among diagnosed DM, duration <= 1 year, N = 310
nhanes_dm_newdiag <- nhanes_dm_diag %>% 
  group_by(respondentid) %>% 
  dplyr::filter(!is.na(dm_age) & !is.na(age) & 
                  (age - dm_age) >= 0 & 
                  (age - dm_age) <= 1) %>%
  ungroup()

# Identify undiagnosed DM based on A1c. Set dm_age = current age, N = 949
nhanes_dm_undiag <- nhanes_20112018 %>%
  group_by(respondentid) %>% 
  dplyr::filter((is.na(dm_age) & dm_doc_told != 1 & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
  ungroup() %>% 
  mutate(dm_age = age)


# Exclude all DM from all HRS to get no DM, N = 18579
nhanes_ndm <- nhanes_20112018 %>% 
  dplyr::filter(!respondentid %in% nhanes_dm_all$respondentid) 


# distinct(respondentid) %>%
# nrow()

### NHANES Newly diagnosed (duration <= 1y + undiagnosed) dm: 1289 ###

#-------------------------------------------------------------------------
# Total sample (no T2D + new T2D), N = obs = 19842
nhanes_total <- nhanes_20112018 %>%
  mutate(dm = case_when(
    respondentid %in% nhanes_ndm$respondentid ~ "non-diabetes",
    respondentid %in% nhanes_dm_newdiag$respondentid ~ "newly and undiagnosed diabetes",
    respondentid %in% nhanes_dm_undiag$respondentid ~ "newly and undiagnosed diabetes",
    TRUE ~ "diagnosed diabetes >1y"),
    newdm = case_when(
      dm == "non-diabetes" ~ 0,
      dm == "newly and undiagnosed diabetes" ~ 1,
      TRUE ~ NA_integer_
    ))

# Define the survey design, ensuring all variables needed are already in nhanes_total
nhanes_total_svy <- nhanes_total %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG")

saveRDS(nhanes_total_svy, paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS"))
