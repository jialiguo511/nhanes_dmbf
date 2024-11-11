rm(list=ls());gc();source(".Rprofile")

library(survey)

source("analysis/dbw01_weighted sample.R")
#-------------------------------------------------------------------------------------------
# Identify all DM (either dm_age is not NA OR (dm_age is NA, HbA1c >= 6.5))
# N = 4036
nhanes_dm_all <- nhanes_20112018 %>% 
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
nhanes_dm_undiag <- nhanes_20112018 %>%
  group_by(respondentid) %>% 
  dplyr::filter((is.na(dm_age) & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
  ungroup() %>% 
  mutate(dm_age = age)


# Exclude all DM from all HRS to get no DM, N = 28620
nhanes_ndm <- nhanes_20112018 %>% 
  dplyr::filter(!respondentid %in% nhanes_dm_all$respondentid) 


# distinct(respondentid) %>%
# nrow()

### NHANES Newly diagnosed (duration <= 1y + undiagnosed) dm: 1289 ###

#-------------------------------------------------------------------------
# Total sample (no T2D + new T2D), N = 29909, obs = 29909
nhanes_total <- bind_rows(nhanes_dm_newdiag,
                          nhanes_dm_undiag,
                          nhanes_ndm) %>% 
  mutate(newdm = case_when(respondentid %in% nhanes_ndm$respondentid ~ "non-diabetes",
                           TRUE ~ "diabetes"))

nhanes_undm <- bind_rows(nhanes_dm_undiag,
                          nhanes_ndm) %>% 
  mutate(newdm = case_when(respondentid %in% nhanes_ndm$respondentid ~ "non-diabetes",
                           TRUE ~ "undiagnosed diabetes"))

# weighted
nhanes_newdm_svy = nhanes_total %>% 
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = mec2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")

nhanes_undm_svy = nhanes_undm %>% 
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = mec2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")
