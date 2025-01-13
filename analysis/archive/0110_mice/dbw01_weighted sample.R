rm(list=ls());gc();source(".Rprofile")

library(mice)

mi_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/mi_dfs.RDS"))

nhanes_svy_dfs <- list()

for(i in 1:mi_dfs$m) {
  df <- complete(mi_dfs, action = i)
  
  analytic_df <- df %>% 
    # mutate(dm = case_when(dm_doc_told == 1 | glycohemoglobin >= 6.5 | fasting_glucose >= 126 ~ "diabetes",
    #                       TRUE ~ "non-diabetes")) %>% 
    mutate(
           # family income-to-poverty ratio (FIPR) (https://www.cdc.gov/nchs/data/series/sr_02/sr02_161.pdf)
           # 0.00–0.99 = Below poverty; 1.00 and above = At or above poverty.  
           fipr = case_when(poverty < 1 ~ "below poverty", 
                            poverty == 1 ~ "at poverty", 
                            poverty > 1 ~ "above poverty"),
           
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
           WHtR = waistcircumference/height
           ) %>% 
    #   # Refer: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf >> Page 22 onwards
    mutate(nhanes2yweight = mec2yweight/4) %>% 
    left_join(nhanes_20112018 %>% select(respondentid,gender,age), by=c("respondentid","age"))
  
  
  
  # Identify all DM (either dm_age is not NA OR (dm_age is NA, HbA1c >= 6.5))
  # N = 3981
  nhanes_dm_all <- analytic_df %>% 
    group_by(respondentid) %>% 
    dplyr::filter((!is.na(dm_age) | 
                     is.na(dm_age) & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
    ungroup()
  
  # Identify diagnosed DM, N = 3032
  nhanes_dm_diag <- nhanes_dm_all %>% 
    group_by(respondentid) %>%
    dplyr::filter(dm_doc_told == 1) %>%
    ungroup()
  
  # Among diagnosed DM, duration <= 1 year, N = 310
  nhanes_dm_newdiag <- nhanes_dm_diag %>% 
    group_by(respondentid) %>% 
    dplyr::filter(!is.na(dm_age) & !is.na(age) & 
                    (age - dm_age) >= 0 & 
                    (age - dm_age) <= 1) %>%
    ungroup()
  
  # Identify undiagnosed DM based on A1c. Set dm_age = current age, N = 949
  nhanes_dm_undiag <- analytic_df %>%
    group_by(respondentid) %>% 
    dplyr::filter((is.na(dm_age) & dm_doc_told != 1 & (glycohemoglobin >= 6.5 | fasting_glucose >= 126))) %>%
    ungroup() %>% 
    mutate(dm_age = age)
  
  
  # Exclude all DM from all HRS to get no DM, N = 18579
  nhanes_ndm <- analytic_df %>% 
    dplyr::filter(!respondentid %in% nhanes_dm_all$respondentid) 
  
  
  # Total sample (no T2D + new T2D), N = obs = 19842
  nhanes_total <- analytic_df %>%
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
  
  
  nhanes_total_svy <- nhanes_total %>%
    as_survey_design(ids = psu,
                     strata = pseudostratum,
                     weights = nhanes2yweight,
                     nest = TRUE,
                     pps = "brewer",
                     variance = "YG")
  
  nhanes_svy_dfs[[i]] <- nhanes_total_svy
}

# check weighted total sample size N
# sum(nhanes_20112018$nhanes2yweight)

saveRDS(nhanes_svy_dfs, paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS"))

