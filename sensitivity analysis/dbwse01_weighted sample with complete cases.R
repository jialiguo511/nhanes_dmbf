rm(list=ls());gc();source(".Rprofile")

library(mice)
library(survey)

mi_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/mi_dfs.RDS"))
nhanes_unweighted <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_unweighted sample.RDS"))


nhanes_svy_dfs <- list()

for(i in 1:mi_dfs$m) {
  df <- complete(mi_dfs, action = i)
  
  analytic_df <- df %>% 
    select(-fat_percentage) %>% 
    left_join(nhanes_unweighted %>% 
                select(respondentid,fat_percentage),
              by = "respondentid") %>% 
    dplyr::filter(!is.na(fat_percentage)) %>% 
    mutate(sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
           dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE),
           WHtR = waistcircumference/height) %>% 
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
  nhanes_NoDM <- analytic_df %>%
    group_by(respondentid) %>% 
    dplyr::filter(dm_doc_told == 0) %>% 
    dplyr::filter(glycohemoglobin < 5.7 & fasting_glucose < 100) %>%
    ungroup() 
  
  
  # Total sample, N = obs = 9,910
  nhanes_total <- analytic_df %>%
    mutate(dm = case_when(
      respondentid %in% nhanes_dm_newdiag$respondentid ~ "NewDM",
      respondentid %in% nhanes_dm_undiag$respondentid ~ "NewDM",
      respondentid %in% nhanes_dm_olddiag$respondentid ~ "DM",
      respondentid %in% nhanes_NoDM$respondentid ~ "NoDM",
      TRUE ~ "PreDM"),
      newdm = case_when(
        dm == "NoDM" ~ 0,
        dm == "NewDM" ~ 1,
        TRUE ~ NA_integer_
      )) %>% 
    mutate(
      dm_sex = case_when(female == 1 & dm == "NoDM" ~ "female NoDM",
                         female == 0 & dm == "NoDM" ~ "male NoDM",
                         female == 1 & dm == "PreDM" ~ "female PreDM",
                         female == 0 & dm == "PreDM" ~ "male PreDM",
                         female == 1 & dm == "NewDM" ~ "female NewDM",
                         female == 0 & dm == "NewDM" ~ "male NewDM",
                         female == 1 & dm == "DM" ~ "female DM",
                         female == 0 & dm == "DM" ~ "male DM")
    ) 
  
  nhanes_total_svy <- nhanes_total %>%
    as_survey_design(ids = psu,
                     strata = pseudostratum,
                     weights = nhanes2yweight,
                     nest = TRUE,
                     pps = "brewer",
                     variance = "YG")
  
  nhanes_svy_dfs[[i]] <- nhanes_total_svy

}


saveRDS(nhanes_svy_dfs, paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse01_weighted df with complete cases.RDS"))

# weighted N, proportion
svytotal(~dm, nhanes_svy_dfs[[i]])
svytotal(~dm_sex, nhanes_svy_dfs[[i]])

