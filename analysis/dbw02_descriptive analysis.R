rm(list=ls());gc();source(".Rprofile")

library(gtsummary)

nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(
    dm_sex = case_when(female == 1 & dm == "non-diabetes" ~ "female non-diabetes",
                       female == 0 & dm == "non-diabetes" ~ "male non-diabetes",
                       female == 1 & dm == "newly and undiagnosed diabetes" ~ "female newly and undiagnosed diabetes",
                       female == 0 & dm == "newly and undiagnosed diabetes" ~ "male newly and undiagnosed diabetes",
                       female == 1 & dm == "diagnosed diabetes >1y" ~ "female diagnosed diabetes >1y",
                       female == 0 & dm == "diagnosed diabetes >1y" ~ "male diagnosed diabetes >1y")
    ) 

#----------------------------------------------------------------------------------------------------------------
### dm vs non-dm
svy_summary <- nhanes_total_svy %>% 
  tbl_svysummary(
    by = dm,
    missing_text = "Missing",
    include = c("age","dm_age","dm","female","race_eth","fipr","immigrant","education","marital","insurance"),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%"),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics by dm.docx")

#----------------------------------------------------------------------------------------------------------------
### dm vs non-dm by sex
svy_summary <- nhanes_total_svy %>% 
  tbl_svysummary(
    by = dm_sex,
    missing_text = "Missing",
    include = c("bmi","fat_percentage","waistcircumference","glycohemoglobin","fasting_glucose",
                "sbp","dbp","ldl","hdl","triglyceride","total_cholesterol"
    ),
    design = ~ age + female + race_eth,
    type = list(waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                total_cholesterol ~ "continuous2"), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive lab characteristics by dm and sex.docx")
