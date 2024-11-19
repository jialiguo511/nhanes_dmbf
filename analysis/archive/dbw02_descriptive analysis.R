rm(list=ls());gc();source(".Rprofile")

nhanes_20112018_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(weight_category = case_when(
    female == 1 & bmi < 18.5 & fat_percentage <= 25 ~ "Underweight",
    female == 1 & bmi >= 18.5 & bmi < 25 & fat_percentage > 25 & fat_percentage <= 30 ~ "Normal",
    female == 1 & bmi >= 25 & bmi < 30 & fat_percentage > 30 & fat_percentage < 35 ~ "Overweight",
    female == 1 & bmi >= 30 & fat_percentage >= 35 ~ "Obesity",
    female == 0 & bmi < 18.5 & fat_percentage <= 14 ~ "Underweight",
    female == 0 & bmi >= 18.5 & bmi < 25 & fat_percentage > 14 & fat_percentage <= 20 ~ "Normal",
    female == 0 & bmi >= 25 & bmi < 30 & fat_percentage > 20 & fat_percentage < 25 ~ "Overweight",
    female == 0 & bmi >= 30 & fat_percentage >= 25 ~ "Obesity",
    TRUE ~ "Undefined"),
    bmi_weight = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obesity",
      TRUE ~ "Undefined"
    )) 

#----------------------------------------------------------------------------------------------------------------
### dm vs non-dm
library(gtsummary)
svy_summary <- nhanes_20112018_svy %>% 
  tbl_svysummary(
    by = dm,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","dm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics by dm.docx")

#----------------------------------------------------------------------------------------------------------------
### newly diagnosed only: dm vs non-dm
library(gtsummary)
svy_summary <- nhanes_newdm1y_svy %>% 
  tbl_svysummary(
    by = newdm,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","newdm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics newly diagnosed by dm.docx")
#-------------------------------------------------------------------------------------------------------------------------------
### gender * dm

nhanes_20112018_svyobj <- nhanes_20112018_svy %>% 
  mutate(sex_dm = case_when(
    female == 1 & dm == "diabetes" ~ "female with diabetes",
    female == 1 & dm == "non-diabetes" ~ "female without diabetes",
    female == 0 & dm == "diabetes" ~ "male with diabetes",
    TRUE ~ "male without diabetes"))
  
svy_summary <- nhanes_20112018_svyobj %>% 
  tbl_svysummary(
    by = sex_dm,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","dm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics by sex and dm.docx")

#-------------------------------------------------------------------------------------------------------------------------------
# mis-classification of BMI vs BF%
# 1.1 non-dm by gender

### male vs female
library(gtsummary)
svy_summary <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "non-diabetes") %>% 
  tbl_svysummary(
    by = female,
    missing_text = "Missing",
    include = c("fat_percentage","bmi","bmi_category"),
    type = list(fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                bmi_category ~ "categorical"
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
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of non-dm by sex.docx")


nhanes_svy_ndm <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "non-diabetes")

bmi_fat_gender_table <- svytable(~bmi_weight + weight_category + female, design = nhanes_svy_ndm)

# Calculate proportions within each BMI and gender category
bmi_fat_gender_proportions <- prop.table(bmi_fat_gender_table, margin = 1) * 100  # margin=1 normalizes by rows

# Convert to data frame for easier manipulation
bmi_fat_gender_results <- as.data.frame(bmi_fat_gender_proportions)
names(bmi_fat_gender_results) <- c("BMI Category", "Weight Category", "Gender", "Percentage")

# Adjust gender naming for clarity
bmi_fat_gender_results$Gender <- ifelse(bmi_fat_gender_results$Gender == 1, "Female", "Male")

# Print results
print(bmi_fat_gender_results)

#--------------------------------------------------------------------------------
# 1.2 non-dm by race_eth, exclude other race
svy_summary <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "non-diabetes" & race_eth != "Other Race") %>% 
  tbl_svysummary(
    by = race_eth,
    missing_text = "Missing",
    include = c("fat_percentage","bmi","bmi_category","weight_category"),
    type = list(fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                bmi_category ~ "categorical",
                weight_category ~ "categorical"
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
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of non-dm by race.docx")


nhanes_svy_ndm <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "non-diabetes" & race_eth != "Other Race")

bmi_fat_race_table <- svytable(~bmi_weight + weight_category + race_eth, design = nhanes_svy_ndm)

# Calculate proportions within each BMI and gender category
bmi_fat_race_proportions <- prop.table(bmi_fat_race_table, margin = 1) * 100  # margin=1 normalizes by rows

bmi_fat_race_results <- as.data.frame(bmi_fat_race_proportions)
names(bmi_fat_race_results) <- c("BMI Category", "Weight Category", "Race", "Percentage")

# Print results
print(bmi_fat_race_results)

#--------------------------------------------------------------------------------
# 2.1 dm by sex
svy_summary <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "diabetes") %>% 
  tbl_svysummary(
    by = female,
    missing_text = "Missing",
    include = c("fat_percentage","bmi","bmi_category"),
    type = list(fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                bmi_category ~ "categorical"
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
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of dm by sex.docx")


nhanes_svy_dm <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "diabetes")

bmi_fat_gender_table <- svytable(~bmi_weight + weight_category + female, design = nhanes_svy_dm)

# Calculate proportions within each BMI and gender category
bmi_fat_gender_proportions <- prop.table(bmi_fat_gender_table, margin = 1) * 100  # margin=1 normalizes by rows

# Convert to data frame for easier manipulation
bmi_fat_gender_results <- as.data.frame(bmi_fat_gender_proportions)
names(bmi_fat_gender_results) <- c("BMI Category", "Weight Category", "Gender", "Percentage")

# Adjust gender naming for clarity
bmi_fat_gender_results$Gender <- ifelse(bmi_fat_gender_results$Gender == 1, "Female", "Male")

# Print results
print(bmi_fat_gender_results)

#--------------------------------------------------------------------------------
# 2.2 dm by race, exclude other race
svy_summary <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "diabetes" & race_eth != "Other Race") %>% 
  tbl_svysummary(
    by = race_eth,
    missing_text = "Missing",
    include = c("fat_percentage","bmi","bmi_category","weight_category"),
    type = list(fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                bmi_category ~ "categorical",
                weight_category ~ "categorical"
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
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of dm by race.docx")


nhanes_svy_dm <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "diabetes" & race_eth != "Other Race")

bmi_fat_race_table <- svytable(~bmi_weight + weight_category + race_eth, design = nhanes_svy_dm)

# Calculate proportions within each BMI and gender category
bmi_fat_race_proportions <- prop.table(bmi_fat_race_table, margin = 1) * 100  # margin=1 normalizes by rows

bmi_fat_race_results <- as.data.frame(bmi_fat_race_proportions)
names(bmi_fat_race_results) <- c("BMI Category", "Weight Category", "Race", "Percentage")

# Print results
print(bmi_fat_race_results)

#-------------------------------------------------------------------------------------------------------------------------------
### dm vs non-dm, newly diagnosed + undiagnosed
source("analysis/dbw01_new dm weighted sample.R")

svy_summary <- nhanes_newdm_svy %>% 
  tbl_svysummary(
    by = newdm,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","dm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of newly diagnosed and undiagnosed by dm.docx")

#----------------------------------------------------------------------------------------------------------------------------
### 2011-2014 vs 2015-2018
library(gtsummary)
svy_summary <- nhanes_20112018_svy %>% 
  tbl_svysummary(
    by = year_broad,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","dm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics by year.docx")

#----------------------------------------------------------------------------------------------------------------------------
### dm: 2011-2014 vs 2015-2018
library(gtsummary)
svy_summary <- nhanes_20112018_svy %>% 
  dplyr::filter(dm == "diabetes") %>% 
  tbl_svysummary(
    by = year_broad,
    missing_text = "Missing",
    include = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp","dm","female",
                "race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type"
    ),
    type = list(age ~ "continuous2",
                dm_age ~ "continuous2",
                waistcircumference ~ "continuous2",
                fasting_glucose ~ "continuous2",
                triglyceride ~ "continuous2",
                ldl ~ "continuous2",
                hdl ~ "continuous2",
                total_fat ~ "continuous2",
                fat_percentage ~ "continuous2",
                bmi ~ "continuous2",
                glycohemoglobin ~ "continuous2",
                sbp ~ "continuous2",
                dbp ~ "continuous2",
                female ~ "dichotomous",
                race_eth ~ "categorical",
                fipr ~ "categorical",
                bmi_category ~ "categorical",
                immigrant ~ "categorical",
                education ~ "categorical",
                marital ~ "categorical",
                insurance ~ "categorical",
                insurance_type ~ "categorical"
    ), 
    statistic = list(all_continuous2() ~ c("{mean} ({sd})","{N_miss_unweighted}"),
                     all_categorical() ~ "{p}%",
                     all_dichotomous() ~ "{p}%",
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}")),
    digits = list(all_continuous2() ~ 1, # Setting 1 decimal 
                  glycohemoglobin ~ 1, 
                  all_categorical() ~ 1,
                  all_dichotomous() ~ 1) 
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt() %>% 
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics of dm cases by year.docx")
