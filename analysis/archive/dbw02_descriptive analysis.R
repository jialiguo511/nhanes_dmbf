rm(list=ls());gc();source(".Rprofile")

source("analysis/dbw01_input data.R")
source("functions/svysummary.R")
source("functions/svysd.R")



library(gtsummary)
nhanes_20112018_svy %>% 
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
                     glycohemoglobin ~ c("{median} ({p25}, {p75})","{N_nonmiss_unweighted}"))
  ) %>% 
  add_n(statistic =  "{N_obs_unweighted}") %>% # See add_n.tbl_summary for more details
  add_overall() %>%
  as_gt()
  gt::gtsave(filename = "analysis/dbw02_descriptive characteristics by dm.docx")





  
  

nhanes_20112018_svy = nhanes_20112018 %>% 
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = normalizedmec2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")


selected_c_vars = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                    "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp")
selected_p_vars = c("female")
selected_g_vars = c("race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type")
# ,"insurance","insurance_type"


means_and_proportions_overall = svysummary(
  svy_des = nhanes_20112018_svy,
  c_vars = selected_c_vars,
  p_vars = selected_p_vars,
  g_vars = selected_g_vars,
  id_vars = "dm")

sds = svysd(svy_des = nhanes_20112018_svy,
            c_vars = selected_c_vars)




nhanes_20112018_svy = nhanes_20112018 %>% 
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = normalizedmec2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")


selected_c_vars = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                    "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp")
selected_p_vars = c("female")
selected_g_vars = c("race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type")
# ,"insurance","insurance_type"


means_and_proportions_overall = svysummary(
  svy_des = nhanes_20112018_svy,
  c_vars = selected_c_vars,
  p_vars = selected_p_vars,
  g_vars = selected_g_vars,
  id_vars = "dm")

sds = svysd(svy_des = nhanes_20112018_svy,
            c_vars = selected_c_vars)












continuous_vars <- c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
                     "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp")

proportion_vars <- c("female")

grouped_vars <- c("race_eth","fipr","bmi_category","immigrant","education","marital")


nhanes_sy <- svysummary(nhanes_20112018_svy,
                        c_vars = continuous_vars,
                        p_vars = proportion_vars,
                        g_vars = grouped_vars,
                        id_vars = "dm"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

nhanes_sy_total <- svysummary(nhanes_20112018_svy,
                              c_vars = continuous_vars,
                              p_vars = proportion_vars,
                              g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

nhanes_table1 <- bind_rows(nhanes_sy_total %>% mutate(dm = "Total"),
                           nhanes_sy) %>% 
  write_csv(.,path = "analysis/dbw02_descriptive characteristics.csv")











