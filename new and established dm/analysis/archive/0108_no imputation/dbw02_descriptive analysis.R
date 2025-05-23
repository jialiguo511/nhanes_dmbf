rm(list=ls());gc();source(".Rprofile")

library(gtsummary)

nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(
    dm_sex = case_when(female == 1 & dm == "non-diabetes" ~ "female non-diabetes",
                       female == 0 & dm == "non-diabetes" ~ "male non-diabetes",
                       female == 1 & dm == "newly and undiagnosed diabetes" ~ "female newly and undiagnosed diabetes",
                       female == 0 & dm == "newly and undiagnosed diabetes" ~ "male newly and undiagnosed diabetes",
                       female == 1 & dm == "diagnosed diabetes >1y" ~ "female diagnosed diabetes >1y",
                       female == 0 & dm == "diagnosed diabetes >1y" ~ "male diagnosed diabetes >1y"),
    race_num = case_when(race == 1 | race == 2 ~ 1,
                         race == 3 ~ 2,
                         race == 4 ~ 3,
                         race == 6 ~ 4,
                         TRUE ~ 5)
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
library(broom)

adjusted_means_list <- list()
variables <- c("bmi", "fat_percentage", "waistcircumference", "fasting_glucose","glycohemoglobin",
               "sbp", "dbp", "ldl", "hdl", "triglyceride", "total_cholesterol")


for(var in variables) {
  
  formula_model <- as.formula(paste(var, "~ age + race_num"))
  model <- svyglm(formula_model, design = nhanes_total_svy)
  formula_means <- as.formula(paste("~", var))
  adjusted_means <- svyby(formula_means, ~dm_sex, nhanes_total_svy, svymean, na.rm = TRUE, adjust =~age + race_num)
  names(adjusted_means) <- c("dm_sex", "mean", "se")
  adjusted_means$Variable <- var

  adjusted_means_list[[var]] <- adjusted_means
}

combined_results <- bind_rows(adjusted_means_list)

wide_df <- combined_results %>%
  mutate(mean_sd = sprintf("%.1f(%.1f)", mean, se)) %>%  # Create a new column combining mean and standard error
  select(-mean, -se) %>%  # Remove the original mean and se columns
  pivot_wider(names_from = dm_sex, values_from = mean_sd, names_sort = TRUE) %>% 
  write_csv(., "analysis/dbw02_descriptive lab characteristics by dm and sex.csv")







###?????????????????????
library(quantreg)

quantiles <- c(0.25, 0.50, 0.75)  # Define quantiles
df1 <- nhanes_total_svy %>% dplyr::filter(dm_sex == "female non-diabetes")
models <- lapply(quantiles, function(q) {
  rq(glycohemoglobin ~ age + race, data = df1, weights = nhanes2yweight, tau = q)
})

# Checking model summaries
lapply(models, summary)





model <- svyglm(bmi~ age + race, design = nhanes_total_svy)
adjusted_means <- svyby(~bmi, ~dm_sex, nhanes_total_svy, svymean, na.rm = TRUE, adjust =~age + race)












