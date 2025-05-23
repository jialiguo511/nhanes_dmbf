rm(list=ls());gc();source(".Rprofile")

library(survey)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

### continuous variables
age_list <- vector("list", length(nhanes_svy_dfs))
dm_age_list <- vector("list", length(nhanes_svy_dfs))
female_list <- vector("list", length(nhanes_svy_dfs))

for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]
  
  # Calculate means and variances
  mean_age <- svyby(~age, ~dm, design = nhanes_total_svy, svymean, na.rm = TRUE)
  var_age <- svyby(~age, ~dm, design = nhanes_total_svy, svyvar, na.rm = TRUE)
  
  mean_dm_age <- svyby(~dm_age, ~dm, design = nhanes_total_svy, svymean, na.rm = TRUE)
  var_dm_age <- svyby(~dm_age, ~dm, design = nhanes_total_svy, svyvar, na.rm = TRUE)
  
  
  female_prop <- svyby(~female, ~dm, nhanes_total_svy, svymean, na.rm = TRUE) %>% 
    mutate(estimate = female * 100) %>%
    select(dm, estimate)
  
  age_sum <- merge(mean_age, var_age, by = "dm", all.x = TRUE) %>% 
    rename(estimate = age.x,
           W_D = age.y) %>% 
    select(dm, estimate, W_D)
  
  dm_age_sum <- merge(mean_dm_age, var_dm_age, by = "dm", all.x = TRUE) %>% 
    rename(estimate = dm_age.x,
           W_D = dm_age.y) %>% 
    select(dm, estimate, W_D)
  
  age_list[[i]] <- age_sum
  dm_age_list[[i]] <- dm_age_sum
  female_list[[i]] <- female_prop
  
}

# Combine all results and calculate pooled statistics
age_results <- bind_rows(age_list) %>%
  group_by(dm) %>%
  summarise(
    theta_D = round(mean(estimate), 1),   # pooled mean
    W_D = mean(W_D),                      # average within-imputation variance
    B_D = var(estimate),                  # between-imputation variance
    .groups = 'drop'
  ) %>%
  mutate(
    D = length(nhanes_svy_dfs),           # number of imputations
    T_D = W_D + (1 + 1/D) * B_D,          # total variance according to Rubin's rules
    pooled_sd = round(sqrt(T_D), 1)       # pooled standard deviation
  ) %>% 
  mutate(estimate = paste0(round(theta_D, 1), " \t (",
                           round(pooled_sd, 1), ")"))


dm_age_results <- bind_rows(dm_age_list) %>%
  group_by(dm) %>%
  summarise(
    theta_D = round(mean(estimate), 1),   # pooled mean
    W_D = mean(W_D),                      # average within-imputation variance
    B_D = var(estimate),                  # between-imputation variance
    .groups = 'drop'
  ) %>%
  mutate(
    D = length(nhanes_svy_dfs),           # number of imputations
    T_D = W_D + (1 + 1/D) * B_D,          # total variance according to Rubin's rules
    pooled_sd = round(sqrt(T_D), 1)       # pooled standard deviation
  ) %>% 
  mutate(estimate = paste0(round(theta_D, 1), " \t (",
                           round(pooled_sd, 1), ")"))


female_results <- bind_rows(female_list) %>% 
  group_by(dm) %>% 
  summarise(estimate = round(mean(estimate), 1),
            .groups = 'drop')


continuous_results <- bind_rows(
  age_results %>% select(dm, estimate) %>% mutate(variable = "age"),
  dm_age_results %>% select(dm, estimate) %>% mutate(variable = "dm_age"),
  female_results %>% select(dm, estimate) %>% mutate(variable = "female", estimate = as.character(estimate))
) %>% 
  pivot_wider(names_from = dm, values_from = estimate, names_sort = TRUE) 




### proportion variables
prop_list <- vector("list", length(nhanes_svy_dfs))

for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]
  
  prop_fipr_dm <- as.data.frame(100 * prop.table(svytable(~fipr + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = fipr,
           variable = "fipr") %>% 
    select(-fipr)
  prop_race_dm <- as.data.frame(100 * prop.table(svytable(~race_eth + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = race_eth,
           variable = "race_eth") %>% 
    select(-race_eth) 
  prop_imm_dm <- as.data.frame(100 * prop.table(svytable(~immigrant + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = immigrant,
           variable = "immigrant") %>% 
    select(-immigrant) 
  prop_edu_dm <- as.data.frame(100 * prop.table(svytable(~education + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = education,
           variable = "education") %>% 
    select(-education)
  prop_mari_dm <- as.data.frame(100 * prop.table(svytable(~marital + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = marital,
           variable = "marital") %>% 
    select(-marital) 
  prop_insu_dm <- as.data.frame(100 * prop.table(svytable(~insurance + dm, design = nhanes_total_svy), margin = 2)) %>% 
    pivot_wider(names_from = dm, values_from = Freq) %>% 
    mutate(category = insurance,
           variable = "insurance") %>% 
    select(-insurance) 
  
  prop_sum <- bind_rows(prop_fipr_dm,
                        prop_race_dm,
                        prop_imm_dm,
                        prop_edu_dm,
                        prop_mari_dm,
                        prop_insu_dm)
  
  prop_list[[i]] <- prop_sum
}

prop_results <- bind_rows(prop_list) %>%
  group_by(variable, category) %>%
  summarize(`NoDM` = as.character(round(mean(`NoDM`, na.rm = TRUE), 1)),
            `PreDM` = as.character(round(mean(`PreDM`, na.rm = TRUE), 1)),
            `DM` = as.character(round(mean(`DM`, na.rm = TRUE), 1)),
            .groups = 'drop')


all_results <- bind_rows(continuous_results, prop_results) %>% 
  write_csv(., "complete cases/analysis/dbwse03a_descriptive characteristics by dm.csv")


#------------------------------------------------------------------------------------
# continuous variables: Survey-weighted ANOVA/Wald test
# categorical variables: Rao–Scott chi-square

library(lmtest)

age_result <- vector("list", length(nhanes_svy_dfs))
female_result <- vector("list", length(nhanes_svy_dfs))
race_result <- vector("list", length(nhanes_svy_dfs))
fipr_result <- vector("list", length(nhanes_svy_dfs))
imm_result <- vector("list", length(nhanes_svy_dfs))
edu_result <- vector("list", length(nhanes_svy_dfs))
mari_result <- vector("list", length(nhanes_svy_dfs))
insu_result <- vector("list", length(nhanes_svy_dfs))

for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]
  
  # continuous variables
  age_test <- svyglm(age ~ dm, design = nhanes_total_svy)
  
  age_result[[i]] <- regTermTest(age_test, ~ dm)
  
  # categorical variables
  female_result[[i]] <- svychisq(~ female + dm, design = nhanes_total_svy, statistic = "F")
  race_result[[i]] <- svychisq(~ race_eth + dm, design = nhanes_total_svy, statistic = "F")
  fipr_result[[i]] <- svychisq(~ fipr + dm, design = nhanes_total_svy, statistic = "F")
  imm_result[[i]] <- svychisq(~ immigrant + dm, design = nhanes_total_svy, statistic = "F")
  edu_result[[i]] <- svychisq(~ education + dm, design = nhanes_total_svy, statistic = "F")
  mari_result[[i]] <- svychisq(~ marital + dm, design = nhanes_total_svy, statistic = "F")
  insu_result[[i]] <- svychisq(~ insurance + dm, design = nhanes_total_svy, statistic = "F")
  
}

# combine all, finalize p-value --------------------------------------------
nhanes_all <- do.call(rbind, lapply(nhanes_svy_dfs, function(design_obj) design_obj$variables))

nhanes_all_svy <- nhanes_all %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG")

age_test <- svyglm(age ~ dm, design = nhanes_all_svy)

age_result <- regTermTest(age_test, ~ dm)

female_result <- svychisq(~ female + dm, design = nhanes_all_svy, statistic = "F")
race_result <- svychisq(~ race_eth + dm, design = nhanes_all_svy, statistic = "F")
fipr_result <- svychisq(~ fipr + dm, design = nhanes_all_svy, statistic = "F")
imm_result <- svychisq(~ immigrant + dm, design = nhanes_all_svy, statistic = "F")
edu_result <- svychisq(~ education + dm, design = nhanes_all_svy, statistic = "F")
mari_result <- svychisq(~ marital + dm, design = nhanes_all_svy, statistic = "F")
insu_result <- svychisq(~ insurance + dm, design = nhanes_all_svy, statistic = "F")


