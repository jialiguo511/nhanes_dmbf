rm(list=ls());gc();source(".Rprofile")

library(survey)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw02_weighted df.RDS")) 

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
  summarize(`normal` = as.character(round(mean(`normal`, na.rm = TRUE), 1)),
            `prediabetes` = as.character(round(mean(`prediabetes`, na.rm = TRUE), 1)),
            `newly and undiagnosed diabetes` = as.character(round(mean(`newly and undiagnosed diabetes`, na.rm = TRUE), 1)),
            `diagnosed diabetes >1y` = as.character(round(mean(`diagnosed diabetes >1y`, na.rm = TRUE), 1)),
            .groups = 'drop')


all_results <- bind_rows(continuous_results, prop_results) %>% 
  write_csv(., "analysis/dbw03a_descriptive characteristics by dm.csv")




