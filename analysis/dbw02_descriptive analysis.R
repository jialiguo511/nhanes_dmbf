rm(list=ls());gc();source(".Rprofile")

library(srvyr)
library(survey)
library(magrittr)


nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS"))

age_list <- list()
dm_age_list <- list()
female_list <- list()
prop_list <- list()

D <- length(nhanes_svy_dfs)

for (i in 1:length(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]  
  
  age_sum <- svyby(~age, ~dm, design = nhanes_total_svy, svymean, na = TRUE) %>% 
    mutate(estimate = age) %>% 
    select(dm, estimate) %>% 
    left_join(svyby(~age, ~dm, design = nhanes_total_svy, svyvar, na = TRUE) %>% 
                # within-imputation variance
                mutate(W_D = age) %>% 
                select(dm, W_D),
              by = "dm")
  
  dm_age_sum <- svyby(~dm_age, ~dm, design = nhanes_total_svy, svymean, na = TRUE) %>% 
    mutate(estimate = dm_age) %>% 
    select(dm, estimate) %>% 
    left_join(svyby(~dm_age, ~dm, design = nhanes_total_svy, svyvar, na = TRUE) %>% 
                # within-imputation variance
                mutate(W_D = dm_age) %>% 
                select(dm, W_D),
              by = "dm")
  
  female_prop <- svyby(~female, ~dm, nhanes_total_svy, svymean, na.rm = TRUE) %>% 
    mutate(female_prop = female * 100) %>%
    select(dm, female_prop)
  
  
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
  
  
  age_list[[i]] <- age_sum
  dm_age_list[[i]] <- dm_age_sum
  female_list[[i]] <- female_prop
  prop_list[[i]] <- prop_sum
  
}

prop_results <- bind_rows(prop_list) %>% 
  group_by(variable, category) %>% 
  summarize(`diagnosed diabetes >1y` = round(mean(`diagnosed diabetes >1y`, na.rm = TRUE), 1),
            `newly and undiagnosed diabetes` = round(mean(`newly and undiagnosed diabetes`, na.rm = TRUE), 1),
            `non-diabetes` = round(mean(`non-diabetes`, na.rm = TRUE), 1),
            .groups = 'drop')  

age_results <- bind_rows(age_list) %>% 
  group_by(dm) %>% 
  mutate(B_D = var(estimate)) %>% 
  summarise(theta_D = round(mean(estimate), 1),
            W_D = mean(W_D), # within-imputation variance
            B_D = mean(B_D), # between-imputation variance
            T_D = W_D + (1 + 1/D) * B_D, # total variance
            pooled_sd = round(sqrt(T_D), 1),
            .groups = 'drop')

dm_age_results <- bind_rows(dm_age_list) %>% 
  dplyr::filter(dm != "non-diabetes") %>% 
  group_by(dm) %>% 
  mutate(B_D = var(estimate)) %>% 
  summarise(theta_D = round(mean(estimate), 1),
            W_D = mean(W_D), # within-imputation variance
            B_D = mean(B_D), # between-imputation variance
            T_D = W_D + (1 + 1/D) * B_D, # total variance
            pooled_sd = round(sqrt(T_D), 1),
            .groups = 'drop')

female_results <- bind_rows(female_list) %>% 
  group_by(dm) %>% 
  summarise(theta_D = round(mean(female_prop), 1),
            .groups = 'drop')







#----------------------------------------------------------------------------------------------------------------
### dm vs non-dm

for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    mutate(
      dm_sex = case_when(female == 1 & dm == "non-diabetes" ~ "female non-diabetes",
                         female == 0 & dm == "non-diabetes" ~ "male non-diabetes",
                         female == 1 & dm == "newly and undiagnosed diabetes" ~ "female newly and undiagnosed diabetes",
                         female == 0 & dm == "newly and undiagnosed diabetes" ~ "male newly and undiagnosed diabetes",
                         female == 1 & dm == "diagnosed diabetes >1y" ~ "female diagnosed diabetes >1y",
                         female == 0 & dm == "diagnosed diabetes >1y" ~ "male diagnosed diabetes >1y"),
      race_num = case_when(race_eth == "Hispanic" ~ 1,
                           race_eth == "NH White" ~ 2,
                           race_eth == "NH Black" ~ 3,
                           race_eth == "Asian" ~ 4,
                           TRUE ~ 5)
    ) 
  
  
  svyby(bmi, ~dm_sex, nhanes_total_svy, svymean, na.rm = TRUE, adjust =~age + race_num)

}









