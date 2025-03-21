rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw02_weighted df.RDS")) 

bmi_list <- list()
fat_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    mutate(
      dm_year_sex = case_when(
        female == 1 & year == "2011-2012" & dm == "NoDM" ~ "female 2011-2012 NoDM",
        female == 0 & year == "2011-2012" & dm == "NoDM" ~ "male 2011-2012 NoDM",
        female == 1 & year == "2013-2014" & dm == "NoDM" ~ "female 2013-2014 NoDM",
        female == 0 & year == "2013-2014" & dm == "NoDM" ~ "male 2013-2014 NoDM",
        female == 1 & year == "2015-2016" & dm == "NoDM" ~ "female 2015-2016 NoDM",
        female == 0 & year == "2015-2016" & dm == "NoDM" ~ "male 2015-2016 NoDM",
        female == 1 & year == "2017-2018" & dm == "NoDM" ~ "female 2017-2018 NoDM",
        female == 0 & year == "2017-2018" & dm == "NoDM" ~ "male 2017-2018 NoDM",
        
        female == 1 & year == "2011-2012" & dm == "PreDM" ~ "female 2011-2012 PreDM",
        female == 0 & year == "2011-2012" & dm == "PreDM" ~ "male 2011-2012 PreDM",
        female == 1 & year == "2013-2014" & dm == "PreDM" ~ "female 2013-2014 PreDM",
        female == 0 & year == "2013-2014" & dm == "PreDM" ~ "male 2013-2014 PreDM",
        female == 1 & year == "2015-2016" & dm == "PreDM" ~ "female 2015-2016 PreDM",
        female == 0 & year == "2015-2016" & dm == "PreDM" ~ "male 2015-2016 PreDM",
        female == 1 & year == "2017-2018" & dm == "PreDM" ~ "female 2017-2018 PreDM",
        female == 0 & year == "2017-2018" & dm == "PreDM" ~ "male 2017-2018 PreDM",
        
        female == 1 & year == "2011-2012" & dm == "NewDM" ~ "female 2011-2012 NewDM",
        female == 0 & year == "2011-2012" & dm == "NewDM" ~ "male 2011-2012 NewDM",
        female == 1 & year == "2013-2014" & dm == "NewDM" ~ "female 2013-2014 NewDM",
        female == 0 & year == "2013-2014" & dm == "NewDM" ~ "male 2013-2014 NewDM",
        female == 1 & year == "2015-2016" & dm == "NewDM" ~ "female 2015-2016 NewDM",
        female == 0 & year == "2015-2016" & dm == "NewDM" ~ "male 2015-2016 NewDM",
        female == 1 & year == "2017-2018" & dm == "NewDM" ~ "female 2017-2018 NewDM",
        female == 0 & year == "2017-2018" & dm == "NewDM" ~ "male 2017-2018 NewDM",
        
        female == 1 & year == "2011-2012" & dm == "DM" ~ "female 2011-2012 DM",
        female == 0 & year == "2011-2012" & dm == "DM" ~ "male 2011-2012 DM",
        female == 1 & year == "2013-2014" & dm == "DM" ~ "female 2013-2014 DM",
        female == 0 & year == "2013-2014" & dm == "DM" ~ "male 2013-2014 DM",
        female == 1 & year == "2015-2016" & dm == "DM" ~ "female 2015-2016 DM",
        female == 0 & year == "2015-2016" & dm == "DM" ~ "male 2015-2016 DM",
        female == 1 & year == "2017-2018" & dm == "DM" ~ "female 2017-2018 DM",
        female == 0 & year == "2017-2018" & dm == "DM" ~ "male 2017-2018 DM"
        
      )
    ) 
  
  
  bmi_mod <- svyglm(bmi ~ age + dm_year_sex, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_year_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + dm_year_sex, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_year_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_year_sex) %>%
    summarise(
      theta_D = mean(emmean, na.rm = TRUE),               # pooled mean
      W_D = mean(W_d, na.rm = TRUE),                      # average within-imputation variance
      B_D = var(emmean, na.rm = TRUE),                    # between-imputation variance
      .groups = 'drop'
    ) %>%
    mutate(
      D = length(nhanes_svy_dfs),                                # number of imputations
      T_D = W_D + (1 + 1/D) * B_D,                        # total variance according to Rubin's rules
      gamma_D = (1 + 1/D)*(B_D/T_D),                      # fraction of missing information
      nu = (D-1)*((1 + (1/(D+1))*(W_D/B_D))^2),
      nu2 = (D-1)/(gamma_D)^2
    ) %>%
    mutate(
      L = theta_D + qt(p = 0.025, df = nu2) * sqrt(T_D), # Lower 95% CI
      U = theta_D + qt(p = 0.975, df = nu2) * sqrt(T_D), # Upper 95% CI
      sqrt_T_D = sqrt(T_D)                               # Square root of total variance
    ) 
  
  return(pooled_results)
}


all_results <- bind_rows(
  pool_ad(bmi_list) %>% select(dm_year_sex, theta_D, L, U) %>% 
    mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_year_sex, theta_D, L, U) %>% 
    mutate(variable = "Fat percentage")
) %>% 
  separate(dm_year_sex, into = c("sex", "year", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "analysis/dbw03e_descriptive characteristics by dm, sex and year.csv")
