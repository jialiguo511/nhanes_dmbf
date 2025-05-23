rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw02_weighted df new and established dm.RDS")) 

bmi_list <- list()
fat_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    mutate(
      sex_year = case_when(
        year == "2011-2012" & female == 1 ~ "2011-2012 female",
        year == "2013-2014" & female == 1 ~ "2013-2014 female",
        year == "2015-2016" & female == 1 ~ "2015-2016 female",
        year == "2017-2018" & female == 1 ~ "2017-2018 female",
        
        year == "2011-2012" & female == 0 ~ "2011-2012 male",
        year == "2013-2014" & female == 0 ~ "2013-2014 male",
        year == "2015-2016" & female == 0 ~ "2015-2016 male",
        year == "2017-2018" & female == 0 ~ "2017-2018 male"
      )
    ) 
  
  
  bmi_mod <- svyglm(bmi ~ age + sex_year, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ sex_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + sex_year, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ sex_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(sex_year) %>%
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
  pool_ad(bmi_list) %>% select(sex_year, theta_D, L, U) %>% 
    mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(sex_year, theta_D, L, U) %>% 
    mutate(variable = "Fat percentage")
) %>% 
  separate(sex_year, into = c("year", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "new and established dm/analysis/dbw03f_descriptive characteristics by sex and year.csv")

