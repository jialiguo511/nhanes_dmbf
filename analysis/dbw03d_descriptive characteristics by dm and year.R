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
      dm_year = case_when(
        year == "2011-2012" & dm == "normal" ~ "2011-2012 normal",
        year == "2013-2014" & dm == "normal" ~ "2013-2014 normal",
        year == "2015-2016" & dm == "normal" ~ "2015-2016 normal",
        year == "2017-2018" & dm == "normal" ~ "2017-2018 normal",
        
        year == "2011-2012" & dm == "prediabetes" ~ "2011-2012 prediabetes",
        year == "2013-2014" & dm == "prediabetes" ~ "2013-2014 prediabetes",
        year == "2015-2016" & dm == "prediabetes" ~ "2015-2016 prediabetes",
        year == "2017-2018" & dm == "prediabetes" ~ "2017-2018 prediabetes",

        year == "2011-2012" & dm == "newly and undiagnosed diabetes" ~ "2011-2012 newly and undiagnosed diabetes",
        year == "2013-2014" & dm == "newly and undiagnosed diabetes" ~ "2013-2014 newly and undiagnosed diabetes",
        year == "2015-2016" & dm == "newly and undiagnosed diabetes" ~ "2015-2016 newly and undiagnosed diabetes",
        year == "2017-2018" & dm == "newly and undiagnosed diabetes" ~ "2017-2018 newly and undiagnosed diabetes",
      
        year == "2011-2012" & dm == "diagnosed diabetes >1y" ~ "2011-2012 diagnosed diabetes >1y",
        year == "2013-2014" & dm == "diagnosed diabetes >1y" ~ "2013-2014 diagnosed diabetes >1y",
        year == "2015-2016" & dm == "diagnosed diabetes >1y" ~ "2015-2016 diagnosed diabetes >1y",
        year == "2017-2018" & dm == "diagnosed diabetes >1y" ~ "2017-2018 diagnosed diabetes >1y"
      )
    ) 
  
  
  bmi_mod <- svyglm(bmi ~ age + dm_year, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + dm_year, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_year) %>%
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
  pool_ad(bmi_list) %>% select(dm_year, theta_D, L, U) %>% 
    mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_year, theta_D, L, U) %>% 
    mutate(variable = "Fat percentage")
) %>% 
  separate(dm_year, into = c("year", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "analysis/dbw03d_descriptive characteristics by dm and year.csv")

