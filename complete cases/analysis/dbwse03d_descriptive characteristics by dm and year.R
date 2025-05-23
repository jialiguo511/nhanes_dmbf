rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

bmi_list <- list()
fat_list <- list()
visfat_list <- list()
subfat_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    mutate(
      dm_year = case_when(
        year == "2011-2012" & dm == "NoDM" ~ "2011-2012 NoDM",
        year == "2013-2014" & dm == "NoDM" ~ "2013-2014 NoDM",
        year == "2015-2016" & dm == "NoDM" ~ "2015-2016 NoDM",
        year == "2017-2018" & dm == "NoDM" ~ "2017-2018 NoDM",
        
        year == "2011-2012" & dm == "PreDM" ~ "2011-2012 PreDM",
        year == "2013-2014" & dm == "PreDM" ~ "2013-2014 PreDM",
        year == "2015-2016" & dm == "PreDM" ~ "2015-2016 PreDM",
        year == "2017-2018" & dm == "PreDM" ~ "2017-2018 PreDM",
      
        year == "2011-2012" & dm == "DM" ~ "2011-2012 DM",
        year == "2013-2014" & dm == "DM" ~ "2013-2014 DM",
        year == "2015-2016" & dm == "DM" ~ "2015-2016 DM",
        year == "2017-2018" & dm == "DM" ~ "2017-2018 DM"
      )
    ) 
  
  
  bmi_mod <- svyglm(bmi ~ age + race_eth + female + dm_year, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + race_eth + female + dm_year, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
  visfat_mod <- svyglm(visceral_fat ~ age + race_eth + female + dm_year, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  visfat_emm <- emmeans(
    object = visfat_mod, 
    specs  = ~ dm_year, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  visfat_emm_df <- as.data.frame(summary(visfat_emm))
  visfat_list[[i]] <- visfat_emm_df
  
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
    mutate(variable = "Fat percentage"),
  pool_ad(visfat_list) %>% select(dm_year, theta_D, L, U) %>% 
    mutate(variable = "Visceral fat mass")
) %>% 
  separate(dm_year, into = c("year", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "complete cases/analysis/dbwse03d_descriptive characteristics by dm and year.csv")

