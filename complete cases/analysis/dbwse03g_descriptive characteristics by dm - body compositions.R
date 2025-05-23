rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

bmi_list <- list()
fat_list <- list()
visfat_list <- list()
fpg_list <- list()
a1c_list <- list()
wt_list <- list()
whtr_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df 
  
  bmi_mod <- svyglm(bmi ~ age + race_eth + female + dm, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + race_eth + female + dm, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
  visfat_mod <- svyglm(visceral_fat ~ age + race_eth + female + dm, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm
  visfat_emm <- emmeans(
    object = visfat_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  visfat_emm_df <- as.data.frame(summary(visfat_emm))
  visfat_list[[i]] <- visfat_emm_df
  
  
  fpg_mod <- svyglm(fasting_glucose ~ age + race_eth + dm + female, design = nhanes_total_svy)
  fpg_emm <- emmeans(
    object = fpg_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fpg_emm_df <- as.data.frame(summary(fpg_emm))
  fpg_list[[i]] <- fpg_emm_df
  
  
  a1c_mod <- svyglm(glycohemoglobin ~ age + race_eth + dm + female, design = nhanes_total_svy)
  a1c_emm <- emmeans(
    object = a1c_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  a1c_emm_df <- as.data.frame(summary(a1c_emm))
  a1c_list[[i]] <- a1c_emm_df
  
  
  wt_mod <- svyglm(waistcircumference ~ age + race_eth + dm + female, design = nhanes_total_svy)
  wt_emm <- emmeans(
    object = wt_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  wt_emm_df <- as.data.frame(summary(wt_emm))
  wt_list[[i]] <- wt_emm_df
  
  
  whtr_mod <- svyglm(WHtR ~ age + race_eth + dm + female, design = nhanes_total_svy)
  whtr_emm <- emmeans(
    object = whtr_mod, 
    specs  = ~ dm, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  whtr_emm_df <- as.data.frame(summary(whtr_emm))
  whtr_list[[i]] <- whtr_emm_df
  
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm) %>%
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
    ) %>% 
    mutate(estimate = paste0(round(theta_D, 1), " \t (",
                             round(L, 1), ", ",
                             round(U, 1), ")"))
  
  return(pooled_results)
}


all_results <- bind_rows(
  pool_ad(bmi_list) %>% select(dm, estimate) %>% mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm, estimate) %>% mutate(variable = "Fat percentage"), 
  pool_ad(visfat_list) %>% select(dm, estimate) %>% mutate(variable = "Visceral fat mass"),
  pool_ad(fpg_list) %>% select(dm, estimate) %>% mutate(variable = "Fasting glucose"), 
  pool_ad(a1c_list) %>% select(dm, estimate) %>% mutate(variable = "Glycohemoglobin"),
  pool_ad(wt_list) %>% select(dm, estimate) %>% mutate(variable = "Waistcircumference"),
  pool_ad(whtr_list) %>% select(dm, estimate) %>% mutate(variable = "Waist-to-Height Ratio")
) %>% 
  write_csv(., "complete cases/analysis/dbwse03g_descriptive characteristics by dm - body compositions.csv")


