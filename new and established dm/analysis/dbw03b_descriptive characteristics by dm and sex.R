rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw02_weighted df new and established dm.RDS")) 

bmi_list <- list()
fat_list <- list()
wt_list <- list()
whtr_list <- list()
fpg_list <- list()
a1c_list <- list()
sbp_list <- list()
dbp_list <- list()
ldl_list <- list()
hdl_list <- list()
tgl_list <- list()
tc_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    mutate(
      dm_sex = case_when(female == 1 & dm == "NoDM" ~ "female NoDM",
                         female == 0 & dm == "NoDM" ~ "male NoDM",
                         female == 1 & dm == "PreDM" ~ "female PreDM",
                         female == 0 & dm == "PreDM" ~ "male PreDM",
                         female == 1 & dm == "NewDM" ~ "female NewDM",
                         female == 0 & dm == "NewDM" ~ "male NewDM",
                         female == 1 & dm == "DM" ~ "female DM",
                         female == 0 & dm == "DM" ~ "male DM")
    ) 
  
  bmi_mod <- svyglm(bmi ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df

  
  fat_mod <- svyglm(fat_percentage ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
  wt_mod <- svyglm(waistcircumference ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  wt_emm <- emmeans(
    object = wt_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  wt_emm_df <- as.data.frame(summary(wt_emm))
  wt_list[[i]] <- wt_emm_df
  
  
  whtr_mod <- svyglm(WHtR ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  whtr_emm <- emmeans(
    object = whtr_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  whtr_emm_df <- as.data.frame(summary(whtr_emm))
  whtr_list[[i]] <- whtr_emm_df
  
  
  fpg_mod <- svyglm(fasting_glucose ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  fpg_emm <- emmeans(
    object = fpg_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fpg_emm_df <- as.data.frame(summary(fpg_emm))
  fpg_list[[i]] <- fpg_emm_df
  
  
  a1c_mod <- svyglm(glycohemoglobin ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  a1c_emm <- emmeans(
    object = a1c_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  a1c_emm_df <- as.data.frame(summary(a1c_emm))
  a1c_list[[i]] <- a1c_emm_df
  
  
  sbp_mod <- svyglm(sbp ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  sbp_emm <- emmeans(
    object = sbp_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  sbp_emm_df <- as.data.frame(summary(sbp_emm))
  sbp_list[[i]] <- sbp_emm_df
  
  
  dbp_mod <- svyglm(dbp ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  dbp_emm <- emmeans(
    object = dbp_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  dbp_emm_df <- as.data.frame(summary(dbp_emm))
  dbp_list[[i]] <- dbp_emm_df
  
  
  ldl_mod <- svyglm(ldl ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  ldl_emm <- emmeans(
    object = ldl_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  ldl_emm_df <- as.data.frame(summary(ldl_emm))
  ldl_list[[i]] <- ldl_emm_df
  
  
  hdl_mod <- svyglm(hdl ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  hdl_emm <- emmeans(
    object = hdl_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  hdl_emm_df <- as.data.frame(summary(hdl_emm))
  hdl_list[[i]] <- hdl_emm_df
  
  
  tgl_mod <- svyglm(triglyceride ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  tgl_emm <- emmeans(
    object = tgl_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  tgl_emm_df <- as.data.frame(summary(tgl_emm))
  tgl_list[[i]] <- tgl_emm_df
  
  
  tc_mod <- svyglm(total_cholesterol ~ age + race_eth + dm_sex, design = nhanes_total_svy)
  tc_emm <- emmeans(
    object = tc_mod, 
    specs  = ~ dm_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  tc_emm_df <- as.data.frame(summary(tc_emm))
  tc_list[[i]] <- tc_emm_df
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_sex) %>%
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
  
  # save 2 digits for A1c
  # mutate(estimate = paste0(round(theta_D, 2, " \t (",
  #                          round(L, 2), ", ",
  #                          round(U, 2), ")"))

  return(pooled_results)
}


all_results <- bind_rows(
  pool_ad(bmi_list) %>% select(dm_sex, estimate) %>% mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Fat percentage"), 
  pool_ad(wt_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Waistcircumference"),
  pool_ad(whtr_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Waist-to-Height Ratio"),
  pool_ad(fpg_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Fasting glucose"), 
  pool_ad(a1c_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Glycohemoglobin"), 
  pool_ad(sbp_list) %>% select(dm_sex, estimate) %>% mutate(variable = "SBP"), 
  pool_ad(dbp_list) %>% select(dm_sex, estimate) %>% mutate(variable = "DBP"), 
  pool_ad(ldl_list) %>% select(dm_sex, estimate) %>% mutate(variable = "LDL"), 
  pool_ad(hdl_list) %>% select(dm_sex, estimate) %>% mutate(variable = "HDL"), 
  pool_ad(tgl_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Triglyceride"), 
  pool_ad(tc_list) %>% select(dm_sex, estimate) %>% mutate(variable = "Total cholesterol")
) %>% 
  pivot_wider(names_from = dm_sex, values_from = estimate, names_sort = TRUE) %>% 
  write_csv(., "new and established dm/analysis/dbw03b_descriptive characteristics by dm and sex.csv")
  



#------------------------------------------------------------------------------------
# continuous variables: Survey-weighted ANOVA/Wald test
# categorical variables: Rao–Scott chi-square

nhanes_all <- do.call(rbind, lapply(nhanes_svy_dfs, function(design_obj) design_obj$variables))

nhanes_all_svy <- nhanes_all %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG") %>% 
  mutate(
    dm_sex = case_when(female == 1 & dm == "NoDM" ~ "female NoDM",
                       female == 0 & dm == "NoDM" ~ "male NoDM",
                       female == 1 & dm == "PreDM" ~ "female PreDM",
                       female == 0 & dm == "PreDM" ~ "male PreDM",
                       female == 1 & dm == "NewDM" ~ "female NewDM",
                       female == 0 & dm == "NewDM" ~ "male NewDM",
                       female == 1 & dm == "DM" ~ "female DM",
                       female == 0 & dm == "DM" ~ "male DM")
  )

bmi_test <- svyglm(bmi ~ dm_sex, design = nhanes_all_svy)
fat_test <- svyglm(fat_percentage ~ dm_sex, design = nhanes_all_svy)
wt_test <- svyglm(waistcircumference ~ dm_sex, design = nhanes_all_svy)
whtr_test <- svyglm(WHtR ~ dm_sex, design = nhanes_all_svy)
fpg_test <- svyglm(fasting_glucose ~ dm_sex, design = nhanes_all_svy)
a1c_test <- svyglm(glycohemoglobin ~ dm_sex, design = nhanes_all_svy)
sbp_test <- svyglm(sbp ~ dm_sex, design = nhanes_all_svy)
dbp_test <- svyglm(dbp ~ dm_sex, design = nhanes_all_svy)
ldl_test <- svyglm(ldl ~ dm_sex, design = nhanes_all_svy)
hdl_test <- svyglm(hdl ~ dm_sex, design = nhanes_all_svy)
tgl_test <- svyglm(triglyceride ~ dm_sex, design = nhanes_all_svy)
tc_test <- svyglm(total_cholesterol ~ dm_sex, design = nhanes_all_svy)

bmi_result <- regTermTest(bmi_test, ~ dm_sex)
fat_result <- regTermTest(fat_test, ~ dm_sex)
wt_result <- regTermTest(wt_test, ~ dm_sex)
whtr_result <- regTermTest(whtr_test, ~ dm_sex)
fpg_result <- regTermTest(fpg_test, ~ dm_sex)
a1c_result <- regTermTest(a1c_test, ~ dm_sex)
sbp_result <- regTermTest(sbp_test, ~ dm_sex)
dbp_result <- regTermTest(dbp_test, ~ dm_sex)
ldl_result <- regTermTest(ldl_test, ~ dm_sex)
hdl_result <- regTermTest(hdl_test, ~ dm_sex)
tgl_result <- regTermTest(tgl_test, ~ dm_sex)
tc_result <- regTermTest(tc_test, ~ dm_sex)

