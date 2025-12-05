rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

bmi_list <- list()
fat_list <- list()
visfat_list <- list()
subfat_list <- list()
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
  
  # Create age group variable (<45, >=45)
  df_with_age_group <- df$variables %>%
    mutate(age_group = ifelse(age < 45, "<45", ">=45"))
  
  # Update the design object with age_group variable
  nhanes_total_svy <- df
  nhanes_total_svy$variables$age_group <- df_with_age_group$age_group
  
  # Create dm_sex_age variable
  nhanes_total_svy$variables <- nhanes_total_svy$variables %>%
    mutate(
      dm_sex_age = case_when(
        female == 1 & dm == "NoDM" & age_group == "<45" ~ "female NoDM <45",
        female == 0 & dm == "NoDM" & age_group == "<45" ~ "male NoDM <45",
        female == 1 & dm == "PreDM" & age_group == "<45" ~ "female PreDM <45",
        female == 0 & dm == "PreDM" & age_group == "<45" ~ "male PreDM <45",
        female == 1 & dm == "DM" & age_group == "<45" ~ "female DM <45",
        female == 0 & dm == "DM" & age_group == "<45" ~ "male DM <45",
        female == 1 & dm == "NoDM" & age_group == ">=45" ~ "female NoDM >=45",
        female == 0 & dm == "NoDM" & age_group == ">=45" ~ "male NoDM >=45",
        female == 1 & dm == "PreDM" & age_group == ">=45" ~ "female PreDM >=45",
        female == 0 & dm == "PreDM" & age_group == ">=45" ~ "male PreDM >=45",
        female == 1 & dm == "DM" & age_group == ">=45" ~ "female DM >=45",
        female == 0 & dm == "DM" & age_group == ">=45" ~ "male DM >=45"
      )
    )
  
  bmi_mod <- svyglm(bmi ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex_age
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
  visfat_mod <- svyglm(visceral_fat ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of visceral fat by dm_sex_age
  visfat_emm <- emmeans(
    object = visfat_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  visfat_emm_df <- as.data.frame(summary(visfat_emm))
  visfat_list[[i]] <- visfat_emm_df
  
  
  subfat_mod <- svyglm(subcutaneous_fat ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  subfat_emm <- emmeans(
    object = subfat_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  subfat_emm_df <- as.data.frame(summary(subfat_emm))
  subfat_list[[i]] <- subfat_emm_df
  
  
  wt_mod <- svyglm(waistcircumference ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  wt_emm <- emmeans(
    object = wt_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  wt_emm_df <- as.data.frame(summary(wt_emm))
  wt_list[[i]] <- wt_emm_df
  
  
  whtr_mod <- svyglm(WHtR ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  whtr_emm <- emmeans(
    object = whtr_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  whtr_emm_df <- as.data.frame(summary(whtr_emm))
  whtr_list[[i]] <- whtr_emm_df
  
  
  fpg_mod <- svyglm(fasting_glucose ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  fpg_emm <- emmeans(
    object = fpg_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fpg_emm_df <- as.data.frame(summary(fpg_emm))
  fpg_list[[i]] <- fpg_emm_df
  
  
  a1c_mod <- svyglm(glycohemoglobin ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  a1c_emm <- emmeans(
    object = a1c_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  a1c_emm_df <- as.data.frame(summary(a1c_emm))
  a1c_list[[i]] <- a1c_emm_df
  
  
  sbp_mod <- svyglm(sbp ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  sbp_emm <- emmeans(
    object = sbp_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  sbp_emm_df <- as.data.frame(summary(sbp_emm))
  sbp_list[[i]] <- sbp_emm_df
  
  
  dbp_mod <- svyglm(dbp ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  dbp_emm <- emmeans(
    object = dbp_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  dbp_emm_df <- as.data.frame(summary(dbp_emm))
  dbp_list[[i]] <- dbp_emm_df
  
  
  ldl_mod <- svyglm(ldl ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  ldl_emm <- emmeans(
    object = ldl_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  ldl_emm_df <- as.data.frame(summary(ldl_emm))
  ldl_list[[i]] <- ldl_emm_df
  
  
  hdl_mod <- svyglm(hdl ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  hdl_emm <- emmeans(
    object = hdl_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  hdl_emm_df <- as.data.frame(summary(hdl_emm))
  hdl_list[[i]] <- hdl_emm_df
  
  
  tgl_mod <- svyglm(triglyceride ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  tgl_emm <- emmeans(
    object = tgl_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  tgl_emm_df <- as.data.frame(summary(tgl_emm))
  tgl_list[[i]] <- tgl_emm_df
  
  
  tc_mod <- svyglm(total_cholesterol ~ race_eth + dm_sex_age, design = nhanes_total_svy)
  tc_emm <- emmeans(
    object = tc_mod, 
    specs  = ~ dm_sex_age, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  tc_emm_df <- as.data.frame(summary(tc_emm))
  tc_list[[i]] <- tc_emm_df
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_sex_age) %>%
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
    mutate(estimate = paste0(round(theta_D, 2), " \t (",
                        round(L, 2), ", ",
                        round(U, 2), ")"))
  
  # save 2 digits for A1c
  # mutate(estimate = paste0(round(theta_D, 2, " \t (",
  #                          round(L, 2), ", ",
  #                          round(U, 2), ")"))

  return(pooled_results)
}


all_results <- bind_rows(
  pool_ad(bmi_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Fat percentage"), 
  pool_ad(visfat_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Visceral fat mass"), 
  pool_ad(subfat_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Subcutaneous fat mass"), 
  pool_ad(wt_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Waistcircumference"),
  pool_ad(whtr_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Waist-to-Height Ratio"),
  pool_ad(fpg_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Fasting glucose"), 
  pool_ad(a1c_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Glycohemoglobin"), 
  pool_ad(sbp_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "SBP"), 
  pool_ad(dbp_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "DBP"), 
  pool_ad(ldl_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "LDL"), 
  pool_ad(hdl_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "HDL"), 
  pool_ad(tgl_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Triglyceride"), 
  pool_ad(tc_list) %>% select(dm_sex_age, estimate) %>% mutate(variable = "Total cholesterol")
) %>% 
  pivot_wider(names_from = dm_sex_age, values_from = estimate, names_sort = TRUE) %>% 
  write_csv(., "complete cases/analysis/dbwse03h_descriptive characteristics by dm, sex and age group.csv")
  



#------------------------------------------------------------------------------------
# continuous variables: Survey-weighted ANOVA/Wald test
# categorical variables: Rao–Scott chi-square

nhanes_all <- do.call(rbind, lapply(nhanes_svy_dfs, function(design_obj) design_obj$variables))

# Create age_group variable
nhanes_all <- nhanes_all %>%
  mutate(age_group = ifelse(age < 45, "<45", ">=45"))

nhanes_all_svy <- nhanes_all %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG") %>% 
  mutate(
    dm_sex_age = case_when(
      female == 1 & dm == "NoDM" & age_group == "<45" ~ "female NoDM <45",
      female == 0 & dm == "NoDM" & age_group == "<45" ~ "male NoDM <45",
      female == 1 & dm == "PreDM" & age_group == "<45" ~ "female PreDM <45",
      female == 0 & dm == "PreDM" & age_group == "<45" ~ "male PreDM <45",
      female == 1 & dm == "DM" & age_group == "<45" ~ "female DM <45",
      female == 0 & dm == "DM" & age_group == "<45" ~ "male DM <45",
      female == 1 & dm == "NoDM" & age_group == ">=45" ~ "female NoDM >=45",
      female == 0 & dm == "NoDM" & age_group == ">=45" ~ "male NoDM >=45",
      female == 1 & dm == "PreDM" & age_group == ">=45" ~ "female PreDM >=45",
      female == 0 & dm == "PreDM" & age_group == ">=45" ~ "male PreDM >=45",
      female == 1 & dm == "DM" & age_group == ">=45" ~ "female DM >=45",
      female == 0 & dm == "DM" & age_group == ">=45" ~ "male DM >=45"
    )
  )

bmi_test <- svyglm(bmi ~ dm_sex_age, design = nhanes_all_svy)
fat_test <- svyglm(fat_percentage ~ dm_sex_age, design = nhanes_all_svy)
visfat_test <- svyglm(visceral_fat ~ dm_sex_age, design = nhanes_all_svy)
subfat_test <- svyglm(subcutaneous_fat ~ dm_sex_age, design = nhanes_all_svy)
wt_test <- svyglm(waistcircumference ~ dm_sex_age, design = nhanes_all_svy)
whtr_test <- svyglm(WHtR ~ dm_sex_age, design = nhanes_all_svy)
fpg_test <- svyglm(fasting_glucose ~ dm_sex_age, design = nhanes_all_svy)
a1c_test <- svyglm(glycohemoglobin ~ dm_sex_age, design = nhanes_all_svy)
sbp_test <- svyglm(sbp ~ dm_sex_age, design = nhanes_all_svy)
dbp_test <- svyglm(dbp ~ dm_sex_age, design = nhanes_all_svy)
ldl_test <- svyglm(ldl ~ dm_sex_age, design = nhanes_all_svy)
hdl_test <- svyglm(hdl ~ dm_sex_age, design = nhanes_all_svy)
tgl_test <- svyglm(triglyceride ~ dm_sex_age, design = nhanes_all_svy)
tc_test <- svyglm(total_cholesterol ~ dm_sex_age, design = nhanes_all_svy)

bmi_result <- regTermTest(bmi_test, ~ dm_sex_age)
fat_result <- regTermTest(fat_test, ~ dm_sex_age)
visfat_result <- regTermTest(visfat_test, ~ dm_sex_age)
subfat_result <- regTermTest(subfat_test, ~ dm_sex_age)
wt_result <- regTermTest(wt_test, ~ dm_sex_age)
whtr_result <- regTermTest(whtr_test, ~ dm_sex_age)
fpg_result <- regTermTest(fpg_test, ~ dm_sex_age)
a1c_result <- regTermTest(a1c_test, ~ dm_sex_age)
sbp_result <- regTermTest(sbp_test, ~ dm_sex_age)
dbp_result <- regTermTest(dbp_test, ~ dm_sex_age)
ldl_result <- regTermTest(ldl_test, ~ dm_sex_age)
hdl_result <- regTermTest(hdl_test, ~ dm_sex_age)
tgl_result <- regTermTest(tgl_test, ~ dm_sex_age)
tc_result <- regTermTest(tc_test, ~ dm_sex_age)


# check sample size -----------------------------------------------

# Use only the FIRST imputation for unweighted counts (since all imputations have same n, just different imputed values)
nhanes_first_imp <- nhanes_svy_dfs[[1]]$variables

# Unweighted sample sizes
unweighted_n <- nhanes_first_imp %>%
  mutate(age_group = ifelse(age < 45, "<45", ">=45")) %>%
  mutate(
    dm_sex_age = case_when(
      female == 1 & dm == "NoDM" & age_group == "<45" ~ "female NoDM <45",
      female == 0 & dm == "NoDM" & age_group == "<45" ~ "male NoDM <45",
      female == 1 & dm == "PreDM" & age_group == "<45" ~ "female PreDM <45",
      female == 0 & dm == "PreDM" & age_group == "<45" ~ "male PreDM <45",
      female == 1 & dm == "DM" & age_group == "<45" ~ "female DM <45",
      female == 0 & dm == "DM" & age_group == "<45" ~ "male DM <45",
      female == 1 & dm == "NoDM" & age_group == ">=45" ~ "female NoDM >=45",
      female == 0 & dm == "NoDM" & age_group == ">=45" ~ "male NoDM >=45",
      female == 1 & dm == "PreDM" & age_group == ">=45" ~ "female PreDM >=45",
      female == 0 & dm == "PreDM" & age_group == ">=45" ~ "male PreDM >=45",
      female == 1 & dm == "DM" & age_group == ">=45" ~ "female DM >=45",
      female == 0 & dm == "DM" & age_group == ">=45" ~ "male DM >=45"
    )
  ) %>%
  group_by(dm_sex_age) %>%
  summarise(n_unweighted = n(), .groups = 'drop') %>%
  arrange(dm_sex_age)


# Weighted sample sizes (use nhanes_all_svy which already pools all imputations)
weighted_n <- svytotal(~dm_sex_age, nhanes_all_svy) %>%
  as.data.frame() %>%
  rownames_to_column(var = "dm_sex_age") %>%
  mutate(dm_sex_age = gsub("dm_sex_age", "", dm_sex_age)) %>%
  rename(n_weighted = total, se_weighted = SE) %>%
  arrange(dm_sex_age)

# Combined table
sample_size_table <- left_join(unweighted_n, weighted_n, by = "dm_sex_age") 
