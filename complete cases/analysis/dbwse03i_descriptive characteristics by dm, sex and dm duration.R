rm(list=ls());gc();source(".Rprofile")

library(survey)
library(tidyverse)
library(broom)
library(emmeans)

# Set survey option to handle strata with only one PSU after filtering
options(survey.lonely.psu = "adjust")

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

#------------------------------------------------------------------------------------
# Remove those missing dm_age and create dm_duration
#------------------------------------------------------------------------------------

print("=== Creating dm_duration and removing missing dm_age ===")

for (i in 1:length(nhanes_svy_dfs)) {
  # Remove rows with missing dm_age first
  nhanes_svy_dfs[[i]]$variables <- nhanes_svy_dfs[[i]]$variables %>%
    dplyr::filter(!is.na(dm_age)) %>%
    mutate(dm_duration = ifelse(dm == "DM", age - dm_age, NA))
  
  # Update the survey design with the filtered data
  nhanes_svy_dfs[[i]] <- survey::svydesign(
    ids = ~psu,
    strata = ~pseudostratum,
    weights = ~nhanes2yweight,
    data = nhanes_svy_dfs[[i]]$variables,
    nest = TRUE
  )
}

# Check sample sizes after filtering
print("Sample sizes after removing missing dm_age:")
for (i in 1:length(nhanes_svy_dfs)) {
  n <- nrow(nhanes_svy_dfs[[i]]$variables)
  print(paste("Imputation", i, ": n =", n))
}

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
  
  # Create dm_duration groups (<5, 5-<10, >=10 years, or NA for non-DM)
  df_with_duration_group <- df$variables %>%
    mutate(
      dm_duration_group = case_when(
        dm == "NoDM" ~ "NoDM",
        dm == "PreDM" ~ "PreDM",
        dm == "DM" & dm_duration < 5 ~ "<5 years",
        dm == "DM" & dm_duration >= 5 & dm_duration < 10 ~ "5-<10 years",
        dm == "DM" & dm_duration >= 10 ~ ">=10 years",
        TRUE ~ NA_character_
      )
    )
  
  # Update the design object with dm_duration_group variable
  nhanes_total_svy <- df
  nhanes_total_svy$variables$dm_duration_group <- df_with_duration_group$dm_duration_group
  
  # Create dm_sex_duration variable
  nhanes_total_svy$variables <- nhanes_total_svy$variables %>%
    mutate(
      dm_sex_duration = case_when(
        female == 1 & dm_duration_group == "NoDM" ~ "female NoDM",
        female == 0 & dm_duration_group == "NoDM" ~ "male NoDM",
        female == 1 & dm_duration_group == "PreDM" ~ "female PreDM",
        female == 0 & dm_duration_group == "PreDM" ~ "male PreDM",
        female == 1 & dm_duration_group == "<5 years" ~ "female DM <5 years",
        female == 0 & dm_duration_group == "<5 years" ~ "male DM <5 years",
        female == 1 & dm_duration_group == "5-<10 years" ~ "female DM 5-<10 years",
        female == 0 & dm_duration_group == "5-<10 years" ~ "male DM 5-<10 years",
        female == 1 & dm_duration_group == ">=10 years" ~ "female DM >=10 years",
        female == 0 & dm_duration_group == ">=10 years" ~ "male DM >=10 years"
      )
    )
  
  bmi_mod <- svyglm(bmi ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  fat_mod <- svyglm(fat_percentage ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  visfat_mod <- svyglm(visceral_fat ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  visfat_emm <- emmeans(
    object = visfat_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  visfat_emm_df <- as.data.frame(summary(visfat_emm))
  visfat_list[[i]] <- visfat_emm_df
  
  subfat_mod <- svyglm(subcutaneous_fat ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  subfat_emm <- emmeans(
    object = subfat_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  subfat_emm_df <- as.data.frame(summary(subfat_emm))
  subfat_list[[i]] <- subfat_emm_df
  
  wt_mod <- svyglm(waistcircumference ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  wt_emm <- emmeans(
    object = wt_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  wt_emm_df <- as.data.frame(summary(wt_emm))
  wt_list[[i]] <- wt_emm_df
  
  whtr_mod <- svyglm(WHtR ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  whtr_emm <- emmeans(
    object = whtr_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  whtr_emm_df <- as.data.frame(summary(whtr_emm))
  whtr_list[[i]] <- whtr_emm_df
  
  fpg_mod <- svyglm(fasting_glucose ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  fpg_emm <- emmeans(
    object = fpg_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  fpg_emm_df <- as.data.frame(summary(fpg_emm))
  fpg_list[[i]] <- fpg_emm_df
  
  a1c_mod <- svyglm(glycohemoglobin ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  a1c_emm <- emmeans(
    object = a1c_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  a1c_emm_df <- as.data.frame(summary(a1c_emm))
  a1c_list[[i]] <- a1c_emm_df
  
  sbp_mod <- svyglm(sbp ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  sbp_emm <- emmeans(
    object = sbp_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  sbp_emm_df <- as.data.frame(summary(sbp_emm))
  sbp_list[[i]] <- sbp_emm_df
  
  dbp_mod <- svyglm(dbp ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  dbp_emm <- emmeans(
    object = dbp_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  dbp_emm_df <- as.data.frame(summary(dbp_emm))
  dbp_list[[i]] <- dbp_emm_df
  
  ldl_mod <- svyglm(ldl ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  ldl_emm <- emmeans(
    object = ldl_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  ldl_emm_df <- as.data.frame(summary(ldl_emm))
  ldl_list[[i]] <- ldl_emm_df
  
  hdl_mod <- svyglm(hdl ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  hdl_emm <- emmeans(
    object = hdl_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  hdl_emm_df <- as.data.frame(summary(hdl_emm))
  hdl_list[[i]] <- hdl_emm_df
  
  tgl_mod <- svyglm(triglyceride ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  tgl_emm <- emmeans(
    object = tgl_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  tgl_emm_df <- as.data.frame(summary(tgl_emm))
  tgl_list[[i]] <- tgl_emm_df
  
  tc_mod <- svyglm(total_cholesterol ~ age + race_eth + dm_sex_duration, design = nhanes_total_svy)
  tc_emm <- emmeans(
    object = tc_mod, 
    specs  = ~ dm_sex_duration, 
    data   = nhanes_total_svy$variables
  )
  tc_emm_df <- as.data.frame(summary(tc_emm))
  tc_list[[i]] <- tc_emm_df
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_sex_duration) %>%
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
  pool_ad(bmi_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Fat percentage"), 
  pool_ad(visfat_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Visceral fat mass"), 
  pool_ad(subfat_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Subcutaneous fat mass"), 
  pool_ad(wt_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Waistcircumference"),
  pool_ad(whtr_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Waist-to-Height Ratio"),
  pool_ad(fpg_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Fasting glucose"), 
  pool_ad(a1c_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Glycohemoglobin"), 
  pool_ad(sbp_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "SBP"), 
  pool_ad(dbp_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "DBP"), 
  pool_ad(ldl_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "LDL"), 
  pool_ad(hdl_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "HDL"), 
  pool_ad(tgl_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Triglyceride"), 
  pool_ad(tc_list) %>% select(dm_sex_duration, estimate) %>% mutate(variable = "Total cholesterol")
) %>% 
  pivot_wider(names_from = dm_sex_duration, values_from = estimate, names_sort = TRUE) %>% 
  write_csv(., "complete cases/analysis/dbwse03i_descriptive characteristics by dm, sex and dm duration.csv")


#------------------------------------------------------------------------------------
# Statistical tests across dm_sex_duration groups
#------------------------------------------------------------------------------------

nhanes_all <- do.call(rbind, lapply(nhanes_svy_dfs, function(design_obj) design_obj$variables))

# Create age_group variable and dm_duration_group
nhanes_all <- nhanes_all %>%
  mutate(
    dm_duration_group = case_when(
      dm == "NoDM" ~ "NoDM",
      dm == "PreDM" ~ "PreDM",
      dm == "DM" & dm_duration < 5 ~ "<5 years",
      dm == "DM" & dm_duration >= 5 & dm_duration < 10 ~ "5-<10 years",
      dm == "DM" & dm_duration >= 10 ~ ">=10 years",
      TRUE ~ NA_character_
    )
  )

nhanes_all_svy <- nhanes_all %>%
  as_survey_design(ids = psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight,
                   nest = TRUE,
                   pps = "brewer",
                   variance = "YG") %>% 
  mutate(
    dm_sex_duration = case_when(
      female == 1 & dm_duration_group == "NoDM" ~ "female NoDM",
      female == 0 & dm_duration_group == "NoDM" ~ "male NoDM",
      female == 1 & dm_duration_group == "PreDM" ~ "female PreDM",
      female == 0 & dm_duration_group == "PreDM" ~ "male PreDM",
      female == 1 & dm_duration_group == "<5 years" ~ "female DM <5 years",
      female == 0 & dm_duration_group == "<5 years" ~ "male DM <5 years",
      female == 1 & dm_duration_group == "5-<10 years" ~ "female DM 5-<10 years",
      female == 0 & dm_duration_group == "5-<10 years" ~ "male DM 5-<10 years",
      female == 1 & dm_duration_group == ">=10 years" ~ "female DM >=10 years",
      female == 0 & dm_duration_group == ">=10 years" ~ "male DM >=10 years"
    )
  )

bmi_test <- svyglm(bmi ~ dm_sex_duration, design = nhanes_all_svy)
fat_test <- svyglm(fat_percentage ~ dm_sex_duration, design = nhanes_all_svy)
visfat_test <- svyglm(visceral_fat ~ dm_sex_duration, design = nhanes_all_svy)
subfat_test <- svyglm(subcutaneous_fat ~ dm_sex_duration, design = nhanes_all_svy)
wt_test <- svyglm(waistcircumference ~ dm_sex_duration, design = nhanes_all_svy)
whtr_test <- svyglm(WHtR ~ dm_sex_duration, design = nhanes_all_svy)
fpg_test <- svyglm(fasting_glucose ~ dm_sex_duration, design = nhanes_all_svy)
a1c_test <- svyglm(glycohemoglobin ~ dm_sex_duration, design = nhanes_all_svy)
sbp_test <- svyglm(sbp ~ dm_sex_duration, design = nhanes_all_svy)
dbp_test <- svyglm(dbp ~ dm_sex_duration, design = nhanes_all_svy)
ldl_test <- svyglm(ldl ~ dm_sex_duration, design = nhanes_all_svy)
hdl_test <- svyglm(hdl ~ dm_sex_duration, design = nhanes_all_svy)
tgl_test <- svyglm(triglyceride ~ dm_sex_duration, design = nhanes_all_svy)
tc_test <- svyglm(total_cholesterol ~ dm_sex_duration, design = nhanes_all_svy)

bmi_result <- regTermTest(bmi_test, ~ dm_sex_duration)
fat_result <- regTermTest(fat_test, ~ dm_sex_duration)
visfat_result <- regTermTest(visfat_test, ~ dm_sex_duration)
subfat_result <- regTermTest(subfat_test, ~ dm_sex_duration)
wt_result <- regTermTest(wt_test, ~ dm_sex_duration)
whtr_result <- regTermTest(whtr_test, ~ dm_sex_duration)
fpg_result <- regTermTest(fpg_test, ~ dm_sex_duration)
a1c_result <- regTermTest(a1c_test, ~ dm_sex_duration)
sbp_result <- regTermTest(sbp_test, ~ dm_sex_duration)
dbp_result <- regTermTest(dbp_test, ~ dm_sex_duration)
ldl_result <- regTermTest(ldl_test, ~ dm_sex_duration)
hdl_result <- regTermTest(hdl_test, ~ dm_sex_duration)
tgl_result <- regTermTest(tgl_test, ~ dm_sex_duration)
tc_result <- regTermTest(tc_test, ~ dm_sex_duration)


#------------------------------------------------------------------------------------
# Check sample size
#------------------------------------------------------------------------------------

# Use only the FIRST imputation for unweighted counts (since all imputations have same n, just different imputed values)
nhanes_first_imp <- nhanes_svy_dfs[[1]]$variables

# Unweighted sample sizes
unweighted_n <- nhanes_first_imp %>%
  mutate(
    dm_duration_group = case_when(
      dm == "NoDM" ~ "NoDM",
      dm == "PreDM" ~ "PreDM",
      dm == "DM" & dm_duration < 5 ~ "<5 years",
      dm == "DM" & dm_duration >= 5 & dm_duration < 10 ~ "5-<10 years",
      dm == "DM" & dm_duration >= 10 ~ ">=10 years",
      TRUE ~ NA_character_
    ),
    dm_sex_duration = case_when(
      female == 1 & dm_duration_group == "NoDM" ~ "female NoDM",
      female == 0 & dm_duration_group == "NoDM" ~ "male NoDM",
      female == 1 & dm_duration_group == "PreDM" ~ "female PreDM",
      female == 0 & dm_duration_group == "PreDM" ~ "male PreDM",
      female == 1 & dm_duration_group == "<5 years" ~ "female DM <5 years",
      female == 0 & dm_duration_group == "<5 years" ~ "male DM <5 years",
      female == 1 & dm_duration_group == "5-<10 years" ~ "female DM 5-<10 years",
      female == 0 & dm_duration_group == "5-<10 years" ~ "male DM 5-<10 years",
      female == 1 & dm_duration_group == ">=10 years" ~ "female DM >=10 years",
      female == 0 & dm_duration_group == ">=10 years" ~ "male DM >=10 years"
    )
  ) %>%
  group_by(dm_sex_duration) %>%
  summarise(n_unweighted = n(), .groups = 'drop') %>%
  arrange(dm_sex_duration)


# Weighted sample sizes (use nhanes_all_svy which already pools all imputations)
weighted_n <- svytotal(~dm_sex_duration, nhanes_all_svy) %>%
  as.data.frame() %>%
  rownames_to_column(var = "dm_sex_duration") %>%
  mutate(dm_sex_duration = gsub("dm_sex_duration", "", dm_sex_duration)) %>%
  rename(n_weighted = total, se_weighted = SE) %>%
  arrange(dm_sex_duration)

# Combined table
sample_size_table <- left_join(unweighted_n, weighted_n, by = "dm_sex_duration")
