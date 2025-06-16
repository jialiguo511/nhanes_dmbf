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

for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    dplyr::filter(race_eth != "Other Race") %>% 
    mutate(
      dm_race_sex = case_when(
        female == 1 & race_eth == "NH White" & dm == "NoDM" ~ "female NHWhite NoDM",
        female == 0 & race_eth == "NH White" & dm == "NoDM" ~ "male NHWhite NoDM",
        female == 1 & race_eth == "NH Black" & dm == "NoDM" ~ "female NHBlack NoDM",
        female == 0 & race_eth == "NH Black" & dm == "NoDM" ~ "male NHBlack NoDM",
        female == 1 & race_eth == "Hispanic" & dm == "NoDM" ~ "female Hispanic NoDM",
        female == 0 & race_eth == "Hispanic" & dm == "NoDM" ~ "male Hispanic NoDM",
        female == 1 & race_eth == "Asian" & dm == "NoDM" ~ "female Asian NoDM",
        female == 0 & race_eth == "Asian" & dm == "NoDM" ~ "male Asian NoDM",
        female == 1 & race_eth == "NH White" & dm == "PreDM" ~ "female NHWhite PreDM",
        female == 0 & race_eth == "NH White" & dm == "PreDM" ~ "male NHWhite PreDM",
        female == 1 & race_eth == "NH Black" & dm == "PreDM" ~ "female NHBlack PreDM",
        female == 0 & race_eth == "NH Black" & dm == "PreDM" ~ "male NHBlack PreDM",
        female == 1 & race_eth == "Hispanic" & dm == "PreDM" ~ "female Hispanic PreDM",
        female == 0 & race_eth == "Hispanic" & dm == "PreDM" ~ "male Hispanic PreDM",
        female == 1 & race_eth == "Asian" & dm == "PreDM" ~ "female Asian PreDM",
        female == 0 & race_eth == "Asian" & dm == "PreDM" ~ "male Asian PreDM",
        female == 1 & race_eth == "NH White" & dm == "DM" ~ "female NHWhite DM",
        female == 0 & race_eth == "NH White" & dm == "DM" ~ "male NHWhite DM",
        female == 1 & race_eth == "NH Black" & dm == "DM" ~ "female NHBlack DM",
        female == 0 & race_eth == "NH Black" & dm == "DM" ~ "male NHBlack DM",
        female == 1 & race_eth == "Hispanic" & dm == "DM" ~ "female Hispanic DM",
        female == 0 & race_eth == "Hispanic" & dm == "DM" ~ "male Hispanic DM",
        female == 1 & race_eth == "Asian" & dm == "DM" ~ "female Asian DM",
        female == 0 & race_eth == "Asian" & dm == "DM" ~ "male Asian DM"
      )
    ) 
  
  
  bmi_mod <- svyglm(bmi ~ age + dm_race_sex, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  bmi_emm <- emmeans(
    object = bmi_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  bmi_emm_df <- as.data.frame(summary(bmi_emm))
  bmi_list[[i]] <- bmi_emm_df
  
  
  fat_mod <- svyglm(fat_percentage ~ age + dm_race_sex, design = nhanes_total_svy)
  fat_emm <- emmeans(
    object = fat_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  fat_emm_df <- as.data.frame(summary(fat_emm))
  fat_list[[i]] <- fat_emm_df
  
  
  visfat_mod <- svyglm(visceral_fat ~ age + dm_race_sex, design = nhanes_total_svy)
  # Calculate marginal (adjusted) means of BMI by dm_sex
  visfat_emm <- emmeans(
    object = visfat_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  visfat_emm_df <- as.data.frame(summary(visfat_emm))
  visfat_list[[i]] <- visfat_emm_df
  
  
  subfat_mod <- svyglm(subcutaneous_fat ~ age + dm_race_sex, design = nhanes_total_svy)
  subfat_emm <- emmeans(
    object = subfat_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  subfat_emm_df <- as.data.frame(summary(subfat_emm))
  subfat_list[[i]] <- subfat_emm_df
  
  
  wt_mod <- svyglm(waistcircumference ~ age + dm_race_sex, design = nhanes_total_svy)
  wt_emm <- emmeans(
    object = wt_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  wt_emm_df <- as.data.frame(summary(wt_emm))
  wt_list[[i]] <- wt_emm_df
  
  
  whtr_mod <- svyglm(WHtR ~ age + dm_race_sex, design = nhanes_total_svy)
  whtr_emm <- emmeans(
    object = whtr_mod, 
    specs  = ~ dm_race_sex, 
    data   = nhanes_total_svy$variables  # <-- specify the underlying data
  )
  
  whtr_emm_df <- as.data.frame(summary(whtr_emm))
  whtr_list[[i]] <- whtr_emm_df
  
}


pool_ad <- function(results_list) {
  # Bind the results and calculate Rubin's rules
  pooled_results <- bind_rows(results_list) %>%
    mutate(W_d = SE^2) %>%
    group_by(dm_race_sex) %>%
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
  pool_ad(bmi_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "BMI"), 
  pool_ad(fat_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "Fat percentage"),
  pool_ad(visfat_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "Visceral fat mass"),
  pool_ad(subfat_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "Subcutaneous fat mass"),
  pool_ad(wt_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "Waist circumference"),
  pool_ad(whtr_list) %>% select(dm_race_sex, theta_D, L, U) %>% 
    mutate(variable = "Waist-to-Height ratio")
) %>% 
  separate(dm_race_sex, into = c("sex", "race_eth", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv")


