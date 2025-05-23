rm(list=ls());gc();source(".Rprofile")

library(survey)
library(broom)
library(emmeans)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS")) 

bmi_list <- list()
fat_list <- list()


for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]] 
  
  nhanes_total_svy <- df %>% 
    dplyr::filter(race_eth != "Other Race") %>% 
    mutate(
      dm_race_sex = case_when(
        female == 1 & race_eth == "NH White" & dm == "non-diabetes" ~ "female NHWhite non-diabetes",
        female == 0 & race_eth == "NH White" & dm == "non-diabetes" ~ "male NHWhite non-diabetes",
        female == 1 & race_eth == "NH Black" & dm == "non-diabetes" ~ "female NHBlack non-diabetes",
        female == 0 & race_eth == "NH Black" & dm == "non-diabetes" ~ "male NHBlack non-diabetes",
        female == 1 & race_eth == "Hispanic" & dm == "non-diabetes" ~ "female Hispanic non-diabetes",
        female == 0 & race_eth == "Hispanic" & dm == "non-diabetes" ~ "male Hispanic non-diabetes",
        female == 1 & race_eth == "Asian" & dm == "non-diabetes" ~ "female Asian non-diabetes",
        female == 0 & race_eth == "Asian" & dm == "non-diabetes" ~ "male Asian non-diabetes",
        female == 1 & race_eth == "NH White" & dm == "newly and undiagnosed diabetes" ~ "female NHWhite newly and undiagnosed diabetes",
        female == 0 & race_eth == "NH White" & dm == "newly and undiagnosed diabetes" ~ "male NHWhite newly and undiagnosed diabetes",
        female == 1 & race_eth == "NH Black" & dm == "newly and undiagnosed diabetes" ~ "female NHBlack newly and undiagnosed diabetes",
        female == 0 & race_eth == "NH Black" & dm == "newly and undiagnosed diabetes" ~ "male NHBlack newly and undiagnosed diabetes",
        female == 1 & race_eth == "Hispanic" & dm == "newly and undiagnosed diabetes" ~ "female Hispanic newly and undiagnosed diabetes",
        female == 0 & race_eth == "Hispanic" & dm == "newly and undiagnosed diabetes" ~ "male Hispanic newly and undiagnosed diabetes",
        female == 1 & race_eth == "Asian" & dm == "newly and undiagnosed diabetes" ~ "female Asian newly and undiagnosed diabetes",
        female == 0 & race_eth == "Asian" & dm == "newly and undiagnosed diabetes" ~ "male Asian newly and undiagnosed diabetes",
        female == 1 & race_eth == "NH White" & dm == "diagnosed diabetes >1y" ~ "female NHWhite diagnosed diabetes >1y",
        female == 0 & race_eth == "NH White" & dm == "diagnosed diabetes >1y" ~ "male NHWhite diagnosed diabetes >1y",
        female == 1 & race_eth == "NH Black" & dm == "diagnosed diabetes >1y" ~ "female NHBlack diagnosed diabetes >1y",
        female == 0 & race_eth == "NH Black" & dm == "diagnosed diabetes >1y" ~ "male NHBlack diagnosed diabetes >1y",
        female == 1 & race_eth == "Hispanic" & dm == "diagnosed diabetes >1y" ~ "female Hispanic diagnosed diabetes >1y",
        female == 0 & race_eth == "Hispanic" & dm == "diagnosed diabetes >1y" ~ "male Hispanic diagnosed diabetes >1y",
        female == 1 & race_eth == "Asian" & dm == "diagnosed diabetes >1y" ~ "female Asian diagnosed diabetes >1y",
        female == 0 & race_eth == "Asian" & dm == "diagnosed diabetes >1y" ~ "male Asian diagnosed diabetes >1y"
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
    mutate(variable = "Fat percentage")
) %>% 
  separate(dm_race_sex, into = c("sex", "race_eth", "dm"), sep = " ", extra = "merge") %>% 
  rename(estimate = theta_D,
         CI_lower = L, 
         CI_upper = U) %>% 
  write_csv(., "analysis/dbw02c_descriptive characteristics by dm, sex and race.csv")


