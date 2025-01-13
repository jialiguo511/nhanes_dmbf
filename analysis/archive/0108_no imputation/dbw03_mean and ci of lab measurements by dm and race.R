rm(list=ls());gc();source(".Rprofile")

library(purrr)

nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  dplyr::filter(race_eth != "Other Race") %>% 
  mutate(
    dm_race = case_when(race_eth == "NH White" & dm == "non-diabetes" ~ "NHWhite non-diabetes",
                        race_eth == "NH Black" & dm == "non-diabetes" ~ "NHBlack non-diabetes",
                        race_eth == "Hispanic" & dm == "non-diabetes" ~ "Hispanic non-diabetes",
                        race_eth == "Asian" & dm == "non-diabetes" ~ "Asian non-diabetes",
                        race_eth == "NH White" & dm == "newly and undiagnosed diabetes" ~ "NHWhite newly and undiagnosed diabetes",
                        race_eth == "NH Black" & dm == "newly and undiagnosed diabetes" ~ "NHBlack newly and undiagnosed diabetes",
                        race_eth == "Hispanic" & dm == "newly and undiagnosed diabetes" ~ "Hispanic newly and undiagnosed diabetes",
                        race_eth == "Asian" & dm == "newly and undiagnosed diabetes" ~ "Asian newly and undiagnosed diabetes",
                        race_eth == "NH White" & dm == "diagnosed diabetes >1y" ~ "NHWhite diagnosed diabetes >1y",
                        race_eth == "NH Black" & dm == "diagnosed diabetes >1y" ~ "NHBlack diagnosed diabetes >1y",
                        race_eth == "Hispanic" & dm == "diagnosed diabetes >1y" ~ "Hispanic diagnosed diabetes >1y",
                        race_eth == "Asian" & dm == "diagnosed diabetes >1y" ~ "Asian diagnosed diabetes >1y"),
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

#----------------------------------------------------------------------------------------------------------------
# adjusted for age and sex
# 
# adjusted_means <- function(variables, design, group_var, covariates, output_file) {
#   combined_results <- data.frame()
#   
#   for (var in variables) {
#     # Fit the GLM
#     formula <- as.formula(paste(var, "~", paste(covariates, collapse = " + ")))
#     model <- svyglm(formula, design = design)
#     
#     # Predict values adjusted for covariates
#     predicted_values <- predict(model, type = "response")
#     
#     # Add predicted values to the design object
#     design$variables[[paste0("predicted_", var)]] <- predicted_values
#     
#     # Calculate adjusted means and standard errors by group
#     adjusted_means <- svyby(
#       as.formula(paste0("~predicted_", var)), 
#       as.formula(paste0("~", group_var)), 
#       design, 
#       svymean, 
#       na.rm = TRUE
#     )
#     
#     # Compute confidence intervals and format the output with one decimal
#     adjusted_means <- adjusted_means %>%
#       mutate(
#         mean_ci = paste0(round(get(paste0("predicted_", var)), 1), 
#                          " (", round(get(paste0("predicted_", var)) - 1.96 * se, 1), 
#                          ", ", round(get(paste0("predicted_", var)) + 1.96 * se, 1), ")")
#       ) %>%
#       select(!!sym(group_var), mean_ci) %>%
#       pivot_wider(names_from = !!sym(group_var), values_from = mean_ci) %>%
#       mutate(variable = var) %>%
#       select(variable, everything())
#     
#     # Combine results
#     combined_results <- bind_rows(combined_results, adjusted_means)
#   }
#   
#   # Export the combined results to an Excel file
#   write_csv(combined_results, output_file)
#   
#   return(combined_results)
# }
# 
# 
# variables <- c("bmi", "fat_percentage")
# covariates <- c("age", "female")
# group_var <- "dm_race"
# output_file <- "analysis/mean and ci of labs by dm and race.csv"
# 
# adjusted_results_combined <- adjusted_means(variables, nhanes_total_svy, group_var, covariates, output_file)
# 
# variables <- c("bmi", "fat_percentage")
# covariates <- c("age")
# group_var <- "dm_race_sex"
# output_file <- "analysis/mean and ci of labs by dm, race and sex.csv"
# 
# adjusted_results_combined <- adjusted_means(variables, nhanes_total_svy, group_var, covariates, output_file)

#---------------------------------------------------------------------------------------------------------------------------------
# by race

bmi_adjusted <- svyglm(bmi ~ age + female, design = nhanes_total_svy)
predicted_bmi <- predict(bmi_adjusted, type = "response")
nhanes_total_svy$predicted_bmi <- predicted_bmi
adjusted_mean_bmi <- svyby(~predicted_bmi, ~dm_race, nhanes_total_svy, svymean, na.rm = TRUE)
mean_bmi_race <- adjusted_mean_bmi %>%
  mutate(
    CI_lower = predicted_bmi - 1.96 * se,
    CI_upper = predicted_bmi + 1.96 * se
  ) %>% 
  separate(dm_race, into = c("race_eth", "dm"), sep = " ", extra = "merge") %>% 
  dplyr::select(-se) %>% 
  rename(estimate = predicted_bmi)



fat_adjusted <- svyglm(fat_percentage ~ age + female, design = nhanes_total_svy)
predicted_fat <- predict(fat_adjusted, type = "response")
nhanes_total_svy$predicted_fat <- predicted_fat
adjusted_mean_fat <- svyby(~predicted_fat, ~dm_race, nhanes_total_svy, svymean, na.rm = TRUE)
mean_fat_race <- adjusted_mean_fat %>%
  mutate(
    CI_lower = predicted_fat - 1.96 * se,
    CI_upper = predicted_fat + 1.96 * se
  ) %>% 
  separate(dm_race, into = c("race_eth", "dm"), sep = " ", extra = "merge") %>% 
  dplyr::select(-se) %>% 
  rename(estimate = predicted_fat)

# by race and sex

bmi_adjusted <- svyglm(bmi ~ age, design = nhanes_total_svy)
predicted_bmi <- predict(bmi_adjusted, type = "response")
nhanes_total_svy$predicted_bmi <- predicted_bmi
adjusted_mean_bmi <- svyby(~predicted_bmi, ~dm_race_sex, nhanes_total_svy, svymean, na.rm = TRUE)
mean_bmi_racesex <- adjusted_mean_bmi %>%
  mutate(
    CI_lower = predicted_bmi - 1.96 * se,
    CI_upper = predicted_bmi + 1.96 * se
  ) %>% 
  separate(dm_race_sex, into = c("sex", "race_eth", "dm"), sep = " ", extra = "merge") %>% 
  dplyr::select(-se) %>% 
  rename(estimate = predicted_bmi)


fat_adjusted <- svyglm(fat_percentage ~ age, design = nhanes_total_svy)
predicted_fat <- predict(fat_adjusted, type = "response")
nhanes_total_svy$predicted_fat <- predicted_fat
adjusted_mean_fat <- svyby(~predicted_fat, ~dm_race_sex, nhanes_total_svy, svymean, na.rm = TRUE)
mean_fat_racesex <- adjusted_mean_fat %>%
  mutate(
    CI_lower = predicted_fat - 1.96 * se,
    CI_upper = predicted_fat + 1.96 * se
  ) %>% 
  separate(dm_race_sex, into = c("sex", "race_eth", "dm"), sep = " ", extra = "merge") %>% 
  dplyr::select(-se) %>% 
  rename(estimate = predicted_fat)











