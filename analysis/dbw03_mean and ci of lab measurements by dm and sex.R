rm(list=ls());gc();source(".Rprofile")

library(purrr)

nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(
    dm_sex = case_when(female == 1 & dm == "non-diabetes" ~ "female non-diabetes",
                       female == 0 & dm == "non-diabetes" ~ "male non-diabetes",
                       female == 1 & dm == "newly and undiagnosed diabetes" ~ "female newly and undiagnosed diabetes",
                       female == 0 & dm == "newly and undiagnosed diabetes" ~ "male newly and undiagnosed diabetes",
                       female == 1 & dm == "diagnosed diabetes >1y" ~ "female diagnosed diabetes >1y",
                       female == 0 & dm == "diagnosed diabetes >1y" ~ "male diagnosed diabetes >1y")
  ) 


#----------------------------------------------------------------------------------------------------------------
# adjusted for age and race/eth

adjusted_means <- function(variables, design, group_var, covariates, output_file) {
  combined_results <- data.frame()
  
  for (var in variables) {
    # Fit the GLM
    formula <- as.formula(paste(var, "~", paste(covariates, collapse = " + ")))
    model <- svyglm(formula, design = design)
    
    # Predict values adjusted for covariates
    predicted_values <- predict(model, type = "response")
    
    # Add predicted values to the design object
    design$variables[[paste0("predicted_", var)]] <- predicted_values
    
    # Calculate adjusted means and standard errors by group
    adjusted_means <- svyby(
      as.formula(paste0("~predicted_", var)), 
      as.formula(paste0("~", group_var)), 
      design, 
      svymean, 
      na.rm = TRUE
    )
    
    # Compute confidence intervals and format the output with one decimal
    adjusted_means <- adjusted_means %>%
      mutate(
        mean_ci = paste0(round(get(paste0("predicted_", var)), 1), 
                         " (", round(get(paste0("predicted_", var)) - 1.96 * se, 1), 
                         ", ", round(get(paste0("predicted_", var)) + 1.96 * se, 1), ")")
      ) %>%
      select(!!sym(group_var), mean_ci) %>%
      pivot_wider(names_from = !!sym(group_var), values_from = mean_ci) %>%
      mutate(variable = var) %>%
      select(variable, everything())
    
    # Combine results
    combined_results <- bind_rows(combined_results, adjusted_means)
  }
  
  # Export the combined results to an Excel file
  write.csv(combined_results, output_file)
  
  return(combined_results)
}


variables <- c("bmi", "fat_percentage", "waistcircumference", "glycohemoglobin", "fasting_glucose", 
               "sbp", "dbp", "total_cholesterol", "hdl", "ldl", "triglyceride")
covariates <- c("age", "race_eth")
group_var <- "dm_sex"
output_file <- "analysis/mean and ci of labs by dm and sex.csv"

adjusted_results_combined <- adjusted_means(variables, nhanes_total_svy, group_var, covariates, output_file)

