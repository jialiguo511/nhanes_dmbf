rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

nhanes_20112018_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(sex = case_when(female == 1 ~ "Female",
                         TRUE ~ "Male"))

svyplot(~bmi+fat_percentage, nhanes_20112018_svy, style="bubble")

# Fit survey-specific linear models for each gender
model_male <- svyglm(fat_percentage ~ bmi, design = subset(nhanes_20112018_svy, sex == "Male"))
model_female <- svyglm(fat_percentage ~ bmi, design = subset(nhanes_20112018_svy, sex == "Female"))

# Extract coefficients
coef_male <- coef(model_male)
coef_female <- coef(model_female)

# Create a new dataframe for the fitted line
bmi_range <- seq(min(nhanes_20112018_svy$variables$bmi, na.rm = TRUE), max(nhanes_20112018_svy$variables$bmi, na.rm = TRUE), by = 0.1)
fitted_male <- data.frame(bmi = bmi_range, fat_percentage = coef_male[1] + coef_male[2] * bmi_range, sex = "Male")
fitted_female <- data.frame(bmi = bmi_range, fat_percentage = coef_female[1] + coef_female[2] * bmi_range, sex = "Female")
fitted_data <- rbind(fitted_male, fitted_female)

# Plotting
fig <- ggplot(data = nhanes_20112018_svy$variables, aes(x = bmi, y = fat_percentage, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_line(data = fitted_data, aes(x = bmi, y = fat_percentage, color = sex)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(x = "BMI", y = "Fat Percentage",
       color = "Gender") +
  theme_minimal()

ggsave(fig, filename=paste0(path_nhanes_dmbf_folder, "/figures/scatterplot of bmi vs body fat by sex.png"),width=8, height = 6)
