rm(list=ls());gc();source(".Rprofile")

nhanes_20112018_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(sex = case_when(female == 1 ~ "Female",
                         TRUE ~ "Male"))

# trend of mean and CI of BF% and BMI

bmi_stats <- svyby(~ bmi, ~ year + dm, nhanes_20112018_svy, svymean, na.rm = TRUE)
bmi_stats$se <- SE(bmi_stats)
bmi_stats$lower_ci <- bmi_stats$bmi - 1.96 * bmi_stats$se
bmi_stats$upper_ci <- bmi_stats$bmi + 1.96 * bmi_stats$se

fat_percentage_stats <- svyby(~ fat_percentage, ~year + dm, nhanes_20112018_svy, svymean, na.rm = TRUE)
fat_percentage_stats$se <- SE(fat_percentage_stats)
fat_percentage_stats$lower_ci <- fat_percentage_stats$fat_percentage - 1.96 * fat_percentage_stats$se
fat_percentage_stats$upper_ci <- fat_percentage_stats$fat_percentage + 1.96 * fat_percentage_stats$se


library(ggplot2)

bmi_stats <- bmi_stats %>%
  rename(value = bmi) %>%
  mutate(type = "BMI")

fat_percentage_stats <- fat_percentage_stats %>%
  rename(value = fat_percentage) %>%
  mutate(type = "Fat Percentage")

# Combining the datasets
combined_stats <- rbind(bmi_stats, fat_percentage_stats)

# Plotting both BMI and Fat Percentage in two figures
fig <- ggplot(data = combined_stats, aes(x = year, y = value, color = dm)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_wrap(~ type, scales = "free_y") +  # Using facet_wrap to separate BMI and Fat Percentage
  labs(title = "Annual BMI and Fat Percentage by Diabetes Status",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))  # Change colors as needed

# Plotting both BMI and Fat Percentage in one figure with facets
fig1 <- ggplot(data = combined_stats, aes(x = year, y = value, color = dm, shape = type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(x = "Year",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue")) + # Change colors as needed
  scale_shape_manual(values = c(16, 17)) 

ggsave(fig1,filename=paste0(path_nhanes_dmbf_folder, "/figures/lineplot of BMI and BF by dm and year.png"),width=8, height = 6)

#---------------------------------------------------------------------------------------------------------------------------------
# trend of mean and CI of BF% by dm and sex
fat_percentage_stats <- svyby(~ fat_percentage, ~ sex + dm, nhanes_20112018_svy, svymean, na.rm = TRUE)
fat_percentage_stats$se <- SE(fat_percentage_stats)
fat_percentage_stats$lower_ci <- fat_percentage_stats$fat_percentage - 1.96 * fat_percentage_stats$se
fat_percentage_stats$upper_ci <- fat_percentage_stats$fat_percentage + 1.96 * fat_percentage_stats$se

fat_percentage_stats <- fat_percentage_stats %>%
  rename(value = fat_percentage) %>%
  mutate(type = "Fat Percentage")


fig2 <- ggplot(data = fat_percentage_stats, aes(x = sex, y = value, color = dm)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_wrap(~ type, scales = "free_y") +  # Using facet_wrap to separate BMI and Fat Percentage
  labs(x = "Sex",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) 

ggsave(fig2,filename=paste0(path_nhanes_dmbf_folder, "/figures/lineplot of BF by dm and sex.png"),width=8, height = 6)
