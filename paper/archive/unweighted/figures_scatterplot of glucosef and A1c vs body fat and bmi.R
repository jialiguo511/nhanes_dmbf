rm(list=ls());gc();source(".Rprofile")

source_df <- readRDS(paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds")) %>% 
  mutate(glucosef_category = case_when(fasting_glucose < 100 ~ "<100",
                                       fasting_glucose >= 100 & fasting_glucose <= 126 ~ "100-126",
                                       fasting_glucose > 126 & fasting_glucose <= 200 ~ "126-200",
                                       TRUE ~ ">200"),
         hba1c_category = case_when(glycohemoglobin < 5.7 ~ "<5.7%",
                                    glycohemoglobin >= 5.7 & glycohemoglobin <= 6.5 ~ "5.7%-6.5%",
                                    glycohemoglobin > 6.5 & glycohemoglobin <= 9 ~ "6.5%-9%",
                                    TRUE ~ ">9%")) %>% 
  mutate(glucosef_category = factor(glucosef_category, levels = c("<100", "100-126", "126-200", ">200")),
    hba1c_category = factor(hba1c_category, levels = c("<5.7%", "5.7%-6.5%", "6.5%-9%", ">9%")))

library(patchwork)

# Define each plot
plot1 <- source_df %>%
  ggplot(aes(x = fat_percentage, y = fasting_glucose)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "Fat Percentage (%)", y = "Fasting Glucose (mg/dL)", title = "Fasting Glucose vs Fat Percentage") +
  theme_minimal()

plot2 <- source_df %>%
  ggplot(aes(x = fat_percentage, y = glycohemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "Fat Percentage (%)", y = "HbA1c (%)", title = "HbA1c vs Fat Percentage") +
  theme_minimal()

plot3 <- source_df %>%
  ggplot(aes(x = bmi, y = fasting_glucose)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "BMI (kg/m^2)", y = "Fasting Glucose (mg/dL)", title = "Fasting Glucose vs BMI") +
  theme_minimal()

plot4 <- source_df %>%
  ggplot(aes(x = bmi, y = glycohemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "BMI (kg/m^2)", y = "HbA1c (%)", title = "HbA1c vs BMI") +
  theme_minimal()

# Combine plots
combined_plot <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2, nrow = 2) # Arrange in 2 columns and 2 rows



plot1 = ggplot(source_df, aes(y = glucosef_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 = ggplot(source_df, aes(y = glucosef_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 = ggplot(source_df, aes(y = hba1c_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 = ggplot(source_df, aes(y = hba1c_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2, nrow = 2) # Arrange in 2 columns and 2 rows











