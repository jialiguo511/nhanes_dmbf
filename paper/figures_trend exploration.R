rm(list=ls());gc();source(".Rprofile")

nhanes_20112018_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  mutate(sex = case_when(female == 1 ~ "Female",
                         TRUE ~ "Male")) %>% 
  mutate(glucosef_category = case_when(fasting_glucose < 100 ~ "<100",
                                       fasting_glucose >= 100 & fasting_glucose <= 126 ~ "100-126",
                                       fasting_glucose > 126 & fasting_glucose <= 200 ~ "126-200",
                                       TRUE ~ ">200"),
         hba1c_category = case_when(glycohemoglobin < 5.7 ~ "<5.7%",
                                    glycohemoglobin >= 5.7 & glycohemoglobin <= 6.5 ~ "5.7%-6.5%",
                                    glycohemoglobin > 6.5 & glycohemoglobin <= 9 ~ "6.5%-9%",
                                    TRUE ~ ">9%")) %>% 
  mutate(glucosef_category = factor(glucosef_category, levels = c("<100", "100-126", "126-200", ">200")),
         hba1c_category = factor(hba1c_category, levels = c("<5.7%", "5.7%-6.5%", "6.5%-9%", ">9%")),
         bmi_category = factor(bmi_category, levels = c("<18.5", "18.5-24.9", "25-29.9", "30-39.9", ">= 40")))

#------------------------------------------------------------------------------------------------------------------------
# trend of mean BF% and BMI

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

# Plotting both BMI and Fat Percentage in one figure with facets
fig <- ggplot(data = combined_stats, aes(x = year, y = value, color = dm)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  facet_wrap(~ type, scales = "free_y") +  # Using facet_wrap to separate BMI and Fat Percentage
  labs(title = "Annual BMI and Fat Percentage by Diabetes Status",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))  # Change colors as needed


fig1 <- ggplot(data = combined_stats, aes(x = year, y = value, color = dm, shape = type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "Annual BMI and Fat Percentage by Diabetes Status",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) + # Change colors as needed
  scale_shape_manual(values = c(16, 17)) 

ggsave(fig1,filename=paste0(path_nhanes_dmbf_folder, "/figures/lineplot of BMI and BF by dm and year.png"),width=8, height = 6)

#---------------------------------------------------------------------------------------------------------------------------------
# trend of BF% by dm and sex
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
  labs(title = "Mean Fat Percentage by Sex and Diabetes Status",
       x = "Sex",
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) 

ggsave(fig2,filename=paste0(path_nhanes_dmbf_folder, "/figures/lineplot of BF by dm and sex.png"),width=7, height = 5)

#-------------------------------------------------------------------------------------------------------------------------------
# scatter plots of FPG, A1c vs BMI, BF%
data_points <- as.data.frame(nhanes_20112018_svy)
data_points <- data_points %>% 
  dplyr::filter(!is.na(fasting_glucose) & !is.na(bmi))

library("plotrix")



fig3 <- ggplot(data = data_points, aes(x = bmi, y = fasting_glucose)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(breaks = breaks, labels = labels,
                     limits = c(0, max(data_points$fasting_glucose, na.rm = TRUE))) +
  labs(title = "Scatterplot of Fasting Glucose vs BMI",
       x = "BMI (kg/m^2)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"), 
        axis.title.y = element_text(size = 12, face = "bold"))


zoomed_plot <- ggplot(data = data_points, aes(x = bmi, y = fasting_glucose)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(limits = c(100, 200), breaks = seq(100, 200, by = 10)) +
  labs(title = "Zoomed Scatterplot (100-200 mg/dL)",
       subtitle = "Focus on glucose levels 100-200 mg/dL",
       x = "BMI (kg/m^2)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()


#-------------------------------------------------------------------------------------------------------------------------------
# boxplot of FPG, A1c vs BF%, BMI

library(patchwork)

plot1 = ggplot(nhanes_20112018_svy, aes(y = glucosef_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 = ggplot(nhanes_20112018_svy, aes(y = glucosef_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 = ggplot(nhanes_20112018_svy, aes(y = hba1c_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 = ggplot(nhanes_20112018_svy, aes(y = hba1c_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2, nrow = 2)

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/boxplot of FPG, A1c and BF, BMI.png"),width=8, height = 6)

#--------------------------------------------------------------------------------------------------------------------------------------
# boxplot of FPG, A1c vs BF%, BMI -- newly diagnosed, undiagnosed dm + non-dm
source("analysis/dbw01_new dm weighted sample.R")

nhanes_newdm_svy <- nhanes_newdm_svy %>% 
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

plot1 = ggplot(nhanes_newdm_svy, aes(y = glucosef_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 = ggplot(nhanes_newdm_svy, aes(y = glucosef_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 = ggplot(nhanes_newdm_svy, aes(y = hba1c_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 = ggplot(nhanes_newdm_svy, aes(y = hba1c_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2, nrow = 2)

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/boxplot of FPG, A1c and BF, BMI newdm.png"),width=8, height = 6)

#--------------------------------------------------------------------------------------------------------------------------------------
# boxplot of FPG, A1c vs BF%, BMI - undiagnosed dm + non-dm
source("analysis/dbw01_new dm weighted sample.R")

nhanes_undm_svy <- nhanes_undm_svy %>% 
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

plot1 = ggplot(nhanes_undm_svy, aes(y = glucosef_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 = ggplot(nhanes_undm_svy, aes(y = glucosef_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "Fasting Glucose (mg/dL)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 = ggplot(nhanes_undm_svy, aes(y = hba1c_category, x = fat_percentage)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "Fat Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 = ggplot(nhanes_undm_svy, aes(y = hba1c_category, x = bmi)) +
  geom_boxplot() +
  labs(y = "HbA1c (%)", x = "BMI (kg/m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2, nrow = 2)

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/boxplot of FPG, A1c and BF, BMI undm.png"),width=8, height = 6)

#--------------------------------------------------------------------------------------------------------------------------------------
# scatter plot of BMI and BF% by sex
scatterplot <- ggplot(nhanes_20112018_svy, aes(x = bmi, y = fat_percentage)) +
  geom_point(aes(color = sex), alpha = 0.3) +
  geom_smooth(aes(color = sex), method = "lm",size = 1.2, se = FALSE) +  # Separate lines without confidence intervals for clarity
  labs(title = "Scatterplot of BMI vs Body Fat Percentage with Linear Fit by Sex",
       x = "BMI (kg/m²)",
       y = "Body Fat Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(scatterplot, filename=paste0(path_nhanes_dmbf_folder, "/figures/Scatterplot of BMI vs Body Fat Percentage by Sex.png"),width=8, height = 6)

#--------------------------------------------------------------------------------------------------------------------------------------
# mis-classification in BMI and BF% -- bar chart 1

nhanes_males_svy <- nhanes_20112018_svy %>% 
  dplyr::filter(female == 0) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 14 ~ "<=14%",
    fat_percentage > 14 & fat_percentage <= 20 ~ "14-20%",
    fat_percentage > 20 & fat_percentage <= 25 ~ "20-25%",
    fat_percentage > 25 ~ ">=25%",
    TRUE ~ "Undefined")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=14%","14-20%","20-25%",">=25%","Undefined")))
nhanes_females_svy <- nhanes_20112018_svy %>% 
  dplyr::filter(female == 1) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 25 ~ "<=25%",
    fat_percentage > 25 & fat_percentage <= 30 ~ "25-30%",
    fat_percentage > 30 & fat_percentage <= 35 ~ "30-35%",
    fat_percentage > 35 ~ ">=35%",
    TRUE ~ "Undefined")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=25%","25-30%","30-35%",">=35%","Undefined")))

male_table <- svytable(~bmi_category + fat_percentage_category, nhanes_males_svy)
male_proportions <- prop.table(male_table, margin = 1) * 100
male_df <- as.data.frame(male_proportions)
names(male_df) <- c("BMI Category", "Fat Percentage Category", "Percentage")

female_table <- svytable(~bmi_category + fat_percentage_category, nhanes_females_svy)
female_proportions <- prop.table(female_table, margin = 1) * 100
female_df <- as.data.frame(female_proportions)
names(female_df) <- c("BMI Category", "Fat Percentage Category", "Percentage")

male_plot <- ggplot(male_df, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Male BMI vs. Fat Percentage Categories",
       x = "BMI Category",
       y = "Percentage",
       fill = "Fat Percentage Category") +
  theme_minimal()

# Plot for females
female_plot <- ggplot(female_df, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Female BMI vs. Fat Percentage Categories",
       x = "BMI Category",
       y = "Percentage",
       fill = "Fat Percentage Category") +
  theme_minimal()


library(gridExtra)

fig <- grid.arrange(male_plot, female_plot, nrow = 2)

ggsave(fig, filename=paste0(path_nhanes_dmbf_folder, "/figures/Bar Chart of BMI vs Body Fat Percentage by Sex.png"),width=8, height = 9)

#-------------------------------------------------------------------------------------------------------------------------------------
# mis-classification in BMI and BF% -- bar chart 2

male_plot <- ggplot(data = nhanes_males_svy,
                    aes(x = bmi_category, y = ..prop.., fill = fat_percentage_category, group = fat_percentage_category)) +
  geom_bar(stat = "count", position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Male BMI vs. Fat Percentage Categories",
       x = "BMI Category",
       y = "Fat Percentage (%)",
       fill = "Fat Percentage Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

female_plot <- ggplot(data = nhanes_females_svy,
                    aes(x = bmi_category, y = ..prop.., fill = fat_percentage_category, group = fat_percentage_category)) +
  geom_bar(stat = "count", position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Female BMI vs. Fat Percentage Categories",
       x = "BMI Category",
       y = "Fat Percentage (%)",
       fill = "Fat Percentage Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(gridExtra)

fig <- grid.arrange(male_plot, female_plot, nrow = 2)

ggsave(fig, filename=paste0(path_nhanes_dmbf_folder, "/figures/Bar Chart of BMI vs Body Fat Percentage by gender.png"),width=8, height = 9)












