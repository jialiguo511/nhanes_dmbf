rm(list=ls());gc();source(".Rprofile")

source("analysis/dbw01_weighted sample.R")
library(ggplot2)

nhanes_total_svy = nhanes_total %>% 
  dplyr::filter(!is.na(fat_percentage) & !is.na(bmi)) %>%
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG") %>% 
  mutate(bmi_category = case_when(bmi<18.5 ~ "<18.5",
                                  bmi>=18.5 & bmi<25 ~ "18.5-24.9",
                                  bmi>=25 & bmi<30 ~ "25-29.9",
                                  TRUE ~ ">=30"),
         bmi_category = factor(bmi_category, levels = c("<18.5", "18.5-24.9", "25-29.9", ">=30")))

nhanes_males_svy <- nhanes_total_svy %>% 
  dplyr::filter(female == 0) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 14 ~ "<=14%",
    fat_percentage > 14 & fat_percentage <= 20 ~ "14-20%",
    fat_percentage > 20 & fat_percentage <= 25 ~ "20-25%",
    TRUE ~ ">25%")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=14%","14-20%","20-25%",">25%")))

nhanes_males_svy_new <- nhanes_males_svy %>%
  dplyr::filter(dm == "newly and undiagnosed diabetes")
nhanes_males_svy_old <- nhanes_males_svy %>%
  dplyr::filter(dm == "diagnosed diabetes >1y")


nhanes_females_svy <- nhanes_total_svy %>% 
  dplyr::filter(female == 1) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 25 ~ "<=25%",
    fat_percentage > 25 & fat_percentage <= 30 ~ "25-30%",
    fat_percentage > 30 & fat_percentage <= 35 ~ "30-35%",
    TRUE ~ ">35%")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=25%","25-30%","30-35%",">35%")))

nhanes_females_svy_new <- nhanes_females_svy %>%
  dplyr::filter(dm == "newly and undiagnosed diabetes")
nhanes_females_svy_old <- nhanes_females_svy %>%
  dplyr::filter(dm == "diagnosed diabetes >1y")

#-----------------------------------------------------------------------------------------------------------------------------------
mean_fat_male_new <- svyby(~fat_percentage, ~bmi_category, nhanes_males_svy_new, svymean)
mean_fat_male_old <- svyby(~fat_percentage, ~bmi_category, nhanes_males_svy_old, svymean)
mean_fat_female_new <- svyby(~fat_percentage, ~bmi_category, nhanes_females_svy_new, svymean)
mean_fat_female_old <- svyby(~fat_percentage, ~bmi_category, nhanes_females_svy_old, svymean)

fat_prop_male_new <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_males_svy_new), margin = 1) * 100
fat_prop_male_old <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_males_svy_old), margin = 1) * 100
fat_prop_female_new <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_females_svy_new), margin = 1) * 100
fat_prop_female_old <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_females_svy_old), margin = 1) * 100

plot_data_male_new <- merge(as.data.frame(mean_fat_male_new), as.data.frame(fat_prop_male_new), by = "bmi_category")
plot_data_male_old <- merge(as.data.frame(mean_fat_male_old), as.data.frame(fat_prop_male_old), by = "bmi_category")
plot_data_female_new <- merge(as.data.frame(mean_fat_female_new), as.data.frame(fat_prop_female_new), by = "bmi_category")
plot_data_female_old <- merge(as.data.frame(mean_fat_female_old), as.data.frame(fat_prop_female_old), by = "bmi_category")

names(plot_data_male_new) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")
names(plot_data_male_old) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")
names(plot_data_female_new) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")
names(plot_data_female_old) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")


fig_male_new <- ggplot(data = plot_data_male_new, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "C. Male newly and undiagnosed diabetes", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=14%" = "green", "14-20%" = "lightblue", "20-25%" = "yellow", ">25%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.7, color = "black", size = 3.5)

fig_male_old <- ggplot(data = plot_data_male_old, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "D. Male diagnosis of diabetes >1y", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=14%" = "green", "14-20%" = "lightblue", "20-25%" = "yellow", ">25%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.7, color = "black", size = 3.5)

fig_female_new <- ggplot(data = plot_data_female_new, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "A. Female newly and undiagnosed diabetes", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=25%" = "green", "25-30%" = "lightblue", "30-35%" = "yellow", ">35%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.7, color = "black", size = 3.5)

fig_female_old <- ggplot(data = plot_data_female_old, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "B. Female diagnosis of diabetes >1y", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=25%" = "green", "25-30%" = "lightblue", "30-35%" = "yellow", ">35%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.7, color = "black", size = 3.5)


library(gridExtra)

fig_combined <- grid.arrange(fig_female_new, fig_female_old, fig_male_new, fig_male_old, nrow = 2, ncol = 2, heights = c(1, 1))

ggsave(fig_combined, filename=paste0(path_nhanes_dmbf_folder, "/figures/distribution of newdm body fat in bmi by sex.png"),width=14, height = 14)

