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

nhanes_females_svy <- nhanes_total_svy %>% 
  dplyr::filter(female == 1) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 25 ~ "<=25%",
    fat_percentage > 25 & fat_percentage <= 30 ~ "25-30%",
    fat_percentage > 30 & fat_percentage <= 35 ~ "30-35%",
    TRUE ~ ">35%")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=25%","25-30%","30-35%",">35%")))

#--------------------------------------------------------------------------------------------------
mean_fat_male <- svyby(~fat_percentage, ~bmi_category, nhanes_males_svy, svymean)
mean_fat_female <- svyby(~fat_percentage, ~bmi_category, nhanes_females_svy, svymean)

fat_category_proportions_male <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_males_svy), margin = 1) * 100
fat_category_proportions_female <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_females_svy), margin = 1) * 100

plot_data_male <- merge(as.data.frame(mean_fat_male), as.data.frame(fat_category_proportions_male), by = "bmi_category")
plot_data_female <- merge(as.data.frame(mean_fat_female), as.data.frame(fat_category_proportions_female), by = "bmi_category")

names(plot_data_male) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")
names(plot_data_female) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")

fig_male <- ggplot(data = plot_data_male, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "B. Male", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=14%" = "green", "14-20%" = "lightblue", "20-25%" = "yellow", ">25%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.5, color = "black", size = 3.5)

fig_female <- ggplot(data = plot_data_female, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "A. Female", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("<=25%" = "green", "25-30%" = "lightblue", "30-35%" = "yellow", ">35%" = "red")) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  geom_text(aes(y = 100, label = sprintf("Mean BF: %.1f%%", `Mean Fat Percentage`)), vjust = -0.5, color = "black", size = 3.5)


library(gridExtra)

fig_combined <- grid.arrange(fig_female, fig_male, nrow = 2, heights = c(1, 1))

ggsave(fig_combined, filename=paste0(path_nhanes_dmbf_folder, "/figures/distribution of all population body fat in bmi by sex.png"),width=8, height = 14)
