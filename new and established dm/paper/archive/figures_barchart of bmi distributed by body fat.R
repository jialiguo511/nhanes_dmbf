rm(list=ls());gc();source(".Rprofile")

source("analysis/dbw01_weighted sample.R")
library(ggplot2)

nhanes_20112018_svy = nhanes_20112018 %>% 
  dplyr::filter(!is.na(fat_percentage)) %>%
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")

nhanes_20112018_svy <- nhanes_20112018_svy %>% 
  mutate(glucosef_category = case_when(fasting_glucose < 100 ~ "<100",
                                       fasting_glucose >= 100 & fasting_glucose <= 126 ~ "100-126",
                                       fasting_glucose > 126 & fasting_glucose <= 200 ~ "126-200",
                                       TRUE ~ ">200"),
         hba1c_category = case_when(glycohemoglobin < 5.7 ~ "<5.7%",
                                    glycohemoglobin >= 5.7 & glycohemoglobin <= 6.5 ~ "5.7%-6.5%",
                                    glycohemoglobin > 6.5 & glycohemoglobin <= 9 ~ "6.5%-9%",
                                    TRUE ~ ">9%"),
         bmi_category = case_when(bmi<18.5 ~ "<18.5",
                                  bmi>=18.5 & bmi<25 ~ "18.5-24.9",
                                  bmi>=25 & bmi<30 ~ "25-29.9",
                                  TRUE ~ ">=30")) %>% 
  mutate(glucosef_category = factor(glucosef_category, levels = c("<100", "100-126", "126-200", ">200")),
         hba1c_category = factor(hba1c_category, levels = c("<5.7%", "5.7%-6.5%", "6.5%-9%", ">9%")),
         bmi_category = factor(bmi_category, levels = c("<18.5", "18.5-24.9", "25-29.9", "30-39.9", ">=40"))) %>% 
  dplyr::filter(bmi_category != "Unknown")

nhanes_males_svy <- nhanes_20112018_svy %>% 
  dplyr::filter(female == 0) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 14 ~ "<=14%",
    fat_percentage > 14 & fat_percentage <= 20 ~ "14-20%",
    fat_percentage > 20 & fat_percentage <= 25 ~ "20-25%",
    TRUE ~ ">25%")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=14%","14-20%","20-25%",">25%")))

nhanes_females_svy <- nhanes_20112018_svy %>% 
  dplyr::filter(female == 1) %>% 
  mutate(fat_percentage_category = case_when(
    fat_percentage <= 25 ~ "<=25%",
    fat_percentage > 25 & fat_percentage <= 30 ~ "25-30%",
    fat_percentage > 30 & fat_percentage <= 35 ~ "30-35%",
    TRUE ~ ">35%")) %>% 
  mutate(fat_percentage_category = factor(
    fat_percentage_category, levels = c("<=25%","25-30%","30-35%",">35%")))

#--------------------------------------------------------------------------------------------------
# Create tables and calculate proportions for males
male_table <- svytable(~bmi_category + fat_percentage_category, nhanes_males_svy)
male_proportions <- prop.table(male_table, margin = 1) * 100
male_df <- as.data.frame(male_proportions)
names(male_df) <- c("BMI Category", "Fat Percentage Category", "Percentage")

# Create tables and calculate proportions for females
female_table <- svytable(~bmi_category + fat_percentage_category, nhanes_females_svy)
female_proportions <- prop.table(female_table, margin = 1) * 100
female_df <- as.data.frame(female_proportions)
names(female_df) <- c("BMI Category", "Fat Percentage Category", "Percentage")


fig_male <- ggplot(data = male_df, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "B. Male", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))

fig_female <- ggplot(data = female_df, aes(x = `BMI Category`, y = Percentage, fill = `Fat Percentage Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "A. Female", x = "BMI Category", y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))

library(gridExtra)

fig_combined <- grid.arrange(fig_female, fig_male, nrow = 2, heights = c(1, 1))

ggsave(fig_combined, filename=paste0(path_nhanes_dmbf_folder, "/figures/distribution of body fat in bmi by sex.png"),width=8, height = 9)

#--------------------------------------------------------------------------------------------------
mean_fat_male <- svyby(~fat_percentage, ~bmi_category, nhanes_males_svy, svymean)
mean_fat_female <- svyby(~fat_percentage, ~bmi_category, nhanes_females_svy, svymean)

fat_category_proportions_male <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_males_svy), margin = 1) * 100
fat_category_proportions_female <- prop.table(svytable(~bmi_category + fat_percentage_category, nhanes_females_svy), margin = 1) * 100

plot_data_male <- merge(as.data.frame(mean_fat_male), as.data.frame(fat_category_proportions_male), by = "bmi_category")
plot_data_female <- merge(as.data.frame(mean_fat_female), as.data.frame(fat_category_proportions_female), by = "bmi_category")

names(plot_data_male) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")
names(plot_data_female) <- c("BMI Category", "Mean Fat Percentage", "SD Fat Percentage", "Fat Percentage Category", "Percentage")


fig_male <- ggplot(data = plot_data_male, aes(x = `BMI Category`, y = `Mean Fat Percentage`, fill = `Fat Percentage Category`)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage), y = `Mean Fat Percentage` * Percentage / 100), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "B. Male",
       x = "BMI Category", y = "Mean Fat Percentage") +
  theme_minimal() 

fig_female <- ggplot(data = plot_data_female, aes(x = `BMI Category`, y = `Mean Fat Percentage`, fill = `Fat Percentage Category`)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage), y = `Mean Fat Percentage` * Percentage / 100), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "A. Female",
       x = "BMI Category", y = "Mean Fat Percentage") +
  theme_minimal()

#--------------------------------------------------------
# Enhance the dataset by calculating cumulative percentages and adjust label positioning
plot_data_female <- plot_data_female %>%
  group_by(`BMI Category`) %>%
  arrange(`BMI Category`, desc(`Fat Percentage Category`)) %>%
  mutate(Percentage = `Mean Fat Percentage` * Percentage / 100,
         Cumulative = cumsum(Percentage),
         Label_Pos = Cumulative - 0.5 * Percentage,
         # Adjust vjust dynamically based on the percentage size for better visibility
         vjust_adjust = ifelse(Percentage < 5, -0.5, 0.5))

fig_female <- ggplot(data = plot_data_female, aes(x = `BMI Category`, y = `Mean Fat Percentage`, fill = `Fat Percentage Category`)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", `Percentage`), y = Label_Pos, vjust = vjust_adjust)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "A. Female",
       x = "BMI Category", y = "Mean Fat Percentage") +
  theme_minimal()

plot_data_male <- plot_data_male %>%
  group_by(`BMI Category`) %>%
  arrange(`BMI Category`, desc(`Fat Percentage Category`)) %>%
  mutate(Percentage = `Mean Fat Percentage` * Percentage / 100,
         Cumulative = cumsum(Percentage),
         Label_Pos = Cumulative - 0.5 * Percentage,
         # Adjust vjust dynamically based on the percentage size for better visibility
         vjust_adjust = ifelse(Percentage < 5, -0.5, 0.5))

fig_male <- ggplot(data = plot_data_male, aes(x = `BMI Category`, y = `Mean Fat Percentage`, fill = `Fat Percentage Category`)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage), y = `Mean Fat Percentage` * Percentage / 100), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "B. Male",
       x = "BMI Category", y = "Mean Fat Percentage") +
  theme_minimal() 

fig_combined <- grid.arrange(fig_female, fig_male, nrow = 2, heights = c(1, 1))

ggsave(fig_combined, filename=paste0(path_nhanes_dmbf_folder, "/figures/distribution of mean body fat in bmi by sex.png"),width=8, height = 9)




















