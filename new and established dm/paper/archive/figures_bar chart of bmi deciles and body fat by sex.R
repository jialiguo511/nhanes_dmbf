rm(list=ls());gc();source(".Rprofile")

library(survey)
library(mitools)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw02_weighted df.RDS")) 

weight_data_list <- list()

for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]]
  
  nhanes_total_svy <- df %>%
    select(respondentid, female, bmi, fat_percentage)
  
  weight_data_list[[i]] <- as.data.frame(nhanes_total_svy)
}

fig_df <- bind_rows(weight_data_list) %>%
  group_by(respondentid, female) %>% 
  summarise(
    bmi = mean(bmi, na.rm = TRUE),
    fat_percentage = mean(fat_percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  mutate(
    bmi_decile = ntile(bmi, 10),
    bmi_grp = case_when(
      bmi_decile == 1 ~ "13.4-21.4",
      bmi_decile == 2 ~ "21.4-23.4",
      bmi_decile == 3 ~ "23.4-25.0",
      bmi_decile == 4 ~ "25.0-26.5",
      bmi_decile == 5 ~ "26.5-28.0",
      bmi_decile == 6 ~ "28.0-29.6",
      bmi_decile == 7 ~ "29.6-31.6",
      bmi_decile == 8 ~ "31.6-34.0",
      bmi_decile == 9 ~ "34.0-38.3",
      bmi_decile == 10 ~ "38.3-86.2",
    ), 
    fat_percentage_grp = case_when(
      female == 0 & fat_percentage <= 25 ~ "Lean/Normal",
      female == 0 & fat_percentage > 25 & fat_percentage <= 30 ~ "Overweight",
      female == 0 & fat_percentage > 30 ~ "Obese",
      female == 1 & fat_percentage > 35 & fat_percentage <= 40 ~ "Overweight",
      female == 1 & fat_percentage > 40 ~ "Obese",
      TRUE ~ "Lean/Normal"
    )
  )

plot_data <- fig_df %>%
  group_by(bmi_grp, fat_percentage_grp) %>%
  summarise(
    female_n = sum(female == 1),  # count females only
    male_n = sum(female == 0),  # count males only
    .groups = 'drop'
  ) %>%
  arrange(bmi_grp, desc(fat_percentage_grp)) %>% 
  # mutate(bmi_grp = factor(bmi_grp, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>% 
  group_by(bmi_grp) %>%
  mutate(
    female_prop = round((female_n / sum(female_n)) * 100, 1),
    male_prop = round((male_n / sum(male_n)) * 100, 1)
  ) %>%
  ungroup()

male_df <- plot_data %>% 
  select(-c("female_n","female_prop")) %>% 
  mutate(fat_percentage_grp = case_when(
    fat_percentage_grp == "Lean/Normal" ~ "Lean/Normal (≤25)",
    fat_percentage_grp == "Overweight" ~ "Overweight (25-29.9)",
    TRUE ~ "Obese (>30)"
  ),
  fat_percentage_grp = factor(fat_percentage_grp, levels = c("Lean/Normal (≤25)", "Overweight (25-29.9)", "Obese (>30)")))

female_df <- plot_data %>% 
  select(-c("male_n","male_prop")) %>% 
  mutate(fat_percentage_grp = case_when(
    fat_percentage_grp == "Lean/Normal" ~ "Lean/Normal (≤35)",
    fat_percentage_grp == "Overweight" ~ "Overweight (35-39.9)",
    TRUE ~ "Obese (>40)"
  ),
  fat_percentage_grp = factor(fat_percentage_grp, levels = c("Lean/Normal (≤35)", "Overweight (35-39.9)", "Obese (>40)")))


fig_male <- ggplot(male_df, aes(x = bmi_grp, y = male_prop, fill = fat_percentage_grp)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  
  geom_text(aes(label = male_prop, y = male_prop + 1), position = position_dodge(width = 0.9), vjust = -0.1, size = 4) +  
  scale_fill_manual(values = c("Lean/Normal (≤25)" = "white", "Overweight (25-29.9)" = "grey", "Obese (>30)" = "black")) +
  labs(x = "BMI", y = "Proportion (%)", fill = "BF%", title = "MEN") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title.x = element_text(size = 14, hjust = 1, vjust = 10), 
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 2)),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 0.6), 
    axis.text.y = element_text(size = 12),  
    legend.position = c(0.25, 1),  
    legend.justification = c(1, 1),  
    legend.background = element_rect(fill = "white", colour = "black"), 
    legend.text = element_text(size = 12),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),  
    plot.title = element_text(hjust = 0.5, vjust = -5, size = 14, face = "bold"),  
    plot.margin = unit(c(-1, 0.3, -1, 0.1), "lines"),  # Reduce plot margins
    # axis.ticks = element_line(colour = "black"),  # Add ticks to show data points
    # axis.ticks.length = unit(0.1, "cm"),  # Shorter length of the ticks
    axis.line = element_blank()  
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.5) +  
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) 
# +  
#   scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
#   scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 101))  


fig_female <- ggplot(female_df, aes(x = bmi_grp, y = female_prop, fill = fat_percentage_grp)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  
  geom_text(aes(label = female_prop, y = female_prop + 1), position = position_dodge(width = 0.9), vjust = -0.1, size = 4) +  
  scale_fill_manual(values = c("Lean/Normal (≤35)" = "white", "Overweight (35-39.9)" = "grey", "Obese (>40)" = "black")) +
  labs(x = "BMI", y = "Proportion (%)", fill = "BF%", title = "WOMEN") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title.x = element_text(size = 14, hjust = 1, vjust = 10), 
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 2)),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 0.6), 
    axis.text.y = element_text(size = 12, hjust = 1),  
    legend.position = c(0.25, 1),  
    legend.justification = c(1, 1),  
    legend.background = element_rect(fill = "white", colour = "black"), 
    legend.text = element_text(size = 12),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),  
    plot.title = element_text(hjust = 0.5, vjust = -5, size = 14, face = "bold"),
    plot.margin = unit(c(-1, 0.3, -1, 0.1), "lines"), 
    # axis.ticks = element_line(colour = "black"),  # Add ticks to show data points
    # axis.ticks.length = unit(0.1, "cm"),  # Shorter length of the ticks
    axis.line = element_blank()  
  ) +
  geom_hline(yintercept = 0, colour = "black", size = 0.5) +  
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) 
# +  
#   scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
#   scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 101))  


library(patchwork)

combined_plot <- fig_male / fig_female 

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/barchart of body fat in bmi deciles by sex.png"),width=12, height = 12)


