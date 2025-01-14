rm(list=ls());gc();source(".Rprofile")

library(survey)
library(mitools)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS")) 

weight_data_list <- list()

for (i in 1:length(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]]
  
  # Assuming you want to keep gender in your final data
  nhanes_total_svy <- df %>%
    select(respondentid, female, bmi, fat_percentage)
  
  weight_data_list[[i]] <- as.data.frame(nhanes_total_svy)
}

# Binding all dataframes into one
fig_df <- bind_rows(weight_data_list) %>%
  group_by(respondentid, female) %>% 
  summarise(
    bmi = mean(bmi, na.rm = TRUE),
    fat_percentage = mean(fat_percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  mutate(
    bmi_grp = case_when(
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ "Lean"
      ),
    fat_percentage_grp = case_when(
      female == 0 & fat_percentage >= 20 & fat_percentage < 25 ~ "Overweight",
      female == 1 & fat_percentage >= 30 & fat_percentage < 35 ~ "Overweight",
      female == 0 & fat_percentage >= 25 ~ "Obese",
      female == 1 & fat_percentage >= 35 ~ "Obese",
      TRUE ~ "Lean"
    )
         ) 

plot_data <- fig_df %>%
  group_by(bmi_grp, fat_percentage_grp) %>%
  summarise(
    total_n = n(),  # total counts per group
    female_n = sum(female == 1),  # count females only
    male_n = sum(female == 0),  # count males only
    .groups = 'drop'
  ) %>%
  arrange(bmi_grp, desc(fat_percentage_grp)) %>% 
  mutate(bmi_grp = factor(bmi_grp, levels = c("Lean", "Overweight", "Obese")),
         fat_percentage_grp = factor(fat_percentage_grp, levels = c("Lean", "Overweight", "Obese")))



fig_all <- ggplot(plot_data, aes(x = bmi_grp, y = total_n, fill = fat_percentage_grp)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  
  geom_text(aes(label = total_n, y = total_n + 50), position = position_dodge(width = 0.9), vjust = -0.1, size = 4) +  
  scale_fill_manual(values = c("Lean" = "white", "Overweight" = "grey", "Obese" = "black")) +
  labs(x = "BMI", y = "Number of Subjects", fill = "BF%", title = "ALL") +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.title.x = element_text(size = 11, hjust = 1, vjust = 10), 
    axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 2)),
    axis.text.x = element_text(size = 11, vjust = 4), 
    axis.text.y = element_text(size = 11, hjust = 1),  
    legend.position = c(0.25, 1),  
    legend.justification = c(1, 1),  
    legend.background = element_rect(fill = "white", colour = "black"), 
    legend.text = element_text(size = 11),  
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
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(limits = c("Lean", "Overweight", "Obese")) +
  scale_y_continuous(breaks = seq(0, 8000, by = 1000))  


fig_male <- ggplot(plot_data, aes(x = bmi_grp, y = male_n, fill = fat_percentage_grp)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  
  geom_text(aes(label = male_n, y = male_n + 50), position = position_dodge(width = 0.9), vjust = -0.1, size = 4) +  
  scale_fill_manual(values = c("Lean" = "white", "Overweight" = "grey", "Obese" = "black")) +
  labs(x = "BMI", y = "Number of Subjects", fill = "BF%", title = "MEN") +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.title.x = element_text(size = 11, hjust = 1, vjust = 10), 
    axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 2)),
    axis.text.x = element_text(size = 11, vjust = 4), 
    axis.text.y = element_text(size = 11),  
    legend.position = "none",
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
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(limits = c("Lean", "Overweight", "Obese")) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500))  


fig_female <- ggplot(plot_data, aes(x = bmi_grp, y = female_n, fill = fat_percentage_grp)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +  
  geom_text(aes(label = female_n, y = female_n + 50), position = position_dodge(width = 0.9), vjust = -0.1, size = 4) +  
  scale_fill_manual(values = c("Lean" = "white", "Overweight" = "grey", "Obese" = "black")) +
  labs(x = "BMI", y = "Number of Subjects", fill = "BF%", title = "WOMEN") +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.title.x = element_text(size = 11, hjust = 1, vjust = 10), 
    axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 2)),
    axis.text.x = element_text(size = 11, vjust = 4), 
    axis.text.y = element_text(size = 11, hjust = 1),  
    legend.position = "none",  
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
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(limits = c("Lean", "Overweight", "Obese")) +
  scale_y_continuous(breaks = seq(0, 4500, by = 500))  


library(patchwork)

combined_plot <- fig_all / fig_male / fig_female 

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/barchart of body fat in bmi by sex.png"),width=7.5, height = 13)
  





