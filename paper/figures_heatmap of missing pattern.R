rm(list=ls());gc();source(".Rprofile")

library(naniar)
library(ggplot2)

nhanes_20112018 <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_unweighted sample.RDS")) %>% 
  mutate(sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
         dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE))

colnames(nhanes_20112018)

vars_to_check <- c("fat_percentage", "visceral_fat", "subcutaneous_fat", 
                   "waistcircumference", "sbp", "dbp",
                   "total_cholesterol", "hdl", "ldl", "triglyceride")

miss_df <- nhanes_20112018 %>%
  select(all_of(vars_to_check))

miss_df_labeled <- miss_df %>%
  rename(
    `Body Fat (%)`            = fat_percentage,
    `Waist Circumference (cm)` = waistcircumference,
    `Systolic BP (mmHg)`       = sbp,
    `Diastolic BP (mmHg)`      = dbp,
    `Total Chol (mg/dL)`       = total_cholesterol,
    `HDL (mg/dL)`              = hdl,
    `LDL (mg/dL)`              = ldl,
    `Triglycerides (mg/dL)`    = triglyceride
  )

heatmap <- vis_miss(miss_df_labeled, cluster = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL, 
    y = NULL,
    fill = "Missing?"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.1)
  )

ggsave(heatmap,filename=paste0(path_nhanes_dmbf_folder,"/figures/one dm group/heatmap of missing pattern.jpg"),width=12,height = 8)


# archive -------------------------------
# Select only the variables of interest
missing_df <- nhanes_20112018 %>% select(all_of(vars_to_check))

# Visualize missingness pattern
gg_miss_upset(missing_df)

# Optional: Heatmap-style missingness matrix
vis_miss(missing_df)


missing_df_sorted <- missing_df %>%
  mutate(missing_count = rowSums(is.na(.))) %>%
  arrange(desc(missing_count)) %>%
  select(-missing_count)

# Plot heatmap
vis_miss(missing_df_sorted, sort_miss = TRUE) +
  labs(
    title = "Missing Data Heatmap",
    subtitle = "Visualizing missingness across selected clinical variables",
    x = "Variables",
    y = "Observations"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    panel.grid = element_blank()
  )
