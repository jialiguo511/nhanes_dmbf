rm(list=ls());gc();source(".Rprofile")

source_df <- readRDS(paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds"))

library(ggplot2)


summary_df <- source_df %>%
  group_by(year, dm) %>%
  summarise(mean_fat_percentage = mean(fat_percentage, na.rm = TRUE),
            se = sd(fat_percentage, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_fat_percentage - qt(0.975, df = n()-1) * se,
            upper_ci = mean_fat_percentage + qt(0.975, df = n()-1) * se)

fig <- ggplot(summary_df, aes(x = as.factor(year), y = mean_fat_percentage, color = as.factor(dm), group = as.factor(dm))) +
  geom_line() +
  geom_point() +  # Adding points to visualize individual data points
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = as.factor(dm)), alpha = 0.2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Non-diabetes", "Diabetes")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF"), labels = c("Non-diabetes", "Diabetes")) +
  labs(x = "Year",
       y = "Mean Body Fat Percentage (%)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_minimal() +
  ggtitle("Trend of Mean Body Fat Percentage by Diabetes Status (2011-2018) with 95% CI")

