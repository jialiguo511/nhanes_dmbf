rm(list = ls()); gc()

library(tidyverse)
library(ggplot2)
library(patchwork)

analysis_output_dir <- "/Users/jialiguo/jialiguo/NHANES Adiposity"

# Load PR and OR results
pr_results <- read_csv("complete cases/analysis/dbwse04_prevalence_ratios.csv")
or_results <- read_csv("complete cases/analysis/dbwse05_odds_ratios.csv")

#------------------------------------------------------------------------------------
# Forest plot for Prevalence Ratios
#------------------------------------------------------------------------------------

# Prepare data for forest plot
pr_plot_data <- pr_results %>%
  mutate(
    predictor_label = case_when(
      predictor == "bmi" ~ "BMI",
      predictor == "fat_percentage" ~ "Body fat %",
      predictor == "visceral_fat" ~ "Visceral fat",
      predictor == "waistcircumference" ~ "Waist circumference",
      predictor == "WHtR" ~ "WHtR",
      TRUE ~ predictor
    ),
    predictor_label = factor(predictor_label, 
                              levels = c("WHtR", "Waist circumference", "Visceral fat", 
                                         "Body fat %", "BMI")),
    model_label = ifelse(model_type == "single", "Single-metric", "Incremental (BMI+)"),
    outcome_label = factor(outcome, levels = c("DM vs NoDM", "PreDM vs NoDM"))
  )

# Create forest plot for PR
p_pr <- ggplot(pr_plot_data, aes(y = predictor_label, x = PR, xmin = CI_low, xmax = CI_high,
                                  color = model_label, shape = model_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbarh(height = 0.2, position = position_dodge(width = 0.5), linewidth = 1) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  facet_wrap(~outcome_label, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("Single-metric" = "#2166AC", "Incremental (BMI+)" = "#D6604D")) +
  scale_shape_manual(values = c("Single-metric" = 16, "Incremental (BMI+)" = 17)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5)) +
  labs(
    title = "Prevalence Ratios (per 1-SD increase)",
    x = "Prevalence Ratio (95% CI)",
    y = NULL,
    color = "Model type",
    shape = "Model type"
  ) +
  theme_bw(base_size = 19) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 21),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19),
    legend.text = element_text(size = 18),
    legend.key.size = grid::unit(1.3, "lines"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Forest plot for Odds Ratios
#------------------------------------------------------------------------------------

or_plot_data <- or_results %>%
  mutate(
    predictor_label = case_when(
      predictor == "bmi" ~ "BMI",
      predictor == "fat_percentage" ~ "Body fat %",
      predictor == "visceral_fat" ~ "Visceral fat",
      predictor == "waistcircumference" ~ "Waist circumference",
      predictor == "WHtR" ~ "WHtR",
      TRUE ~ predictor
    ),
    predictor_label = factor(predictor_label, 
                              levels = c("WHtR", "Waist circumference", "Visceral fat", 
                                         "Body fat %", "BMI")),
    model_label = ifelse(model_type == "single", "Single-metric", "Incremental (BMI+)"),
    outcome_label = factor(outcome, levels = c("DM vs NoDM", "PreDM vs NoDM"))
  )

p_or <- ggplot(or_plot_data, aes(y = predictor_label, x = OR, xmin = CI_low, xmax = CI_high,
                                  color = model_label, shape = model_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbarh(height = 0.2, position = position_dodge(width = 0.5), linewidth = 1) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  facet_wrap(~outcome_label, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("Single-metric" = "#2166AC", "Incremental (BMI+)" = "#D6604D")) +
  scale_shape_manual(values = c("Single-metric" = 16, "Incremental (BMI+)" = 17)) +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  labs(
    title = "Odds Ratios (per 1-SD increase)",
    x = "Odds Ratio (95% CI)",
    y = NULL,
    color = "Model type",
    shape = "Model type"
  ) +
  theme_bw(base_size = 19) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 21),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19),
    legend.text = element_text(size = 18),
    legend.key.size = grid::unit(1.3, "lines"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Combined plot
#------------------------------------------------------------------------------------

p_combined <- p_pr / p_or + 
  plot_annotation(tag_levels = 'A', 
                  theme = theme(plot.tag = element_text(face = "bold", size = 27)))

# Save figures
# ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/forest_plot_PR.png"),
#        plot = p_pr, width = 10, height = 6, units = "in", dpi = 300)
# 
# ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/forest_plot_OR.png"),
#        plot = p_or, width = 10, height = 6, units = "in", dpi = 300)
# 
# ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/forest_plot_PR_OR_combined.png"),
#        plot = p_combined, width = 10, height = 11, units = "in", dpi = 300)

ggsave(
  file.path(analysis_output_dir, "figures", "forest_plot_PR_OR_combined.png"),
  plot = p_combined,
  width = 13,
  height = 13,
  units = "in",
  dpi = 300
)

