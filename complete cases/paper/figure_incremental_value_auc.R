rm(list=ls());gc();source(".Rprofile")

library(tidyverse)
library(ggplot2)
library(patchwork)

# Load AUC and Brier results
auc_results <- read_csv("complete cases/analysis/dbwse07_auc_results.csv")
brier_results <- read_csv("complete cases/analysis/dbwse07_calibration_brier.csv")

#------------------------------------------------------------------------------------
# Figure 1: AUC comparison (base vs incremental)
#------------------------------------------------------------------------------------

auc_plot_data <- auc_results %>%
  mutate(
    predictor_label = case_when(
      added_predictor == "bmi_only" ~ "BMI only",
      added_predictor == "fat_percentage" ~ "BMI + Body fat %",
      added_predictor == "visceral_fat" ~ "BMI + Visceral fat",
      added_predictor == "waistcircumference" ~ "BMI + Waist circ.",
      added_predictor == "WHtR" ~ "BMI + WHtR",
      TRUE ~ added_predictor
    ),
    predictor_label = factor(predictor_label, 
                              levels = c("BMI only", "BMI + Body fat %", 
                                         "BMI + Visceral fat", "BMI + Waist circ.", 
                                         "BMI + WHtR")),
    outcome_label = outcome,
    sig_label = case_when(
      is.na(p_delong) ~ "",
      p_delong < 0.001 ~ "***",
      p_delong < 0.01 ~ "**",
      p_delong < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

p_auc <- ggplot(auc_plot_data, aes(x = predictor_label, y = AUC, fill = predictor_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", AUC)), vjust = -0.5, size = 3.5, fontface = "bold") +
  geom_text(aes(label = sig_label), vjust = 2, size = 5, color = "white") +
  facet_wrap(~outcome_label, ncol = 2) +
  scale_fill_manual(values = c("BMI only" = "gray60",
                                "BMI + Body fat %" = "#E0E0E0",
                                "BMI + Visceral fat" = "#1B9E77",
                                "BMI + Waist circ." = "#D95F02",
                                "BMI + WHtR" = "#7570B3")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Model discrimination: AUC with 95% CI",
    subtitle = "Significance (DeLong test): *** p<0.001, ** p<0.01, * p<0.05",
    x = NULL,
    y = "Area Under the Curve (AUC)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Figure 2: AUC improvement (delta AUC from base)
#------------------------------------------------------------------------------------

auc_delta_data <- auc_results %>%
  dplyr::filter(model_type == "incremental") %>%
  mutate(
    predictor_label = case_when(
      added_predictor == "fat_percentage" ~ "Body fat %",
      added_predictor == "visceral_fat" ~ "Visceral fat",
      added_predictor == "waistcircumference" ~ "Waist circ.",
      added_predictor == "WHtR" ~ "WHtR",
      TRUE ~ added_predictor
    ),
    predictor_label = factor(predictor_label, 
                              levels = c("WHtR", "Waist circ.", "Visceral fat", "Body fat %")),
    outcome_label = outcome,
    sig_label = case_when(
      p_delong < 0.001 ~ "***",
      p_delong < 0.01 ~ "**",
      p_delong < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    sig_color = ifelse(p_delong < 0.05, "Significant", "Non-significant")
  )

p_delta <- ggplot(auc_delta_data, aes(y = predictor_label, x = AUC_diff * 100,
                                       fill = sig_color)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", AUC_diff * 100)), 
            hjust = ifelse(auc_delta_data$AUC_diff > 0, -0.2, 1.2), size = 3.5, fontface = "bold") +
  geom_text(aes(label = sig_label), 
            hjust = 1.2, size = 4, color = "white", fontface = "bold") +
  facet_wrap(~outcome_label, ncol = 1) +
  scale_fill_manual(values = c("Significant" = "#1B9E77", "Non-significant" = "#999999")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Incremental value beyond BMI",
    subtitle = "Change in AUC when adding each metric to BMI base model (ns = non-significant)",
    x = "ΔAUC (percentage points)",
    y = NULL,
    fill = "DeLong test"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Figure 3: Brier scores (calibration)
#------------------------------------------------------------------------------------

brier_plot_data <- brier_results %>%
  mutate(
    predictor_label = case_when(
      added_predictor == "bmi_only" ~ "BMI only",
      added_predictor == "fat_percentage" ~ "BMI + Body fat %",
      added_predictor == "visceral_fat" ~ "BMI + Visceral fat",
      added_predictor == "waistcircumference" ~ "BMI + Waist circ.",
      added_predictor == "WHtR" ~ "BMI + WHtR",
      TRUE ~ added_predictor
    ),
    predictor_label = factor(predictor_label, 
                              levels = c("BMI only", "BMI + Body fat %", 
                                         "BMI + Visceral fat", "BMI + Waist circ.", 
                                         "BMI + WHtR")),
    outcome_label = outcome
  )

p_brier <- ggplot(brier_plot_data, aes(x = predictor_label, y = Brier, fill = predictor_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.4f", Brier)), vjust = -0.5, size = 3.5) +
  facet_wrap(~outcome_label, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("BMI only" = "gray60",
                                "BMI + Body fat %" = "#E0E0E0",
                                "BMI + Visceral fat" = "#1B9E77",
                                "BMI + Waist circ." = "#D95F02",
                                "BMI + WHtR" = "#7570B3")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Model calibration: Brier score",
    subtitle = "Lower values indicate better calibration",
    x = NULL,
    y = "Brier Score"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Combined plot
#------------------------------------------------------------------------------------

p_combined <- (p_auc | p_delta) / p_brier + 
  plot_annotation(tag_levels = 'A',
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

# Save figures
ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/incremental_value_auc.png"),
       plot = p_auc, width = 10, height = 6, units = "in", dpi = 300)

ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/incremental_value_delta_auc.png"),
       plot = p_delta, width = 7, height = 6, units = "in", dpi = 300)

ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/incremental_value_brier.png"),
       plot = p_brier, width = 10, height = 5, units = "in", dpi = 300)

ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/incremental_value_combined.png"),
       plot = p_combined, width = 14, height = 10, units = "in", dpi = 300)
