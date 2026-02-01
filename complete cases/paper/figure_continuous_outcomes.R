rm(list=ls());gc();source(".Rprofile")

library(tidyverse)
library(ggplot2)
library(patchwork)

# Load spline analysis results
spline_results <- read_csv("complete cases/analysis/dbwse06_spline_models.csv")

#------------------------------------------------------------------------------------
# Figure 1: Linear coefficients (beta) for FPG and A1c
#------------------------------------------------------------------------------------

plot_data_beta <- spline_results %>%
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
    outcome_label = case_when(
      outcome == "FPG" ~ "Fasting glucose (mg/dL)",
      outcome == "A1c" ~ "HbA1c (%)",
      TRUE ~ outcome
    ),
    CI_low = beta_linear - 1.96 * SE_linear,
    CI_high = beta_linear + 1.96 * SE_linear
  )

p_beta <- ggplot(plot_data_beta, aes(y = predictor_label, x = beta_linear, 
                                      xmin = CI_low, xmax = CI_high,
                                      color = model_label, shape = model_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_errorbarh(height = 0.2, position = position_dodge(width = 0.5), linewidth = 0.8) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  facet_wrap(~outcome_label, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("Single-metric" = "#2166AC", "Incremental (BMI+)" = "#D6604D")) +
  scale_shape_manual(values = c("Single-metric" = 16, "Incremental (BMI+)" = 17)) +
  labs(
    title = "Linear associations with continuous outcomes (per 1-SD increase)",
    x = "Beta coefficient (95% CI)",
    y = NULL,
    color = "Model type",
    shape = "Model type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------------
# Figure 2: Nonlinearity test (LR test p-values)
#------------------------------------------------------------------------------------

plot_data_nonlin <- spline_results %>%
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
                              levels = c("BMI", "Body fat %", "Visceral fat", 
                                         "Waist circumference", "WHtR")),
    model_label = ifelse(model_type == "single", "Single-metric", "Incremental (BMI+)"),
    outcome_label = case_when(
      outcome == "FPG" ~ "Fasting glucose",
      outcome == "A1c" ~ "HbA1c",
      TRUE ~ outcome
    ),
    sig_label = case_when(
      p_nonlinearity < 0.001 ~ "***",
      p_nonlinearity < 0.01 ~ "**",
      p_nonlinearity < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    neg_log_p = -log10(p_nonlinearity)
  )

p_nonlin <- ggplot(plot_data_nonlin, aes(x = predictor_label, y = neg_log_p, 
                                          fill = model_label)) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sig_label), position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) +
  facet_wrap(~outcome_label, ncol = 2) +
  scale_fill_manual(values = c("Single-metric" = "#2166AC", "Incremental (BMI+)" = "#D6604D")) +
  labs(
    title = "Evidence for nonlinear associations (Likelihood Ratio test)",
    x = NULL,
    y = expression(-log[10](P[nonlinearity])),
    fill = "Model type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.5, y = -log10(0.05), label = "P = 0.05", 
           hjust = -0.1, vjust = -0.5, size = 3, color = "red")

#------------------------------------------------------------------------------------
# Combined plot with caption
#------------------------------------------------------------------------------------

# Create figure caption
fig_caption <- str_wrap(
  "This figure presents the associations between adiposity metrics and continuous glycemic outcomes (fasting glucose and HbA1c) in 5,168 U.S. adults from NHANES. 
  Panel A displays beta coefficients (per 1-SD increase) from survey-weighted linear regression models adjusted for age, sex, and race/ethnicity. 
  Single-metric models include one adiposity measure along with covariates. Incremental models add each adiposity measure to a base model that already includes BMI and covariates. 
  All adiposity metrics were z-score standardized using mean and standard deviation from the overall sample before subsetting. 
  Panel B presents evidence for nonlinearity using likelihood ratio tests that compare the fit of linear models versus models with natural cubic splines (4 degrees of freedom). 
  Bars extending above the red dashed horizontal line (P=0.05 threshold) indicate statistically significant departure from linearity. 
  Central adiposity measures (visceral fat, waist circumference, and waist-to-height ratio) demonstrate stronger associations with both glycemic outcomes compared to BMI and body fat percentage. 
  Most associations exhibit significant nonlinearity (P<0.001), indicating that the risk increases more rapidly at higher levels of adiposity rather than following a constant linear relationship. 
  In incremental models, central adiposity measures provide substantial additional predictive value beyond BMI alone, whereas body fat percentage shows minimal or negative associations after accounting for BMI. 
  Statistical significance is denoted as: *** P<0.001; ** P<0.01; * P<0.05; ns = non-significant (P≥0.05).",
  width = 180
)

p_combined <- p_beta / p_nonlin + 
  plot_annotation(
    tag_levels = 'A',
    title = "Adiposity metrics and continuous glycemic outcomes: linear associations and nonlinearity testing",
    caption = fig_caption,
    theme = theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0),
      plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2, margin = margin(t = 15))
    )
  )

# Save figures
ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/continuous_outcomes_beta.png"),
       plot = p_beta, width = 10, height = 6, units = "in", dpi = 300)

ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/continuous_outcomes_nonlinearity.png"),
       plot = p_nonlin, width = 10, height = 6, units = "in", dpi = 300)

ggsave(paste0(path_nhanes_dmbf_folder, "/figures/complete cases/continuous_outcomes_combined.png"),
       plot = p_combined, width = 10, height = 11, units = "in", dpi = 300)

