rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
})

analysis_dir <- file.path("complete cases", "analysis")
required_files <- c(
  "dbwse07_auc_results.csv",
  "dbwse07_calibration_brier.csv",
  "dbwse07_roc_curve_data.csv",
  "dbwse07_calibration_curve_data.csv"
)
missing_files <- required_files[
  !file.exists(file.path(analysis_dir, required_files))
]
if (length(missing_files) > 0) {
  stop(
    "Run complete cases/analysis/dbwse07_incremental_value_advanced.R first. ",
    "Missing: ", paste(missing_files, collapse = ", ")
  )
}

auc_results <- read_csv(
  file.path(analysis_dir, "dbwse07_auc_results.csv"),
  show_col_types = FALSE
)
roc_curve_data <- read_csv(
  file.path(analysis_dir, "dbwse07_roc_curve_data.csv"),
  show_col_types = FALSE
)
calibration_data <- read_csv(
  file.path(analysis_dir, "dbwse07_calibration_curve_data.csv"),
  show_col_types = FALSE
)

outcome_levels <- c("DM vs NoDM", "PreDM vs NoDM")
model_levels <- c("base", "fat", "vf", "wc", "whtr")
model_labels <- c(
  base = "Base model",
  fat = "+ Body fat %",
  vf = "+ Visceral fat",
  wc = "+ Waist circumference",
  whtr = "+ Waist-to-height ratio"
)
annotation_model_labels <- c(
  base = "Base",
  fat = "+ TBF",
  vf = "+ VF",
  wc = "+ WC",
  whtr = "+ WHtR"
)
model_colors <- c(
  base = "#222222",
  fat = "#999999",
  vf = "#009E73",
  wc = "#D55E00",
  whtr = "#0072B2"
)

prepare_factors <- function(data) {
  data %>%
    mutate(
      outcome = factor(outcome, levels = outcome_levels),
      model_id = factor(model_id, levels = model_levels)
    )
}

auc_results <- prepare_factors(auc_results)
roc_curve_data <- prepare_factors(roc_curve_data)
calibration_data <- prepare_factors(calibration_data)

base_theme <- theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12.5, margin = margin(b = 8)),
    plot.caption = element_text(size = 11, hjust = 0),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11.5, color = "#333333"),
    strip.background = element_rect(fill = "#EEEEEE", color = "#777777"),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.key.width = unit(1.2, "lines"),
    legend.spacing.x = unit(0.35, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
    panel.spacing.x = unit(1.35, "lines"),
    plot.margin = margin(10, 12, 8, 10)
  )

# -----------------------------------------------------------------------------
# Panel A: standard ROC curves with AUC and 95% CI
# -----------------------------------------------------------------------------

auc_annotation <- auc_results %>%
  group_by(outcome) %>%
  arrange(model_id, .by_group = TRUE) %>%
  mutate(
    annotation_y = 0.42 - (row_number() - 1) * 0.065,
    annotation_x = 0.32,
    annotation = sprintf(
      "%s %s (%s–%s)",
      annotation_model_labels[as.character(model_id)],
      sub("^0", "", sprintf("%.3f", AUC)),
      sub("^0", "", sprintf("%.3f", AUC_ci_lower)),
      sub("^0", "", sprintf("%.3f", AUC_ci_upper))
    )
  ) %>%
  ungroup()

p_roc <- ggplot(
  roc_curve_data,
  aes(
    x = false_positive_rate,
    y = sensitivity,
    color = model_id,
    group = model_id
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linewidth = 0.7,
    linetype = "dashed",
    color = "#BDBDBD"
  ) +
  geom_step(linewidth = 0.95, alpha = 0.95, direction = "vh") +
  geom_text(
    data = auc_annotation,
    aes(x = annotation_x, y = annotation_y, label = annotation, color = model_id),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4.25,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ outcome, nrow = 1) +
  scale_color_manual(
    values = model_colors,
    labels = model_labels,
    drop = FALSE,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(
    title = "Model discrimination",
    subtitle = "ROC curves; annotations show AUC (95% CI)",
    x = "1 − Specificity",
    y = "Sensitivity"
  ) +
  base_theme +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# Panel B: incremental AUC with 95% CI and DeLong P values
# -----------------------------------------------------------------------------

delta_data <- auc_results %>%
  filter(model_type == "incremental") %>%
  mutate(
    metric_label = factor(
      as.character(model_id),
      levels = rev(c("fat", "vf", "wc", "whtr")),
      labels = rev(c(
        "Body fat %",
        "Visceral fat",
        "Waist circumference",
        "Waist-to-height ratio"
      ))
    ),
    significance = case_when(
      p_delong < 0.001 ~ "***",
      p_delong < 0.01 ~ "**",
      p_delong < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    result_label = sprintf("%.2f %s", AUC_diff * 100, significance),
    label_x = AUC_diff_ci_upper * 100 + 0.06
  )

p_delta <- ggplot(
  delta_data,
  aes(y = metric_label, x = AUC_diff * 100, color = model_id)
) +
  geom_vline(xintercept = 0, color = "#777777", linewidth = 0.6) +
  geom_errorbarh(
    aes(
      xmin = AUC_diff_ci_lower * 100,
      xmax = AUC_diff_ci_upper * 100
    ),
    height = 0.20,
    linewidth = 0.8
  ) +
  geom_point(size = 3.3) +
  geom_text(
    aes(x = label_x, label = result_label),
    hjust = 0,
    size = 4.70,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ outcome, ncol = 1) +
  scale_color_manual(values = model_colors, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.12, 0.28))) +
  labs(
    title = "Incremental discrimination",
    subtitle = "Change in AUC beyond the base model\n(percentage points, 95% CI)",
    x = "Change in AUC (percentage points)",
    y = NULL,
    caption = "DeLong: *** P<0.001; ** P<0.01; * P<0.05; ns P≥0.05"
  ) +
  base_theme +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# -----------------------------------------------------------------------------
# Panel C: calibration/reliability curves with Brier scores
# -----------------------------------------------------------------------------

brier_annotation <- auc_results %>%
  group_by(outcome) %>%
  arrange(model_id, .by_group = TRUE) %>%
  mutate(
    annotation_x = 0.04,
    annotation_y = 0.95 - (row_number() - 1) * 0.055,
    annotation = sprintf(
      "%s: Brier %.4f",
      model_labels[as.character(model_id)],
      Brier
    )
  ) %>%
  ungroup()

p_calibration <- ggplot(
  calibration_data,
  aes(
    x = mean_predicted,
    y = observed_rate,
    color = model_id,
    group = model_id
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linewidth = 0.8,
    linetype = "dashed",
    color = "#777777"
  ) +
  geom_errorbar(
    aes(ymin = observed_ci_lower, ymax = observed_ci_upper),
    width = 0,
    linewidth = 0.45,
    alpha = 0.28
  ) +
  geom_line(linewidth = 0.9, alpha = 0.90) +
  geom_point(size = 2.3, alpha = 0.95) +
  geom_text(
    data = brier_annotation,
    aes(x = annotation_x, y = annotation_y, label = annotation, color = model_id),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4.50,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ outcome, nrow = 1) +
  scale_color_manual(values = model_colors, labels = model_labels, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(
    title = "Model calibration",
    subtitle = "Apparent reliability curves by prediction decile; vertical bars show 95% CI",
    x = "Mean predicted probability",
    y = "Observed proportion",
    caption = "Dashed line indicates perfect calibration. Lower Brier scores indicate lower overall prediction error."
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2, size = 3)
    )
  ) +
  base_theme

figure_design <- "
AAB
AAB
CCC
CCC
"

combined_figure <- p_roc + p_delta + p_calibration +
  plot_layout(design = figure_design, guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    caption = paste0(
      "Base model includes age, sex, race/ethnicity, and BMI. ",
      "Incremental models add one adiposity measure. Analyses are unweighted and use pairwise outcome samples."
    ),
    theme = theme(
      plot.tag = element_text(face = "bold", size = 20),
      plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 8))
    )
  ) &
  theme(legend.position = "bottom")

local_output_dir <- file.path("complete cases", "paper", "output")
dir.create(local_output_dir, recursive = TRUE, showWarnings = FALSE)

configured_output_dir <- Sys.getenv(
  "NHANES_ADIPOSITY_DIR",
  unset = "/Users/jialiguo/jialiguo/NHANES Adiposity"
)
publication_output_dir <- file.path(configured_output_dir, "figures")
dir.create(publication_output_dir, recursive = TRUE, showWarnings = FALSE)

png_name <- "incremental_value_combined_corrected.png"
tiff_name <- "incremental_value_combined_corrected.tiff"

ggsave(
  file.path(local_output_dir, png_name),
  plot = combined_figure,
  width = 18,
  height = 13,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggsave(
  file.path(publication_output_dir, png_name),
  plot = combined_figure,
  width = 18,
  height = 13,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggsave(
  file.path(publication_output_dir, tiff_name),
  plot = combined_figure,
  width = 18,
  height = 13,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

cat("Saved corrected figure to:\n")
cat("  ", file.path(local_output_dir, png_name), "\n", sep = "")
cat("  ", file.path(publication_output_dir, png_name), "\n", sep = "")
cat("  ", file.path(publication_output_dir, tiff_name), "\n", sep = "")
