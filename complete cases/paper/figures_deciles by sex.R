rm(list=ls());gc();source(".Rprofile")

library(survey)
library(ggplot2)
library(dplyr)
library(tidyr)

# -------------------------------
# Load survey-weighted data (10 imputations)
# -------------------------------
nhanes_svy_dfs <- readRDS(
  paste0(path_nhanes_dmbf_folder,
         "/working/cleaned/dbwse02_weighted df with complete cases.RDS")
)

# -------------------------------
# Settings
# -------------------------------
measurements <- c(
  "bmi" = "BMI (kg/m²)",
  "fat_percentage" = "Body Fat (%)",
  "visceral_fat" = "Visceral Fat (g)",
  "waistcircumference" = "Waist Circumference (cm)",
  "WHtR" = "Waist Circumference to Height Ratio"
)

sex_var <- "female"   # 0 = Male, 1 = Female

probs <- seq(0.1, 0.9, by = 0.1)

# -------------------------------
# Rubin combine helper
# -------------------------------
rubin_combine <- function(Q, U) {
  m <- length(Q)
  Qbar <- mean(Q, na.rm = TRUE)
  Ubar <- mean(U, na.rm = TRUE)
  B <- stats::var(Q, na.rm = TRUE)
  T <- Ubar + (1 + 1/m) * B
  se <- sqrt(T)
  tibble(est = Qbar, se = se)
}

# -------------------------------
# Extract from ONE imputed survey design
# -------------------------------
extract_one_design <- function(des, bmi_var, sex_var, probs) {
  
  dat <- des$variables
  sex_vals <- sort(unique(dat[[sex_var]]))   # 0 / 1
  
  out <- list()
  k <- 1
  
  for (sx in sex_vals) {
    
    # Create explicit logical vector for subsetting
    keep_rows <- dat[[sex_var]] == sx
    sub_des <- des[keep_rows, ]
    
    # --- mean BMI ---
    m_obj <- svymean(as.formula(paste0("~", bmi_var)),
                     sub_des, na.rm = TRUE)
    mean_est <- coef(m_obj)[1]
    mean_var <- vcov(m_obj)[1, 1]
    
    # --- deciles ---
    q_obj <- svyquantile(as.formula(paste0("~", bmi_var)),
                         design = sub_des,
                         quantiles = probs,
                         ci = TRUE,
                         se = TRUE,
                         na.rm = TRUE)
    
    q_est <- as.numeric(coef(q_obj))
    q_se <- as.numeric(attr(q_obj, "se"))
    if (length(q_se) == 0) {
      q_se <- rep(NA_real_, length(q_est))
    }
    q_var <- q_se^2
    
    # mean
    out[[k]] <- tibble(
      female = sx,
      stat = "mean",
      prob = NA_real_,
      est = mean_est,
      var = mean_var
    )
    k <- k + 1
    
    # deciles
    out[[k]] <- tibble(
      female = sx,
      stat = "quantile",
      prob = probs,
      est = q_est,
      var = q_var
    )
    k <- k + 1
  }
  
  bind_rows(out)
}

# -------------------------------
# 1) Loop through each measurement
# -------------------------------
for (meas_idx in seq_along(measurements)) {
  
  meas_var <- names(measurements)[meas_idx]
  meas_label <- measurements[meas_idx]
  
  cat("\n=== Processing:", meas_label, "===")
  
  # -------------------------------
  # Extract all imputations
  # -------------------------------
  all_imp <- lapply(
    nhanes_svy_dfs,
    extract_one_design,
    bmi_var = meas_var,
    sex_var = sex_var,
    probs   = probs
  )
  
  all_imp_df <- bind_rows(all_imp, .id = "imp") %>%
    mutate(imp = as.integer(imp))

# -------------------------------
# 2) Rubin combine across imputations
# -------------------------------
combined <- all_imp_df %>%
  group_by(female, stat, prob) %>%
  summarise(Q = list(est), U = list(var), .groups = "drop") %>%
  rowwise() %>%
  mutate(tmp = list(rubin_combine(unlist(Q), unlist(U)))) %>%
  ungroup() %>%
  unnest(tmp) %>%
  mutate(
    lcl = est - 1.96 * se,
    ucl = est + 1.96 * se,
    sex_label = ifelse(female == 1, "Female", "Male")
  )

# -------------------------------
# 3) Prepare plotting data
# -------------------------------
mean_df <- combined %>%
  dplyr::filter(stat == "mean") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female"))
  ) %>%
  arrange(sex_label)

q_df <- combined %>%
  dplyr::filter(stat == "quantile") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female"))
  ) %>%
  arrange(sex_label, prob)

# Create x positions
group_df <- mean_df %>%
  mutate(x = row_number()) %>%
  select(female, sex_label, x)

mean_df <- mean_df %>%
  left_join(group_df %>% select(female, x), by = "female")

q_df <- q_df %>%
  left_join(group_df %>% select(female, x), by = "female")

# Extract key deciles for each group
decile_10th <- q_df %>% dplyr::filter(prob == 0.1)
decile_25th <- q_df %>% dplyr::filter(prob == 0.2)
decile_75th <- q_df %>% dplyr::filter(prob == 0.8)
decile_90th <- q_df %>% dplyr::filter(prob == 0.9)

# Create data for bar ranges (from 10th to 90th decile)
bar_df <- decile_10th %>%
  select(female, sex_label, x, ymin = est) %>%
  left_join(
    decile_90th %>% select(female, ymax = est),
    by = "female"
  )

# Create label data (mean, 10th, 25th, 75th, 90th)
label_df <- bind_rows(
  mean_df %>% mutate(label_type = "Mean"),
  decile_10th %>% mutate(label_type = "10th"),
  decile_25th %>% mutate(label_type = "25th"),
  decile_75th %>% mutate(label_type = "75th"),
  decile_90th %>% mutate(label_type = "90th")
) %>%
  mutate(
    label = sprintf("%.1f", est),
    x_nudge = x + 0.4  # position labels to the right of bars
  )

# -------------------------------
# 4) Plot
# -------------------------------
bar_width <- 0.7

p <- ggplot() +
  # Bars showing 10th-90th decile range
  geom_rect(
    data = bar_df,
    aes(xmin = x - bar_width/2, xmax = x + bar_width/2,
        ymin = ymin, ymax = ymax),
    fill = "steelblue",
    alpha = 0.5
  ) +
  # Horizontal lines for each decile
  geom_segment(
    data = q_df,
    aes(x = x - bar_width/2, xend = x + bar_width/2,
        y = est, yend = est),
    linewidth = 0.6,
    color = "black"
  ) +
  # Points at deciles
  geom_point(
    data = q_df,
    aes(x = x, y = est),
    size = 2,
    color = "black"
  ) +
  # Mean line (slightly thicker)
  geom_segment(
    data = mean_df,
    aes(x = x - bar_width/2, xend = x + bar_width/2,
        y = est, yend = est),
    linewidth = 1,
    color = "red"
  ) +
  # Labels for mean, 10th, 25th, 75th, and 90th
  geom_text(
    data = label_df,
    aes(x = x_nudge, y = est, label = label, color = label_type),
    hjust = 0,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("Mean" = "red", "10th" = "darkblue", "25th" = "purple",
               "75th" = "purple", "90th" = "darkblue"),
    name = ""
  ) +
  scale_x_continuous(
    breaks = group_df$x,
    labels = as.character(group_df$sex_label),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(
    x = NULL,
    y = meas_label,
    title = paste0("Survey-weighted ", meas_label, ":\n", "Mean and Deciles (10th-90th) by Sex"),
    subtitle = "Bars show 10th-90th decile range; horizontal lines show each decile; red line = mean"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )
  
  print(p)
  
  # Save plot
  plot_filename <- paste0(gsub("[^A-Za-z0-9]", "_", meas_label), "_deciles_by_sex.png")
  ggsave(filename = paste0(path_nhanes_dmbf_folder, "/figures/complete cases/", plot_filename),
         plot = p, width = 10, height = 6, dpi = 300)
  cat("\nSaved:", plot_filename)
  
} # End loop over measurements
