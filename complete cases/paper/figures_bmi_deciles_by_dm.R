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
bmi_var <- "bmi"
dm_var  <- "dm"       # diabetes status

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
extract_one_design <- function(des, bmi_var, dm_var, probs) {
  
  dat <- des$variables
  dm_vals  <- sort(unique(dat[[dm_var]]))
  
  out <- list()
  k <- 1
  
  for (dm_val in dm_vals) {
    
    # Create explicit logical vector for subsetting
    keep_rows <- dat[[dm_var]] == dm_val
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
      dm = dm_val,
      stat = "mean",
      prob = NA_real_,
      est = mean_est,
      var = mean_var
    )
    k <- k + 1
    
    # deciles
    out[[k]] <- tibble(
      dm = dm_val,
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
# 1) Extract all imputations
# -------------------------------
all_imp <- lapply(
  nhanes_svy_dfs,
  extract_one_design,
  bmi_var = bmi_var,
  dm_var  = dm_var,
  probs   = probs
)

all_imp_df <- bind_rows(all_imp, .id = "imp") %>%
  mutate(imp = as.integer(imp))

# -------------------------------
# 2) Rubin combine across imputations
# -------------------------------
combined <- all_imp_df %>%
  group_by(dm, stat, prob) %>%
  summarise(Q = list(est), U = list(var), .groups = "drop") %>%
  rowwise() %>%
  mutate(tmp = list(rubin_combine(unlist(Q), unlist(U)))) %>%
  ungroup() %>%
  unnest(tmp) %>%
  mutate(
    lcl = est - 1.96 * se,
    ucl = est + 1.96 * se,
    dm_label = dm
  )

# -------------------------------
# 3) Prepare plotting data
# -------------------------------
mean_df <- combined %>%
  dplyr::filter(stat == "mean") %>%
  mutate(
    dm_label = factor(dm_label, levels = c("NoDM", "PreDM", "DM"))
  ) %>%
  arrange(dm_label)

q_df <- combined %>%
  dplyr::filter(stat == "quantile") %>%
  mutate(
    dm_label = factor(dm_label, levels = c("NoDM", "PreDM", "DM"))
  ) %>%
  arrange(dm_label, prob)

# Create x positions
group_df <- mean_df %>%
  mutate(x = row_number()) %>%
  select(dm, dm_label, x)

mean_df <- mean_df %>%
  left_join(group_df %>% select(dm, x), by = "dm")

q_df <- q_df %>%
  left_join(group_df %>% select(dm, x), by = "dm")

# Extract 10th and 90th deciles for each group
decile_10th <- q_df %>% dplyr::filter(prob == 0.1)
decile_90th <- q_df %>% dplyr::filter(prob == 0.9)

# Create data for bar ranges (from 10th to 90th decile)
bar_df <- decile_10th %>%
  select(dm, dm_label, x, ymin = est) %>%
  left_join(
    decile_90th %>% select(dm, ymax = est),
    by = "dm"
  )

# Create label data (mean, 10th, 90th)
label_df <- bind_rows(
  mean_df %>% mutate(label_type = "Mean"),
  decile_10th %>% mutate(label_type = "10th"),
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
  # Labels for mean, 10th, and 90th
  geom_text(
    data = label_df,
    aes(x = x_nudge, y = est, label = label, color = label_type),
    hjust = 0,
    size = 3,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("Mean" = "red", "10th" = "darkblue", "90th" = "darkblue"),
    name = ""
  ) +
  scale_x_continuous(
    breaks = group_df$x,
    labels = as.character(group_df$dm_label),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(
    x = NULL,
    y = "BMI (kg/m²)",
    title = "Survey-weighted BMI: Mean and Deciles (10th-90th) by Diabetes Status",
    subtitle = "Bars show 10th-90th decile range; horizontal lines show each decile; red line = mean"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

print(p)

# Save plot
# ggsave("bmi_deciles_by_dm.png", p, width = 8, height = 6, dpi = 300)
