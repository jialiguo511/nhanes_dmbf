rm(list=ls());gc();source(".Rprofile")

library(survey)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load imputed datasets
nhanes_svy_dfs <- readRDS(
  paste0(path_nhanes_dmbf_folder,
         "/working/cleaned/dbwse02_weighted df with complete cases.RDS")
)

# Settings
bmi_var <- "bmi"
sex_var <- "female"
dm_var  <- "dm"
probs <- seq(0.1, 0.9, by = 0.1)

# Extract means and quantiles from one design
extract_stats <- function(des, bmi_var, sex_var, dm_var, probs) {
  
  results <- list()
  
  for (sx in c(0, 1)) {
    for (dm in c("NoDM", "PreDM", "DM")) {
      
      sub_des <- subset(
        des,
        des$variables[[sex_var]] == sx &
          des$variables[[dm_var]] == dm
      )
      
      if (nrow(sub_des) == 0) next
      
      # Get mean
      m_obj <- svymean(as.formula(paste0("~", bmi_var)), sub_des, na.rm = TRUE)
      mean_est <- coef(m_obj)[1]
      mean_se <- SE(m_obj)[1]
      
      results[[paste(sx, dm, "mean", sep="_")]] <- data.frame(
        female = sx,
        dm = dm,
        stat = "mean",
        prob = NA_real_,
        est = mean_est,
        se = mean_se
      )
      
      # Get quantiles - directly from raw data
      bmi_values <- sub_des$variables[[bmi_var]]
      bmi_values <- bmi_values[!is.na(bmi_values)]
      
      if (length(bmi_values) > 0) {
        for (p in probs) {
          q_val <- quantile(bmi_values, p, type = 5)
          results[[paste(sx, dm, "q", p, sep="_")]] <- data.frame(
            female = sx,
            dm = dm,
            stat = "quantile",
            prob = p,
            est = as.numeric(q_val),
            se = NA_real_
          )
        }
      }
    }
  }
  
  bind_rows(results)
}

# Extract from all imputations
all_results <- lapply(nhanes_svy_dfs, extract_stats, 
                      bmi_var = bmi_var,
                      sex_var = sex_var,
                      dm_var = dm_var,
                      probs = probs)

all_df <- bind_rows(all_results, .id = "imp") %>%
  mutate(imp = as.integer(imp))

# Pool results using Rubin's rules
combined <- all_df %>%
  group_by(female, dm, stat, prob) %>%
  summarise(
    Q = list(est),
    U = list(se^2),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    m = length(unlist(Q)),
    Q_bar = mean(unlist(Q), na.rm = TRUE),
    U_bar = mean(unlist(U), na.rm = TRUE),
    B = var(unlist(Q), na.rm = TRUE),
    B = ifelse(is.na(B), 0, B),
    T = U_bar + (1 + 1/m) * B,
    se = sqrt(T),
    lcl = Q_bar - 1.96 * se,
    ucl = Q_bar + 1.96 * se,
    est = Q_bar,
    sex_label = ifelse(female == 1, "Female", "Male")
  ) %>%
  ungroup() %>%
  select(female, dm, stat, prob, est, se, lcl, ucl, sex_label)

cat("Combined data:\n")
print(head(combined))
cat("Unique stat values:", unique(combined$stat), "\n")
cat("Quantile estimates - NAs:", sum(is.na(combined$est[combined$stat == "quantile"])), "\n")
cat("Quantile estimates - non-NAs:", sum(!is.na(combined$est[combined$stat == "quantile"])), "\n")

# Prepare plotting data
mean_df <- combined %>%
  dplyr::filter(stat == "mean") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female")),
    dm = factor(dm, levels = c("NoDM", "PreDM", "DM"),
                labels = c("No DM", "Pre-DM", "DM"))
  ) %>%
  arrange(sex_label, dm) %>%
  mutate(
    group = interaction(sex_label, dm, sep = " | "),
    x = as.numeric(group)
  )

q_df <- combined %>%
  dplyr::filter(stat == "quantile") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female")),
    dm = factor(dm, levels = c("NoDM", "PreDM", "DM"),
                labels = c("No DM", "Pre-DM", "DM"))
  ) %>%
  arrange(sex_label, dm, prob) %>%
  mutate(
    group = interaction(sex_label, dm, sep = " | ")
  )

# Add x_center mapping from mean_df to q_df
x_mapping <- mean_df %>% select(group, x) %>% distinct()
q_df <- q_df %>% left_join(x_mapping, by = "group")
colnames(q_df)[colnames(q_df) == "x"] <- "x_center"

# Plot
p <- ggplot() +
  geom_col(
    data = mean_df,
    aes(x = x, y = est),
    width = 0.75,
    alpha = 0.7,
    fill = "steelblue"
  ) +
  geom_errorbar(
    data = mean_df,
    aes(x = x, ymin = lcl, ymax = ucl),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_segment(
    data = q_df,
    aes(x = x_center - 0.35, xend = x_center + 0.35, y = est, yend = est),
    linewidth = 0.6,
    color = "darkred",
    alpha = 0.8
  ) +
  scale_x_continuous(
    breaks = mean_df$x,
    labels = as.character(mean_df$group),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "BMI",
    title = "Mean BMI with deciles (10th-90th) by Female and DM Status"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 13)
  )

print(p)

# ggsave("figure_deciles.png", p, width = 12, height = 6, dpi = 300)
