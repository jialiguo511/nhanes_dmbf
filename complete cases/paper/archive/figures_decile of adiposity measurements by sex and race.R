rm(list=ls());gc();source(".Rprofile")

library(survey)
library(ggplot2)


nhanes_svy_dfs <- readRDS(
  paste0(path_nhanes_dmbf_folder,
         "/working/cleaned/dbwse02_weighted df with complete cases.RDS")
)

# -------------------------------
# Settings
# -------------------------------
bmi_var <- "bmi"
sex_var <- "female"   # 0 = Male, 1 = Female
dm_var  <- "dm"

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
extract_one_design <- function(des, bmi_var, sex_var, dm_var, probs) {
  
  dat <- des$variables
  sex_vals <- sort(unique(dat[[sex_var]]))   # 0 / 1
  dm_vals  <- sort(unique(dat[[dm_var]]))
  
  out <- list()
  k <- 1
  
  for (sx in sex_vals) {
    for (dm in dm_vals) {
      
      sub_des <- subset(
        des,
        des$variables[[sex_var]] == sx &
          des$variables[[dm_var]]  == dm
      )
      
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
        dm = dm,
        stat = "mean",
        prob = NA_real_,
        est = mean_est,
        var = mean_var
      )
      k <- k + 1
      
      # deciles
      out[[k]] <- tibble(
        female = sx,
        dm = dm,
        stat = "quantile",
        prob = probs,
        est = q_est,
        var = q_var
      )
      k <- k + 1
    }
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
  sex_var = sex_var,
  dm_var  = dm_var,
  probs   = probs
)

all_imp_df <- bind_rows(all_imp, .id = "imp") %>%
  mutate(imp = as.integer(imp))

# -------------------------------
# 2) Rubin combine
# -------------------------------
combined <- all_imp_df %>%
  group_by(female, dm, stat, prob) %>%
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
  filter(stat == "mean") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female")),
    dm = factor(dm)
  ) %>%
  arrange(sex_label, dm) %>%
  mutate(
    group = interaction(sex_label, dm, sep = " | "),
    x = as.numeric(group)
  )

q_df <- combined %>%
  filter(stat == "quantile") %>%
  mutate(
    sex_label = factor(sex_label, levels = c("Male", "Female")),
    dm = factor(dm)
  ) %>%
  arrange(sex_label, dm, prob) %>%
  mutate(
    group = interaction(sex_label, dm, sep = " | "),
    x_center = as.numeric(group)
  )

# spread deciles inside each bar
spread_width <- 0.28
q_df <- q_df %>%
  mutate(x = x_center + scales::rescale(prob,
                                        to = c(-spread_width, spread_width)))

# -------------------------------
# 4) Plot
# -------------------------------
p <- ggplot() +
  geom_col(
    data = mean_df,
    aes(x = x, y = est),
    width = 0.75,
    alpha = 0.7
  ) +
  geom_errorbar(
    data = mean_df,
    aes(x = x, ymin = lcl, ymax = ucl),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_line(
    data = q_df,
    aes(x = x, y = est, group = group),
    linewidth = 0.7
  ) +
  geom_point(
    data = q_df,
    aes(x = x, y = est),
    size = 1.8
  ) +
  scale_x_continuous(
    breaks = mean_df$x,
    labels = as.character(mean_df$group),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "BMI",
    title = "Survey-weighted mean BMI with deciles (10th–90th)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(p)

# ggsave("bmi_mean_deciles_female.png", p, width = 11, height = 5, dpi = 300)
