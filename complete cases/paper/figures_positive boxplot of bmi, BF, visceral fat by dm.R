rm(list=ls());gc();source(".Rprofile")

library(survey)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 


# 1. Build a single tibble of predictions from all imputed designs
df_all_pred <- map_dfr(nhanes_svy_dfs, ~{
  svy  <- .
  df   <- as_tibble(svy$variables) %>%
    select(bmi, fat_percentage, visceral_fat,
           age, race_eth, dm_sex)
  
  # 2. Fit three survey models
  mod_bmi <- svyglm(bmi            ~ age + race_eth, design = svy)
  mod_fat <- svyglm(fat_percentage ~ age + race_eth, design = svy)
  mod_vis <- svyglm(visceral_fat   ~ age + race_eth, design = svy)
  
  # 3. Add predicted values on the original scale
  df %>%
    mutate(
      bmi_pred = as.numeric(predict(mod_bmi, newdata = df)),
      fat_pred = as.numeric(predict(mod_fat, newdata = df)),
      vis_pred = as.numeric(predict(mod_vis, newdata = df))
    ) %>%
    select(dm_sex, ends_with("_pred")) %>%
    pivot_longer(
      cols      = -dm_sex,
      names_to  = "variable",
      values_to = "predicted"
    ) %>%
    mutate(
      variable = recode(variable,
                        bmi_pred = "BMI (kg/m²)",
                        fat_pred = "Fat Percentage (%)",
                        vis_pred = "Visceral Fat (g)")
    )
})

# 4. Separate dm_sex and factor the dm levels
fig_df <- df_all_pred %>%
  separate(dm_sex, into = c("sex", "dm"), sep = " ", remove = FALSE) %>%
  mutate(dm = factor(dm, levels = c("NoDM","PreDM","DM")))

# 5. Plot the three‐panel boxplot of fitted values
positive_box_plot <- ggplot(fig_df, 
       aes(x = interaction(dm, sex, sep = "\n"),
           y = predicted,
           fill = dm)
) +
  geom_boxplot(color = "black", outlier.size = 1, width = 0.6) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(
    name   = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
  labs(
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing    = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 0, hjust = 1),
    legend.position  = "bottom",
    legend.box       = "horizontal"
  )


ggsave(positive_box_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/positive boxplot of bmi, BF and visceral fat mass by dm, race and sex.jpg"),width=12,height =8)

