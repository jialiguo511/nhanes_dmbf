rm(list=ls());gc();source(".Rprofile")

library(survey)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbwse02_weighted df with complete cases.RDS")) 

# 1. map over your list of survey‐designs, 
#    build each long tibble, then row‐bind them into one df
df_all <- map_dfr(nhanes_svy_dfs, ~{
  svy <- .
  
  # pull out raw variables
  df <- as_tibble(svy$variables) %>% 
    select(bmi, fat_percentage, visceral_fat, age, race_eth, dm_sex)
  
  # fit models & get “adjusted” values
  df %>% 
    mutate(
      bmi_adj = resid(svyglm(bmi            ~ age + race_eth, design = svy)) +
        mean(bmi, na.rm = TRUE),
      fat_adj = resid(svyglm(fat_percentage ~ age + race_eth, design = svy)) +
        mean(fat_percentage, na.rm = TRUE),
      vis_adj = resid(svyglm(visceral_fat   ~ age + race_eth, design = svy)) +
        mean(visceral_fat, na.rm = TRUE)
    ) %>%
    select(dm_sex, ends_with("_adj")) %>%
    pivot_longer(
      cols      = -dm_sex,
      names_to  = "variable",
      values_to = "adjusted"
    ) %>%
    mutate(
      variable = recode(variable,
                        bmi_adj = "BMI (kg/m^2)",
                        fat_adj = "Total Body Fat (%)",
                        vis_adj = "Visceral Fat (g)")
    )
})

fig_df <- df_all %>% 
  separate(
    col    = dm_sex, 
    into   = c("sex", "dm"), 
    sep    = " ",       # split at the space
    remove = FALSE      # keep the original dm_sex column
  ) %>% 
  mutate(
    dm = factor(dm, levels = c("NoDM","PreDM","DM"))
  )


box_plot <- ggplot(fig_df, 
       aes(
         x    = interaction(dm, sex, sep = "\n"),   # ← compute here
         y    = adjusted,
         fill = dm
       )
) +
  geom_boxplot(
    color        = "black",  # outline
    outlier.size = 1,
    width        = 0.6
  ) +
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

ggsave(box_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/boxplot of bmi, BF and visceral fat mass by dm, race and sex.jpg"),width=12,height =8)


