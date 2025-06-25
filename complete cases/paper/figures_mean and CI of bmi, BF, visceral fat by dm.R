rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("complete cases/analysis/dbwse03g_descriptive characteristics by dm - body compositions.csv") %>% 
  mutate(dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        trimws(format(round(CI_lower, 1), nsmall = 1)), ", ",
                        trimws(format(round(CI_upper, 1), nsmall = 1)), ")")) %>% 
  dplyr::filter(variable %in% c("BMI","Fat percentage","Visceral fat mass"))


var_labels <- c(
  "BMI" = "BMI (kg/m²)",
  "Fat percentage" = "Fat Percentage (%)",
  "Visceral fat mass" = "Visceral Fat (g)"
)

shape_vals <- c(NoDM = 16, PreDM = 17, DM = 15)  # circle, triangle, square
color_vals <- c(NoDM = "red", PreDM = "blue", DM = "darkgreen")

plot <- ggplot(fig_df, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(variable = var_labels)) +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/mean and ci of bmi, BF and visceral fat mass by dm.jpg"),width=8,height =6)

