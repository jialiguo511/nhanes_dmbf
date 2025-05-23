rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("complete cases/analysis/dbwse03g_descriptive characteristics by dm - body compositions.csv") %>% 
  mutate(dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        trimws(format(round(CI_lower, 1), nsmall = 1)), ", ",
                        trimws(format(round(CI_upper, 1), nsmall = 1)), ")"))


var_labels <- c(
  "BMI" = "BMI (kg/m²)",
  "Fat percentage" = "Fat Percentage (%)",
  "Visceral fat mass" = "Visceral Fat (g)"
)

plot <- ggplot(fig_df, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 4, shape = 21, color = "black") +  # shape 21 uses fill
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(variable = var_labels)) +
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # Add panel border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # Space between panels
    panel.spacing = unit(0.8, "lines"),
    
    # Move legend title to bottom
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    
    # Optional: strip appearance
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )

ggsave(plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/mean and ci of bmi, BF and visceral fat mass by dm.jpg"),width=8,height =6)

