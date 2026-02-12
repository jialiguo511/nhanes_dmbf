rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
library(patchwork)

fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv")  %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("Asian","NHWhite","Hispanic","NHBlack")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        trimws(format(round(CI_lower, 1), nsmall = 1)), ", ",
                        trimws(format(round(CI_upper, 1), nsmall = 1)), ")"))

write.csv(fig_df,"complete cases/paper/table_mean and ci of body compositions with complete cases.csv")

shape_vals <- c(NoDM = 16, PreDM = 17, DM = 15)  # circle, triangle, square
color_vals <- c(NoDM = "red", PreDM = "blue", DM = "darkgreen")

bmi_racesex <- fig_df %>% dplyr::filter(variable == "BMI")

bmi_plot <- ggplot(bmi_racesex, aes(x = dm, y = estimate, shape = dm)) +
  geom_point(size = 3, aes(color = dm)) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = dm), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "BMI (kg/m²)", subtitle = "A. Body Mass Index (BMI)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


fat_racesex <-  fig_df %>% dplyr::filter(variable == "Fat percentage")

fat_plot <- ggplot(fat_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Total Body Fat (%)", subtitle = "B. Total Body Fat (%TBF)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


visfat_racesex <- fig_df %>% dplyr::filter(variable == "Visceral fat mass")

visfat_plot <- ggplot(visfat_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Visceral fat mass (g)", subtitle = "C. Visceral Fat (VF)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )

wt_racesex <- fig_df %>% dplyr::filter(variable == "Waist circumference")

wt_plot <- ggplot(wt_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Waist circumference (cm)", subtitle = "D. Waist Circumference (WC)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("Asian","NHWhite","Hispanic","NHBlack")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 2), nsmall = 2), " (",
                        trimws(format(round(CI_lower, 2), nsmall = 2)), ", ",
                        trimws(format(round(CI_upper, 2), nsmall = 2)), ")")) 

whtr_racesex <- fig_df %>% dplyr::filter(variable == "Waist-to-Height ratio")

whtr_plot <- ggplot(whtr_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Waist-to-Height ratio", subtitle = "E. Waist-to-Height Ratio (WTHR)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )

# Combine all plots into 2x3 grid
combined_plot <- wrap_plots(list(bmi_plot, fat_plot, visfat_plot, wt_plot, whtr_plot), 
                             ncol = 3, nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")


ggsave(combined_plot, 
       filename = paste0(path_nhanes_dmbf_folder, "/figures/complete cases/combined_vertical_mean_ci_by_dm_sex_race.png"),
       width = 22, height = 14, dpi = 300)

