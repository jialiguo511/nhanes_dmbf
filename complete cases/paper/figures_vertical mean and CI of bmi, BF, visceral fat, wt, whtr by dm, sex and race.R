rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

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
  labs(x = NULL, y = "BMI (kg/m²)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )

ggsave(bmi_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of bmi by dm, sex and race.jpg"),width=8,height =6)


fat_racesex <-  fig_df %>% dplyr::filter(variable == "Fat percentage")

fat_plot <- ggplot(fat_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Total Body Fat (%)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(fat_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of bf by dm, sex and race.jpg"),width=8,height =6)


visfat_racesex <- fig_df %>% dplyr::filter(variable == "Visceral fat mass")

visfat_plot <- ggplot(visfat_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Visceral fat mass (g)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(visfat_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of visceral fat by dm, sex and race.jpg"),width=8,height =6)

wt_racesex <- fig_df %>% dplyr::filter(variable == "Waist circumference")

wt_plot <- ggplot(wt_racesex, aes(x = dm, y = estimate, shape = dm, color = dm)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +
  scale_shape_manual(name = "Diabetes status", values = shape_vals) +
  scale_color_manual(name = "Diabetes status", values = color_vals) +
  labs(x = NULL, y = "Waist circumference (cm)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(wt_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of wt by dm, sex and race.jpg"),width=8,height =6)


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
  labs(x = NULL, y = "Waist-to-Height ratio") +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 12)
  )


ggsave(whtr_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of whtr by dm, sex and race.jpg"),width=8,height =6)


