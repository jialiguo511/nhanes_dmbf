rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv")  %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("Asian","NHWhite","Hispanic","NHBlack")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        trimws(format(round(CI_lower, 1), nsmall = 1)), ", ",
                        trimws(format(round(CI_upper, 1), nsmall = 1)), ")"))


bmi_racesex <- fig_df %>% dplyr::filter(variable == "BMI")

bmi_plot <- ggplot(bmi_racesex, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +  # same y-axis
  # scale_y_continuous(limits = c(20, 40), breaks = seq(20, 40, by = 5)) +
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
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

fat_plot <- ggplot(fat_racesex, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +  # same y-axis
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
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


ggsave(fat_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of bf by dm, sex and race.jpg"),width=8,height =6)


visfat_racesex <- fig_df %>% dplyr::filter(variable == "Visceral fat mass")

visfat_plot <- ggplot(visfat_racesex, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +  # same y-axis
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
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


ggsave(visfat_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of visceral fat by dm, sex and race.jpg"),width=8,height =6)

wt_racesex <- fig_df %>% dplyr::filter(variable == "Waist circumference")

wt_plot <- ggplot(wt_racesex, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +  # same y-axis
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
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


ggsave(wt_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of wt by dm, sex and race.jpg"),width=8,height =6)


fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("NHBlack","Hispanic","NHWhite","Asian")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 2), nsmall = 2), " (",
                        trimws(format(round(CI_lower, 2), nsmall = 2)), ", ",
                        trimws(format(round(CI_upper, 2), nsmall = 2)), ")"))

whtr_racesex <- fig_df %>% dplyr::filter(variable == "Waist-to-Height ratio")

whtr_plot <- ggplot(whtr_racesex, aes(x = dm, y = estimate, fill = dm)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  facet_grid(sex ~ race_eth, scales = "fixed") +  # same y-axis
  scale_fill_manual(
    name = "Diabetes status",
    values = c(NoDM = "red", PreDM = "blue", DM = "darkgreen")
  ) +
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


ggsave(whtr_plot, filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/vertical mean and ci of whtr by dm, sex and race.jpg"),width=8,height =6)


