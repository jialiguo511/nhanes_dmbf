rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("NHBlack","Hispanic","NHWhite","Asian")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        trimws(format(round(CI_lower, 1), nsmall = 1)), ", ",
                        trimws(format(round(CI_upper, 1), nsmall = 1)), ")"))


bmi_racesex <- fig_df %>% dplyr::filter(variable == "BMI")

bmi_plot <- ggplot(bmi_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "BMI (kg/m^2)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13, face = "bold"), 
        strip.text.y = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  xlim(20, 40)

ggsave(bmi_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/forest plot of mean and ci of bmi by dm, race and sex.jpg"),width=12,height =8)



fat_racesex <-  fig_df %>% dplyr::filter(variable == "Fat percentage") 

fat_plot <- ggplot(fat_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Total Body Fat (%)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13, face = "bold"), 
        strip.text.y = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  xlim(20, 45)

ggsave(fat_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/forest plot of mean and ci of body fat by dm, race and sex.jpg"),width=12,height =8)



visfat_racesex <- fig_df %>% dplyr::filter(variable == "Visceral fat mass")

visfat_plot <- ggplot(visfat_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Visceral fat mass (g)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13, face = "bold"), 
        strip.text.y = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  xlim(250, 910)

ggsave(visfat_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/forest plot of mean and ci of visceral fat mass by dm, race and sex.jpg"),width=12,height =8)



wt_racesex <- fig_df %>% dplyr::filter(variable == "Waist circumference")

wt_plot <- ggplot(wt_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Waist circumference (cm)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13, face = "bold"), 
        strip.text.y = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  xlim(75, 120)

ggsave(wt_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/forest plot of mean and ci of waist circumference by dm, race and sex.jpg"),width=12,height =8)



fig_df <- read_csv("complete cases/analysis/dbwse03c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("MEN","WOMEN")),
         race_eth = factor(race_eth, levels = c("NHBlack","Hispanic","NHWhite","Asian")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 2), nsmall = 2), " (",
                        trimws(format(round(CI_lower, 2), nsmall = 2)), ", ",
                        trimws(format(round(CI_upper, 2), nsmall = 2)), ")"))

whtr_racesex <- fig_df %>% dplyr::filter(variable == "Waist-to-Height ratio")

whtr_plot <- ggplot(whtr_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Waist-to-Height ratio", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13, face = "bold"), 
        strip.text.y = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_x_continuous(
    limits = c(0.45, 0.75),
    breaks = seq(0.45, 0.75, by = 0.05)
  )

ggsave(whtr_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/forest plot of mean and ci of waist-to-height ratio by dm, race and sex.jpg"),width=12,height =8)
