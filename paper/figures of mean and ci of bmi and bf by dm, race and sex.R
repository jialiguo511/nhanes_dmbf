rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
source("analysis/dbw03_mean and ci of lab measurements by dm and race.R")

bmi_racesex <- mean_bmi_racesex %>% 
  mutate(sex = factor(sex, levels = c("female", "male")),
         race_eth = factor(race_eth, levels = c("NHWhite","NHBlack","Hispanic","Asian")))


bmi_plot <- ggplot(bmi_racesex, aes(y = race_eth, x = estimate, color = race_eth)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  facet_grid(dm ~ sex, scales = "free", space = "free") +
  labs(x = "BMI (kg/m^2)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red", "purple"))

ggsave(bmi_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of bmi by dm, race and sex.jpg"),width=12,height =5.5)


fat_racesex <- mean_fat_racesex %>% 
  mutate(sex = factor(sex, levels = c("female", "male")),
         race_eth = factor(race_eth, levels = c("NHWhite","NHBlack","Hispanic","Asian")))


fat_plot <- ggplot(fat_racesex, aes(y = race_eth, x = estimate, color = race_eth)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  facet_grid(dm ~ sex, scales = "free", space = "free") +
  labs(x = "Body Fat (%)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red", "purple"))

ggsave(fat_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of body fat by dm, race and sex.jpg"),width=12,height =5.5)


















