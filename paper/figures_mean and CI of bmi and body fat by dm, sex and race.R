rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("analysis/dbw02c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female")),
         race_eth = factor(race_eth, levels = c("NHBlack","Hispanic","NHWhite","Asian")),
         dm = case_when(dm == "newly and undiagnosed diabetes" ~ "New (≤1y) & Undiagnosed",
                        dm == "diagnosed diabetes >1y" ~ "Diabetes>1y",
                        TRUE ~ "No Diabetes"),
         dm = factor(dm, levels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y"))) %>% 
  mutate(value = paste0(round(estimate, 1), " (",
                        round(CI_lower, 1), ", ",
                        round(CI_upper, 1), ")"))


bmi_racesex <- fig_df %>% dplyr::filter(variable == "BMI")

bmi_plot <- ggplot(bmi_racesex, aes(y = race_eth, x = estimate, color = race_eth)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 3, fontface = "bold", color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "BMI (kg/m^2)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red", "purple"))

ggsave(bmi_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of bmi by dm, race and sex.jpg"),width=12,height =5.5)



fat_racesex <-  fig_df %>% dplyr::filter(variable == "Fat percentage") 

fat_plot <- ggplot(fat_racesex, aes(y = race_eth, x = estimate, color = race_eth)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 3, fontface = "bold", color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Body Fat (%)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red", "purple"))

ggsave(fat_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of body fat by dm, race and sex.jpg"),width=12,height =5.5)
