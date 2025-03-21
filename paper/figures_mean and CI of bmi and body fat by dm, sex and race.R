rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("analysis/dbw03c_descriptive characteristics by dm, sex and race.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female"), labels = c("Men","Women")),
         race_eth = factor(race_eth, levels = c("NHBlack","Hispanic","NHWhite","Asian")),
         dm = factor(dm, levels = c("normal","prediabetes","newly and undiagnosed diabetes","diagnosed diabetes >1y"),
                     labels = c("Normal","Prediabetes","New Diabetes","Established Diabetes"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        format(round(CI_lower, 1), nsmall = 1), ", ",
                        format(round(CI_upper, 1), nsmall = 1), ")"))


bmi_racesex <- fig_df %>% dplyr::filter(variable == "BMI")

bmi_plot <- ggplot(bmi_racesex, aes(y = race_eth, x = estimate, color = dm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower-1, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, fontface = "bold", color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "BMI (kg/m^2)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("red", "blue", "darkgreen","orange")) 

ggsave(bmi_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of bmi by dm, race and sex.jpg"),width=12,height =8)



fat_racesex <-  fig_df %>% dplyr::filter(variable == "Fat percentage") 

fat_plot <- ggplot(fat_racesex, aes(y = race_eth, x = estimate, color = race_eth)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_text(aes(label = value), vjust = -0.8, hjust = 0.4, size = 4, fontface = "bold", color = "black") +
  facet_grid(dm ~ sex, scales = "fixed", space = "fixed") +
  labs(x = "Body Fat (%)", y = "Race/Ethnicity") +
  theme_bw() +
  theme(panel.spacing = grid::unit(1, "lines"),
        strip.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("red", "blue", "darkgreen","orange")) 

ggsave(fat_plot,filename=paste0(path_nhanes_dmbf_folder,"/figures/forest plot of mean and ci of body fat by dm, race and sex.jpg"),width=12,height =8)
