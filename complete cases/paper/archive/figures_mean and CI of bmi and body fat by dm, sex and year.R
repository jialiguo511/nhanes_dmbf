rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("complete cases/analysis/dbwse03d_descriptive characteristics by dm and year.csv") %>% 
  mutate(year = factor(year, levels = c("2011-2012","2013-2014","2015-2016","2017-2018")),
         dm = factor(dm, levels = c("NoDM","PreDM","DM"))) %>% 
  mutate(value = paste0(format(round(estimate, 1), nsmall = 1), " (",
                        format(round(CI_lower, 1), nsmall = 1), ", ",
                        format(round(CI_upper, 1), nsmall = 1), ")"))


bmi_year <- fig_df %>% dplyr::filter(variable == "BMI")

fig_bmi <- ggplot(data=bmi_year,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC")) 


bf_year <- fig_df %>% dplyr::filter(variable == "Fat percentage")

fig_bf <- ggplot(data=bf_year,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Body Fat (%)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC")) 


visfat_year <- fig_df %>% dplyr::filter(variable == "Visceral fat mass")

fig_visfat <- ggplot(data=visfat_year,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Visceral Fat Mass (g)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"))


library(ggpubr)
ggarrange(fig_bmi,
          fig_bf,
          fig_visfat,
          common.legend = TRUE,
          legend = "bottom",
          nrow = 1,
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/trend of BMI, BF, viceral fat by year.jpg"),width=14,height = 6)

#-------------------------------------------------------------------------

fig_df <- read_csv("complete cases/analysis/dbwse03e_descriptive characteristics by dm, sex and year.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female")),
         year = factor(year, levels = c("2011-2012","2013-2014","2015-2016","2017-2018")),
         dm = factor(dm, levels = c("NoDM","PreDM","NewDM","DM"))) %>%  
  mutate(value = paste0(round(estimate, 1), " (",
                        round(CI_lower, 1), ", ",
                        round(CI_upper, 1), ")"))


bmi_year_female <- fig_df %>% dplyr::filter(variable == "BMI" & sex == "female")

fig_bmi_female <- ggplot(data=bmi_year_female,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       title = "WOMEN",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 13)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"))

bf_year_female <- fig_df %>% dplyr::filter(variable == "Fat percentage" & sex == "female")

fig_bf_female <- ggplot(data=bf_year_female,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Body Fat (%)",
       title = "",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 13)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"))

visfat_year_female <- fig_df %>% dplyr::filter(variable == "Visceral fat mass" & sex == "female")

fig_visfat_female <- ggplot(data=visfat_year_female,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Visceral Fat Mass (g)",
       title = "",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 13)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC")) 



bmi_year_male <- fig_df %>% dplyr::filter(variable == "BMI" & sex == "male")

fig_bmi_male <- ggplot(data=bmi_year_male,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       title = "MEN",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 13)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"))

bf_year_male <- fig_df %>% dplyr::filter(variable == "Fat percentage" & sex == "male")

fig_bf_male <- ggplot(data=bf_year_male,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Body Fat (%)",
       title = "",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), # Increase title font size
    axis.title = element_text(size = 14), # Increase axis labels font size
    axis.text = element_text(size = 12), # Increase axis text size
    legend.title = element_text(size = 13), # Increase legend title size
    legend.text = element_text(size = 13)  # Increase legend text size
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC")) 

visfat_year_male <- fig_df %>% dplyr::filter(variable == "Visceral fat mass" & sex == "male")

fig_visfat_male <- ggplot(data=visfat_year_male,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Visceral Fat Mass (g)",
       title = "",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 13)  
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC")) 


library(ggpubr)
ggarrange(fig_bmi_male,
          fig_bf_male,
          fig_visfat_male,
          fig_bmi_female,
          fig_bf_female,
          fig_visfat_female,
          common.legend = TRUE,
          legend = "bottom",
          nrow = 2,
          ncol = 3) %>% 
  ggsave(.,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/trend of BMI, BF and visceral fat by year and sex one dm group.jpg"),width=16,height = 8)

