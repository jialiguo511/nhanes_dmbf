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
       y = "Total Body Fat (%)",
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
  ggsave(.,filename=paste0(path_nhanes_dmbf_folder,"/figures/complete cases/trend of BMI, BF, viceral fat by year.jpg"),width=18,height = 6)

