rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

fig_df <- read_csv("analysis/dbw02d_descriptive characteristics by dm and year.csv") %>% 
  mutate(year = factor(year, levels = c("2011-2012","2013-2014","2015-2016","2017-2018")),
         dm = factor(dm, levels = c("non-diabetes","newly and undiagnosed diabetes","diagnosed diabetes >1y"),
                     labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y"))) %>% 
  mutate(value = paste0(round(estimate, 1), " (",
                        round(CI_lower, 1), ", ",
                        round(CI_upper, 1), ")"))


bmi_year <- fig_df %>% dplyr::filter(variable == "BMI")

fig_bmi <- ggplot(data=bmi_year,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 

bf_year <- fig_df %>% dplyr::filter(variable == "Fat percentage")

fig_bf <- ggplot(data=bf_year,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "Body Fat (%)",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 


library(ggpubr)
ggarrange(fig_bmi,
          fig_bf,
          common.legend = TRUE,
          legend = "bottom",
          nrow = 1,
          ncol = 2) %>% 
  ggsave(.,filename=paste0(path_nhanes_dmbf_folder,"/figures/trend of BMI and BF by year.jpg"),width=12,height = 8)

#-------------------------------------------------------------------------

fig_df <- read_csv("analysis/dbw02e_descriptive characteristics by dm, sex and year.csv") %>% 
  mutate(sex = factor(sex, levels = c("male", "female")),
         year = factor(year, levels = c("2011-2012","2013-2014","2015-2016","2017-2018")),
         dm = factor(dm, levels = c("non-diabetes","newly and undiagnosed diabetes","diagnosed diabetes >1y"),
                     labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y"))) %>% 
  mutate(value = paste0(round(estimate, 1), " (",
                        round(CI_lower, 1), ", ",
                        round(CI_upper, 1), ")"))


bmi_year_female <- fig_df %>% dplyr::filter(variable == "BMI" & sex == "female")

fig_bmi_female <- ggplot(data=bmi_year_female,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       title = "Female",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 

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
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 


bmi_year_male <- fig_df %>% dplyr::filter(variable == "BMI" & sex == "male")

fig_bmi_male <- ggplot(data=bmi_year_male,aes(x=year,ymin=CI_lower,ymax=CI_upper,y=estimate,col=dm,fill=dm,group=dm)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  labs(x = "Year",
       y = "BMI (kg/m^2)",
       title = "Male",
       color = "Diabetes Status",
       fill = "Diabetes Status") +
  theme_bw() + 
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 

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
  scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) +
  scale_fill_manual(values = c("#FFCCCC", "#CCCCFF", "#CCFFCC"), labels = c("No Diabetes","New (≤1y) & Undiagnosed","Diabetes>1y")) 


library(ggpubr)
ggarrange(fig_bmi_male,
          fig_bf_male,
          fig_bmi_female,
          fig_bf_female,
          common.legend = TRUE,
          legend = "bottom",
          nrow = 2,
          ncol = 2) %>% 
  ggsave(.,filename=paste0(path_nhanes_dmbf_folder,"/figures/trend of BMI and BF by year and sex.jpg"),width=12,height = 8)


