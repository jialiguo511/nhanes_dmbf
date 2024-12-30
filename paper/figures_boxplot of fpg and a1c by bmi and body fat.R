rm(list=ls());gc();source(".Rprofile")

nhanes_total_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS")) %>% 
  dplyr::filter(bmi <= 60) %>% 
  mutate(glucosef_category = case_when(fasting_glucose < 100 ~ "<100",
                                       fasting_glucose >= 100 & fasting_glucose <= 126 ~ "100-126",
                                       TRUE ~ ">126"),
         hba1c_category = case_when(glycohemoglobin < 5.7 ~ "<5.7%",
                                    glycohemoglobin >= 5.7 & glycohemoglobin <= 6.5 ~ "5.7%-6.5%",
                                    glycohemoglobin > 6.5 & glycohemoglobin <= 9 ~ "6.5%-9%",
                                    TRUE ~ ">9%"),
         bmi_category = case_when(bmi<18.5 ~ "<18.5",
                                  bmi>=18.5 & bmi<25 ~ "18.5-24.9",
                                  bmi>=25 & bmi<30 ~ "25-29.9",
                                  TRUE ~ ">=30")) %>% 
  mutate(glucosef_category = factor(glucosef_category, levels = c("<100", "100-126", ">126")),
         hba1c_category = factor(hba1c_category, levels = c("<5.7%", "5.7%-6.5%", "6.5%-9%", ">9%")),
         bmi_category = factor(bmi_category, levels = c("<18.5", "18.5-24.9", "25-29.9", "30-39.9", ">=40")))
 

library(jtools)
library(remotes)
library(gridExtra)
library(patchwork)
#------------------------------------------------------------------------------------------------------------------------------------------------------
# new + undiagnosed + non-dm
nhanes_newdm_male_svy <- nhanes_total_svy %>% 
  dplyr::filter(dm != "diagnosed diabetes >1y" & female == 0)

nhanes_newdm_female_svy <- nhanes_total_svy %>% 
  dplyr::filter(dm != "diagnosed diabetes >1y" & female == 1)

png(paste0(path_nhanes_dmbf_folder, "/figures/boxplot of males fpg and a1c vs bmi and bf of new and undiagnosed.png"), width=800, height=800)

par(mfrow = c(2, 2))
plot1 <- svyboxplot(fat_percentage ~ factor(glucosef_category), design = nhanes_newdm_male_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "Fasting Glucose Category", ylab = "Fat Percentage (%)")
plot2 <- svyboxplot(bmi ~ factor(glucosef_category), design = nhanes_newdm_male_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "Fasting Glucose Category", ylab = "BMI (kg/m^2)")
plot3 <- svyboxplot(fat_percentage ~ factor(hba1c_category), design = nhanes_newdm_male_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "HbA1c Category", ylab = "Fat Percentage (%)")
plot4 <- svyboxplot(bmi ~ factor(hba1c_category), design = nhanes_newdm_male_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "HbA1c Category", ylab = "BMI (kg/m^2)")
par(mfrow = c(1, 1))
dev.off()

png(paste0(path_nhanes_dmbf_folder, "/figures/boxplot of females fpg and a1c vs bmi and bf of new and undiagnosed.png"), width=800, height=800)

par(mfrow = c(2, 2))
plot1 <- svyboxplot(fat_percentage ~ factor(glucosef_category), design = nhanes_newdm_female_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "Fasting Glucose Category", ylab = "Fat Percentage (%)")
plot2 <- svyboxplot(bmi ~ factor(glucosef_category), design = nhanes_newdm_female_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "Fasting Glucose Category", ylab = "BMI (kg/m^2)")
plot3 <- svyboxplot(fat_percentage ~ factor(hba1c_category), design = nhanes_newdm_female_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "HbA1c Category", ylab = "Fat Percentage (%)")
plot4 <- svyboxplot(bmi ~ factor(hba1c_category), design = nhanes_newdm_female_svy, all.outliers = TRUE, ylim=c(0,60), xlab = "HbA1c Category", ylab = "BMI (kg/m^2)")
par(mfrow = c(1, 1))
dev.off()


#-------------------------------------------------------------------------------------------------------------------------------
nhanes_total_male_svy <- nhanes_total_svy %>% 
  dplyr::filter(female == 0)

nhanes_total_female_svy <- nhanes_total_svy %>% 
  dplyr::filter(female == 1)

png(paste0(path_nhanes_dmbf_folder, "/figures/boxplot of males fpg and a1c vs bmi and bf of all.png"), width=800, height=800)

par(mfrow = c(2, 2))
plot1 <- svyboxplot(fat_percentage ~ factor(glucosef_category), design = nhanes_total_male_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "Fasting Glucose Category", ylab = "Fat Percentage (%)")
plot2 <- svyboxplot(bmi ~ factor(glucosef_category), design = nhanes_total_male_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "Fasting Glucose Category", ylab = "BMI (kg/m^2)")
plot3 <- svyboxplot(fat_percentage ~ factor(hba1c_category), design = nhanes_total_male_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "HbA1c Category", ylab = "Fat Percentage (%)")
plot4 <- svyboxplot(bmi ~ factor(hba1c_category), design = nhanes_total_male_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "HbA1c Category", ylab = "BMI (kg/m^2)")
par(mfrow = c(1, 1))
dev.off()


png(paste0(path_nhanes_dmbf_folder, "/figures/boxplot of females fpg and a1c vs bmi and bf of all.png"), width=800, height=800)

par(mfrow = c(2, 2))
plot1 <- svyboxplot(fat_percentage ~ factor(glucosef_category), design = nhanes_total_female_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "Fasting Glucose Category", ylab = "Fat Percentage (%)")
plot2 <- svyboxplot(bmi ~ factor(glucosef_category), design = nhanes_total_female_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "Fasting Glucose Category", ylab = "BMI (kg/m^2)")
plot3 <- svyboxplot(fat_percentage ~ factor(hba1c_category), design = nhanes_total_female_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "HbA1c Category", ylab = "Fat Percentage (%)")
plot4 <- svyboxplot(bmi ~ factor(hba1c_category), design = nhanes_total_female_svy, all.outliers = TRUE, ylim=c(0,60),xlab = "HbA1c Category", ylab = "BMI (kg/m^2)")
par(mfrow = c(1, 1))
dev.off()

