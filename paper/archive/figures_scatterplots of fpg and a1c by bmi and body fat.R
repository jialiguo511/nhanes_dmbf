rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

nhanes_20112018_svy <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS"))

model_fb <- svyglm(fasting_glucose ~ bmi, design = nhanes_20112018_svy)
model_ff <- svyglm(fasting_glucose ~ fat_percentage, design = nhanes_20112018_svy)
model_gb <- svyglm(glycohemoglobin ~ bmi, design = nhanes_20112018_svy)
model_gf <- svyglm(glycohemoglobin ~ fat_percentage, design = nhanes_20112018_svy)

coef_fb <- coef(model_fb)
coef_ff <- coef(model_ff)
coef_gb <- coef(model_gb)
coef_gf <- coef(model_gf)

data_frame <- as.data.frame(nhanes_20112018_svy) 

# Filter columns necessary for plotting to reduce memory usage if the dataset is large
data_fb <- data_frame[, c("fasting_glucose", "bmi")]
data_ff <- data_frame[, c("fasting_glucose", "fat_percentage")]
data_gb <- data_frame[, c("glycohemoglobin", "bmi")]
data_gf <- data_frame[, c("glycohemoglobin", "fat_percentage")]

p1 <- ggplot(data_fb, aes(x = bmi, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BMI (kg/m^2)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p2 <- ggplot(data_ff, aes(x = fat_percentage, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p3 <- ggplot(data_gb, aes(x = bmi, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BMI (kg/m^2)",
       y = "HbA1c (%)") +
  theme_minimal()

p4 <- ggplot(data_gf, aes(x = fat_percentage, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "HbA1c (%)") +
  theme_minimal()

library(patchwork)

combined_plot <- p1 + p2 + p3 + p4 + 
  plot_layout(ncol = 2, nrow = 2) 

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/scatterplot of fpg, a1c vs bmi, body fat by sex.png"),width=8, height = 7)

#------------------------------------------------------------
p1 <- ggplot(data_fb, aes(x = bmi, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BMI (kg/m^2)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p2 <- ggplot(data_ff, aes(x = fat_percentage, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p3 <- ggplot(data_gb, aes(x = bmi, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BMI (kg/m^2)",
       y = "HbA1c (%)") +
  theme_minimal()

p4 <- ggplot(data_gf, aes(x = fat_percentage, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "HbA1c (%)") +
  theme_minimal()

library(patchwork)

combined_plot <- p1 + p2 + p3 + p4 + 
  plot_layout(ncol = 2, nrow = 2) 

ggsave(combined_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/scatterplot of fpg, a1c vs bmi, body fat by sex.png"),width=8, height = 7)

#--------------------------------------------------------------------
p1 <- ggplot(data_fb, aes(x = bmi, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(limits = c(100, 200), breaks = seq(100, 200, by = 10)) +
  labs(x = "BMI (kg/m^2)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p2 <- ggplot(data_ff, aes(x = fat_percentage, y = fasting_glucose)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(limits = c(100, 200), breaks = seq(100, 200, by = 10)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "Fasting Glucose (mg/dL)") +
  theme_minimal()

p3 <- ggplot(data_gb, aes(x = bmi, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(limits = c(5, 6), breaks = seq(5, 6, by = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BMI (kg/m^2)",
       y = "HbA1c (%)") +
  theme_minimal()

p4 <- ggplot(data_gf, aes(x = fat_percentage, y = glycohemoglobin)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(limits = c(5, 6), breaks = seq(5, 6, by = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body Fat Percentage (%)",
       y = "HbA1c (%)") +
  theme_minimal()

# Combine plots
zoomed_plot <- p1 + p2 + p3 + p4 + 
  plot_layout(ncol = 2, nrow = 2) # Arrange in 2 columns and 2 rows

ggsave(zoomed_plot, filename=paste0(path_nhanes_dmbf_folder, "/figures/zoomed scatterplot of fpg, a1c vs bmi, body fat by sex.png"),width=8, height = 7)

