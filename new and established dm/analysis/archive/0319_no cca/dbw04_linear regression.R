rm(list=ls());gc();source(".Rprofile")

nhanes_svy_dfs <- readRDS(paste0(path_nhanes_dmbf_folder, "/working/cleaned/dbw01_weighted df.RDS")) 


s1_list <- vector("list", length(nhanes_svy_dfs))
s2_list <- vector("list", length(nhanes_svy_dfs))
s3_list <- vector("list", length(nhanes_svy_dfs))
s4_list <- vector("list", length(nhanes_svy_dfs))

d1_list <- vector("list", length(nhanes_svy_dfs))
d2_list <- vector("list", length(nhanes_svy_dfs))
d3_list <- vector("list", length(nhanes_svy_dfs))
d4_list <- vector("list", length(nhanes_svy_dfs))

ld1_list <- vector("list", length(nhanes_svy_dfs))
ld2_list <- vector("list", length(nhanes_svy_dfs))
ld3_list <- vector("list", length(nhanes_svy_dfs))
ld4_list <- vector("list", length(nhanes_svy_dfs))

h1_list <- vector("list", length(nhanes_svy_dfs))
h2_list <- vector("list", length(nhanes_svy_dfs))
h3_list <- vector("list", length(nhanes_svy_dfs))
h4_list <- vector("list", length(nhanes_svy_dfs))

tg1_list <- vector("list", length(nhanes_svy_dfs))
tg2_list <- vector("list", length(nhanes_svy_dfs))
tg3_list <- vector("list", length(nhanes_svy_dfs))
tg4_list <- vector("list", length(nhanes_svy_dfs))

tc1_list <- vector("list", length(nhanes_svy_dfs))
tc2_list <- vector("list", length(nhanes_svy_dfs))
tc3_list <- vector("list", length(nhanes_svy_dfs))
tc4_list <- vector("list", length(nhanes_svy_dfs))


for (i in seq_along(nhanes_svy_dfs)) {
  nhanes_total_svy <- nhanes_svy_dfs[[i]]
  
  s1 = svyglm(sbp ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  s2 = svyglm(sbp ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  s3 = svyglm(sbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  s4 = svyglm(sbp ~ age + female + race_eth + fat_percentage + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  
  d1 = svyglm(dbp ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  d2 = svyglm(dbp ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  d3 = svyglm(dbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  d4 = svyglm(dbp ~ age + female + race_eth + fat_percentage + htn_med_told + htn_med_taking, design = nhanes_total_svy)
  
  ld1 = svyglm(ldl ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  ld2 = svyglm(ldl ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  ld3 = svyglm(ldl ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  ld4 = svyglm(ldl ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  h1 = svyglm(hdl ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  h2 = svyglm(hdl ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  h3 = svyglm(hdl ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  h4 = svyglm(hdl ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  tg1 = svyglm(triglyceride ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  tg2 = svyglm(triglyceride ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  tg3 = svyglm(triglyceride ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  tg4 = svyglm(triglyceride ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  tc1 = svyglm(total_cholesterol ~ age + female + race_eth + bmi, design = nhanes_total_svy)
  tc2 = svyglm(total_cholesterol ~ age + female + race_eth + fat_percentage, design = nhanes_total_svy)
  tc3 = svyglm(total_cholesterol ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  tc4 = svyglm(total_cholesterol ~ age + female + race_eth + fat_percentage + chol_med_told + chol_med_taking, design = nhanes_total_svy)
  
  
  s1_list[[i]] = summary(s1)
  s2_list[[i]] = summary(s2)
  s3_list[[i]] = summary(s3)
  s4_list[[i]] = summary(s4)
  
  d1_list[[i]] = summary(d1)
  d2_list[[i]] = summary(d2)
  d3_list[[i]] = summary(d3)
  d4_list[[i]] = summary(d4)
  
  ld1_list[[i]] = summary(ld1)
  ld2_list[[i]] = summary(ld2)
  ld3_list[[i]] = summary(ld3)
  ld4_list[[i]] = summary(ld4)
  
  h1_list[[i]] = summary(h1)
  h2_list[[i]] = summary(h2)
  h3_list[[i]] = summary(h3)
  h4_list[[i]] = summary(h4)
  
  tg1_list[[i]] = summary(tg1)
  tg2_list[[i]] = summary(tg2)
  tg3_list[[i]] = summary(tg3)
  tg4_list[[i]] = summary(tg4)
  
  tc1_list[[i]] = summary(tc1)
  tc2_list[[i]] = summary(tc2)
  tc3_list[[i]] = summary(tc3)
  tc4_list[[i]] = summary(tc4)
}


source("functions/extract_summary.R")
source("functions/pool_results_lm.R")

s1_sum = extract_summary(s1_list)
s2_sum = extract_summary(s2_list)
s3_sum = extract_summary(s3_list)
s4_sum = extract_summary(s4_list)

d1_sum = extract_summary(d1_list)
d2_sum = extract_summary(d2_list)
d3_sum = extract_summary(d3_list)
d4_sum = extract_summary(d4_list)

ld1_sum = extract_summary(ld1_list)
ld2_sum = extract_summary(ld2_list)
ld3_sum = extract_summary(ld3_list)
ld4_sum = extract_summary(ld4_list)

h1_sum = extract_summary(h1_list)
h2_sum = extract_summary(h2_list)
h3_sum = extract_summary(h3_list)
h4_sum = extract_summary(h4_list)

tg1_sum = extract_summary(tg1_list)
tg2_sum = extract_summary(tg2_list)
tg3_sum = extract_summary(tg3_list)
tg4_sum = extract_summary(tg4_list)

tc1_sum = extract_summary(tc1_list)
tc2_sum = extract_summary(tc2_list)
tc3_sum = extract_summary(tc3_list)
tc4_sum = extract_summary(tc4_list)


results <- bind_rows(
  pool_results_lm(s1_sum) %>% mutate(model = "SBP_BMI"),
  pool_results_lm(s2_sum) %>% mutate(model = "SBP_BF"),
  pool_results_lm(s3_sum) %>% mutate(model = "SBP_BMI_trt"),
  pool_results_lm(s4_sum) %>% mutate(model = "SBP_BF_trt"),
  pool_results_lm(d1_sum) %>% mutate(model = "DBP_BMI"),
  pool_results_lm(d2_sum) %>% mutate(model = "DBP_BF"),
  pool_results_lm(d3_sum) %>% mutate(model = "DBP_BMI_trt"),
  pool_results_lm(d4_sum) %>% mutate(model = "DBP_BF_trt"),
  pool_results_lm(ld1_sum) %>% mutate(model = "LDL_BMI"),
  pool_results_lm(ld2_sum) %>% mutate(model = "LDL_BF"),
  pool_results_lm(ld3_sum) %>% mutate(model = "LDL_BMI_trt"),
  pool_results_lm(ld4_sum) %>% mutate(model = "LDL_BF_trt"),
  pool_results_lm(h1_sum) %>% mutate(model = "HDL_BMI"),
  pool_results_lm(h2_sum) %>% mutate(model = "HDL_BF"),
  pool_results_lm(h3_sum) %>% mutate(model = "HDL_BMI_trt"),
  pool_results_lm(h4_sum) %>% mutate(model = "HDL_BF_trt"),
  pool_results_lm(tg1_sum) %>% mutate(model = "Triglyceride_BMI"),
  pool_results_lm(tg2_sum) %>% mutate(model = "Triglyceride_BF"),
  pool_results_lm(tg3_sum) %>% mutate(model = "Triglyceride_BMI_trt"),
  pool_results_lm(tg4_sum) %>% mutate(model = "Triglyceride_BF_trt"),
  pool_results_lm(tc1_sum) %>% mutate(model = "Total_cholesterol_BMI"),
  pool_results_lm(tc2_sum) %>% mutate(model = "Total_cholesterol_BF"),
  pool_results_lm(tc3_sum) %>% mutate(model = "Total_cholesterol_BMI_trt"),
  pool_results_lm(tc4_sum) %>% mutate(model = "Total_cholesterol_BF_trt"),) %>% 
  write_csv(.,"analysis/dbw04_linear regression pooled results with multiple imputation.csv")

