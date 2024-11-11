rm(list=ls());gc();source(".Rprofile")

ndm_df <- readRDS(paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds")) %>% 
  dplyr::filter(dm == 0) %>% 
  mutate(dm = case_when(dm == 1 ~ "diabetes",
                        TRUE ~ "nondiabetes")) %>% 
  mutate(bf_category = case_when(
    # For males
    gender == "Male" & fat_percentage <= 14 ~ 0,
    gender == "Male" & fat_percentage > 14 & fat_percentage <= 20 ~ 1,
    gender == "Male" & fat_percentage > 20 & fat_percentage < 25 ~ 2,
    gender == "Male" & fat_percentage >= 25 ~ 3,
    
    # For females
    gender == "Female" & fat_percentage <= 25 ~ 0,
    gender == "Female" & fat_percentage > 25 & fat_percentage <= 30 ~ 1,
    gender == "Female" & fat_percentage > 30 & fat_percentage < 35 ~ 2,
    gender == "Female" & fat_percentage >= 35 ~ 3,
    
    TRUE ~ NA_real_
  ))

source("functions/table1_summary.R")

c_vars = c("fat_percentage","bmi")
g_vars = c("bmi_category","bf_category")

table_df = ndm_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(gender = "Total")) %>% 
  table1_summary(.,c_vars = c_vars,g_vars = g_vars,id_vars = "gender")

write_csv(table_df,"analysis/dmbf02d_descriptive characteristics non-dm by gender.csv")

mean_vars = c("fat_percentage","bmi")

# non-dm

table_df <- read_csv("analysis/dmbf02d_descriptive characteristics non-dm by gender.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          !variable %in% c(mean_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(gender,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,gender,output) %>% 
  pivot_wider(names_from=gender,values_from=output) %>% 
  dplyr::select(variable,group,Total,Male,Female)


write_csv(table_df,"paper/table_descriptive characteristics non-dm by gender.csv")

#--------------------------------------------------------------------------------------------------------------------
dm_df <- readRDS(paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds")) %>% 
  dplyr::filter(dm == 1) %>% 
  mutate(dm = case_when(dm == 1 ~ "diabetes",
                        TRUE ~ "nondiabetes")) %>% 
  mutate(bf_category = case_when(
    # For males
    gender == "Male" & fat_percentage <= 14 ~ 0,
    gender == "Male" & fat_percentage > 14 & fat_percentage <= 20 ~ 1,
    gender == "Male" & fat_percentage > 20 & fat_percentage < 25 ~ 2,
    gender == "Male" & fat_percentage >= 25 ~ 3,
    
    # For females
    gender == "Female" & fat_percentage <= 25 ~ 0,
    gender == "Female" & fat_percentage > 25 & fat_percentage <= 30 ~ 1,
    gender == "Female" & fat_percentage > 30 & fat_percentage < 35 ~ 2,
    gender == "Female" & fat_percentage >= 35 ~ 3,
    
    TRUE ~ NA_real_
  ))

source("functions/table1_summary.R")

c_vars = c("fat_percentage","bmi")
g_vars = c("bmi_category","bf_category")

table_df = dm_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(gender = "Total")) %>% 
  table1_summary(.,c_vars = c_vars,g_vars = g_vars,id_vars = "gender")

write_csv(table_df,"analysis/dmbf02e_descriptive characteristics dm by gender.csv")

mean_vars = c("fat_percentage","bmi")

# non-dm

table_df <- read_csv("analysis/dmbf02e_descriptive characteristics dm by gender.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          !variable %in% c(mean_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(gender,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,gender,output) %>% 
  pivot_wider(names_from=gender,values_from=output) %>% 
  dplyr::select(variable,group,Total,Male,Female)


write_csv(table_df,"paper/table_descriptive characteristics dm by gender.csv")