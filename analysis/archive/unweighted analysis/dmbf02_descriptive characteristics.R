rm(list=ls());gc();source(".Rprofile")

source_df <- readRDS(paste0(path_nhanes_dmbf_folder,"/working/cleaned/source df.rds")) %>% 
  mutate(year_broad = case_when(year %in% c('2011-2012','2013-2014') ~ '2011-2014',
                                TRUE ~ '2015-2018')) %>% 
  mutate(dm = case_when(dm == 1 ~ "diabetes",
                        TRUE ~ "nondiabetes"))

source("functions/table1_summary.R")

c_vars = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
           "total_fat","fat_percentage","bmi","glycohemoglobin","sbp","dbp")
g_vars = c("gender","race_eth","fipr","bmi_category","immigrant","education","marital","insurance","insurance_type")

#----------------------------------------------------------------------------------------------------------------------------------
# all people, all year by dm

table_df = source_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(dm = "Total")) %>% 
  table1_summary(.,c_vars = c_vars,g_vars = g_vars,id_vars = "dm")
  
  
write_csv(table_df,"analysis/dmbf02a_descriptive characteristics all by dm.csv")


#----------------------------------------------------------------------------------------------------------------------------------
# all people, by year_board

table_df = source_df %>% 
  bind_rows(.,
            {.} %>% 
              mutate(year_broad = "Total")) %>% 
  table1_summary(.,c_vars = c_vars,p_vars = "dm", g_vars = g_vars,id_vars = "year_broad")


write_csv(table_df,"analysis/dmbf02b_descriptive characteristics all by year.csv")

#----------------------------------------------------------------------------------------------------------------------------------
# dm cases by year_board
table_df = source_df %>% 
  dplyr::filter(dm == "diabetes") %>% 
  bind_rows(.,
            {.} %>% 
              mutate(year_broad = "Total")) %>% 
  table1_summary(.,c_vars = c_vars,g_vars = g_vars,id_vars = "year_broad")

write_csv(table_df,"analysis/dmbf02c_descriptive characteristics dm cases by year.csv")


