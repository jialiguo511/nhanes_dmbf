rm(list=ls());gc();source(".Rprofile")


mean_vars = c("age","dm_age","waistcircumference","fasting_glucose","triglyceride","ldl","hdl",
              "total_fat","fat_percentage","bmi","sbp","dbp")

median_vars = c("glycohemoglobin")


# all people, all year by dm

table_df <- read_csv("analysis/dmbf02a_descriptive characteristics all by dm.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(dm,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,dm,output) %>% 
  pivot_wider(names_from=dm,values_from=output) %>% 
  dplyr::select(variable,group,Total,diabetes,nondiabetes)


write_csv(table_df,"paper/table_descriptive characteristics all by dm.csv")

#-------------------------------------------------------------------------------------------------
# all people, by year_board

table_df <- read_csv("analysis/dmbf02b_descriptive characteristics all by year.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(year_broad,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,year_broad,output) %>% 
  pivot_wider(names_from=year_broad,values_from=output) %>% 
  dplyr::select(variable,group,Total,`2011-2014`,`2015-2018`)

write_csv(table_df,"paper/table_descriptive characteristics all by year.csv")

#-------------------------------------------------------------------------------------------------
# DM cases only

table_df <- read_csv("analysis/dmbf02c_descriptive characteristics dm cases by year.csv") %>% 
  dplyr::mutate(selected_rows = case_when(variable %in% mean_vars & est %in% c("mean","sd") ~ 1,
                                          variable %in% median_vars & est %in% c("median","q25","q75") ~ 1,
                                          !variable %in% c(mean_vars,median_vars) ~ 1,
                                          TRUE ~ 0
  )) %>% 
  dplyr::filter(selected_rows == 1) %>% 
  dplyr::select(year_broad,group,variable,est,value) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  mutate(output = case_when(variable %in% mean_vars ~ paste0(round(mean,1)," (",round(sd,1),")"),
                            variable %in% median_vars ~ paste0(round(median,1)," (",round(q25,1),", ",round(q75,1),")"),
                            TRUE ~ paste0(round(freq,0)," (",round(proportion,1),"%)")
  )) %>% 
  dplyr::select(variable,group,year_broad,output) %>% 
  pivot_wider(names_from=year_broad,values_from=output) %>% 
  dplyr::select(variable,group,Total,`2011-2014`,`2015-2018`)

write_csv(table_df,"paper/table_descriptive characteristics dm cases by year.csv")









