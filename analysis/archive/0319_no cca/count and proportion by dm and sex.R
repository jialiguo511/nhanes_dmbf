
dm_list <- vector("list", length(nhanes_svy_dfs))
dmsex_list <- vector("list", length(nhanes_svy_dfs))

for (i in seq_along(nhanes_svy_dfs)) {
  df <- nhanes_svy_dfs[[i]]

  nhanes_total_svy <- df %>% 
    mutate(
      dm_sex = case_when(female == 1 & dm == "non-diabetes" ~ "female non-diabetes",
                         female == 0 & dm == "non-diabetes" ~ "male non-diabetes",
                         female == 1 & dm == "newly and undiagnosed diabetes" ~ "female newly and undiagnosed diabetes",
                         female == 0 & dm == "newly and undiagnosed diabetes" ~ "male newly and undiagnosed diabetes",
                         female == 1 & dm == "diagnosed diabetes >1y" ~ "female diagnosed diabetes >1y",
                         female == 0 & dm == "diagnosed diabetes >1y" ~ "male diagnosed diabetes >1y")
    ) 
  
  
  dm_table <- svytable(~dm, design = nhanes_total_svy)
  dm_list[[i]] <- dm_table
  
  dmsex_table <- svytable(~dm_sex, design = nhanes_total_svy)
  dmsex_list[[i]] <- dmsex_table
}

dm_total <- bind_rows(dm_list) %>% 
  summarise(
    dm_1y = mean(`diagnosed diabetes >1y`),
    new = mean(`newly and undiagnosed diabetes`),
    nodm = mean(`non-diabetes`)
  )


dmsex_total <- bind_rows(dmsex_list) %>% 
  summarise(
    female_dm_1y = mean(`female diagnosed diabetes >1y`),
    male_dm_1y = mean(`male diagnosed diabetes >1y`),
    female_new = mean(`female newly and undiagnosed diabetes`),
    male_new = mean(`male newly and undiagnosed diabetes`),
    female_nodm = mean(`female non-diabetes`),
    male_nodm = mean(`male non-diabetes`),
    total = sum(female_dm_1y,male_dm_1y,female_new,male_new,female_nodm,male_nodm)
  ) %>% 
  mutate(female_dm_1y_prop = female_dm_1y/total,
         male_dm_1y_prop = male_dm_1y/total,
         female_new_prop = female_new/total,
         male_new_prop = male_new/total,
         female_nodm_prop = female_nodm/total,
         male_nodm_prop = male_nodm/total)

