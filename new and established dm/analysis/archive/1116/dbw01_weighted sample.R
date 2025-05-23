rm(list=ls());gc();source(".Rprofile")

library(survey)

nhanes_20112012 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20112012.rds"))
nhanes_20132014 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20132014.rds"))
nhanes_20152016 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20152016.rds"))
nhanes_20172018 <- readRDS(paste0(path_nhanes_ckm_folder,"/working/cleaned/nhanes_20172018.rds"))


nhanes_20112018 <- bind_rows(
  nhanes_20112012 %>% 
    mutate(year = "2011-2012"),
  nhanes_20132014 %>% 
    mutate(year = "2013-2014"),
  nhanes_20152016 %>% 
    mutate(year = "2015-2016"),
  nhanes_20172018 %>% 
    mutate(year = "2017-2018")) %>% 
  dplyr::filter(age >= 18) %>% 
  mutate(dm = case_when(dm_doc_told == 1 | glycohemoglobin >= 6.5 | fasting_glucose >= 126 ~ "diabetes",
                        TRUE ~ "non-diabetes")) %>% 
  mutate(female = case_when(gender == 2 ~ 1,
                            TRUE ~ 0),
         race_eth = case_when(race == 3 ~ "NH White",
                              race == 4 ~ "NH Black",
                              race == 6 ~ "Asian",
                              race == 1 | race == 2 ~ "Hispanic",
                              TRUE ~ "Other Race"),
         education = case_when(education == 1 | education == 2 ~ "Less than high school", 
                               education == 3 ~ "High school only", 
                               education == 4 |  education == 5 ~ "Any college and above",
                               TRUE ~ ""),
         # family income-to-poverty ratio (FIPR) (https://www.cdc.gov/nchs/data/series/sr_02/sr02_161.pdf)
         # 0.00–0.99 = Below poverty; 1.00 and above = At or above poverty.  
         fipr = case_when(poverty < 1 ~ "below poverty", 
                          poverty == 1 ~ "at poverty", 
                          poverty > 1 ~ "above poverty"),
         marital = case_when(marital == 1 ~ "married", 
                             marital == 5 | marital == 6 ~"never married",
                             is.na(marital) ~ "",
                             TRUE ~ "previously married"),
         immigrant = case_when(immigrant == 1 ~ "Born in 50 US states or Washington, DC",
                               immigrant == 2 ~ "Other",
                               TRUE ~ ""),
         insurance = case_when(insured == 1 ~ "Covered by health insurance",
                               insured == 2 ~ "No health insurance",
                               TRUE ~ ""),
         insurance_type = case_when(insured_private == 1 ~ "Privately Insured",
                                    insured_medicare == 1 ~ "Insured by Medicare",
                                    insured_medicaid == 1 ~ "Insured by Medicaid",
                                    TRUE ~ ""),
         bmi_category = case_when(bmi<18.5 ~ "<18.5",
                                  bmi>=18.5 & bmi<25 ~ "18.5-24.9",
                                  bmi>=25 & bmi<30 ~ "25-29.9",
                                  bmi>=30 & bmi<40 ~ "30-39.9",
                                  bmi>=40 ~ ">= 40",
                                  TRUE ~ "Unkown"),
         sbp = rowMeans(select(., systolic1, systolic2, systolic3, systolic4), na.rm = TRUE),  
         dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3, diastolic4), na.rm = TRUE),
         year_broad = case_when(year == '2011-2012' | year == '2013-2014' ~ '2011-2014',
                                TRUE ~ '2015-2018')) %>% 
  dplyr::filter(pregnant == 2 | pregnant == 3 | is.na(pregnant)) %>% 
  dplyr::select(-c(citizenship, race, poverty, hhincome, pregnant)) %>% 
  mutate(nhanes2yweight = mec2yweight/4) 


# check weighted total sample size N
sum(nhanes_20112018$nhanes2yweight)


#   # Refer: https://www.cdc.gov/nchs/data/series/sr_02/sr02-190.pdf >> Page 22 onwards
#   group_by(year) %>%
#   mutate(
#     normalizedmec2yweight = mec2yweight / 4,  # Apply your normalization step
#     year_total_weight = sum(normalizedmec2yweight, na.rm = TRUE)  # Calculate total weight per year
#   ) %>%
#   ungroup() %>%
#   mutate(
#     overall_total_weight = sum(year_total_weight, na.rm = TRUE),  # Total weight across all years
#     year_proportion = year_total_weight / overall_total_weight,  # Proportion of each year's weight
#     new_weight = normalizedmec2yweight * year_proportion  # Apply the scaling
#   )

# The `new_weight` column now represents the scaled weight for representing the overall US population across 2011-2018.


# MEC: Mobile Examination Center
# For cross-sectional analysis of biomarkers, use the MEC weights 
# For cross-sectional analysis of questionnaire, use the interview weights
# Always use the lowest common denominator to select the right weights
# If pooling, there are specific guidelines around how to combine weights

nhanes_20112018_svy = nhanes_20112018 %>% 
  as_survey_design(.data=.,
                   ids=psu,
                   strata = pseudostratum,
                   weights = nhanes2yweight, # Specify the weight variable here
                   nest = TRUE,
                   # Both the below work well for most cases
                   pps = "brewer",variance = "YG")

saveRDS(nhanes_20112018_svy, paste0(path_nhanes_dmbf_folder, "/working/cleaned/weighted sample.RDS"))

