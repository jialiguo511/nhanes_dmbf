



sbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking
dbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking
total_cholesterol ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking
triglyceride ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking



# Model for Systolic Blood Pressure (SBP)
model_sbp <- svyglm(sbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)

# Model for Diastolic Blood Pressure (DBP)
model_dbp <- svyglm(dbp ~ age + female + race_eth + bmi + htn_med_told + htn_med_taking, design = nhanes_total_svy)

# Model for Total Cholesterol
model_total_cholesterol <- svyglm(total_cholesterol ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)

# Model for Triglycerides
model_triglycerides <- svyglm(triglyceride ~ age + female + race_eth + bmi + chol_med_told + chol_med_taking, design = nhanes_total_svy)



summary(model_sbp)
summary(model_dbp)
summary(model_total_cholesterol)
summary(model_triglycerides)





