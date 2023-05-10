##set your working directory, this is where the output tables go

##FIRST PHASE: SINGLE-FAMILY HOME VARIABLE 
##deepsolar logit regressions - single-family home variables
dslog1 <- glm(deepsolar_logit ~ log1p(n_units), family = "binomial", data = final_tract)
dslog2 <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, family = "binomial", data = final_tract)
dslog3 <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, family = "binomial", data = final_tract)
dslog4 <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, family = "binomial", data = final_tract)   
dslog5 <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_sfh + pct_lep, family = "binomial", data = final_tract)   
dslog6 <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_sfh+ pct_lep, family = "binomial", data = final_tract)

##generate output table for DeepSolar logit regressions
stargazer(dslog1, dslog2, dslog3, dslog4, dslog5, dslog6,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_logit_ds.htm")
         
         
##sunroof logit regressions - single-family home variables
srlog1 <- glm(sunroof_logit ~ log1p(n_units), family = "binomial", data = final_tract)
srlog2 <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, family = "binomial", data = final_tract)
srlog3 <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, family = "binomial", data = final_tract)     
srlog4 <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, family = "binomial", data = final_tract)     
srlog5 <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_sfh + pct_lep, family = "binomial", data = final_tract)    
srlog6 <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_sfh+ pct_lep, family = "binomial", data = final_tract)
        
##generate output table for Project Sunroof logit regressions
stargazer(srlog1, srlog2, srlog3, srlog4, srlog5, srlog6,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_logit_sr.htm")
         

##deepsolar OLS regressions - single-family home variable
dsols1 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units), data = final_tract)
dsols2 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
dsols3 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
dsols4 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, data = final_tract)
dsols5 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_sfh + pct_lep, data = final_tract)
dsols6 <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_sfh+ pct_lep, data = final_tract)
         
##generate output table for DeepSolar OLS - SFH variable
stargazer(dsols1, dsols2, dsols3, dsols4, dsols5, dsols6,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_ds.htm")
    
         
##generate standardized coefficients for DeepSolar OLS - SFH variable
dsols1std <- lm.beta(dsols1)
dsols2std <- lm.beta(dsols2)
dsols3std <- lm.beta(dsols3)
dsols4std <- lm.beta(dsols4)
dsols5std <- lm.beta(dsols5)
dsols6std <- lm.beta(dsols6)

##generate output table for DeepSolar OLS - SFH variable          
stargazer(dsols1std, dsols2std, dsols3std, dsols4std, dsols5std, dsols6std,
          type = "html",
          coef = list(dsols1std$standardized.coefficients, dsols2std$standardized.coefficients, dsols3std$standardized.coefficients, dsols4std$standardized.coefficients, dsols5std$standardized.coefficients, dsols6std$standardized.coefficients),
          p = list(coef(summary(dsols1std))[,5], coef(summary(dsols2std))[,5], coef(summary(dsols3std))[,5], coef(summary(dsols4std))[,5], coef(summary(dsols5std))[,5], coef(summary(dsols6std))[,5]),
          omit = "Constant",
          omit.labels = "Intercept",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_ds_std.htm")
         
##sunroof OLS regressions - single-family home variable
srols1 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units), data = final_tract)
srols2 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
srols3 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
srols4 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, data = final_tract)
srols5 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_sfh + pct_lep, data = final_tract)     
srols6 <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_sfh+ pct_lep, data = final_tract)

#generate output table for project sunroof - sfh variable
stargazer(srols1, srols2, srols3, srols4, srols5, srols6,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_sr.htm")
         
##generate standardized coefficients for Project Sunroof OLS - SFH variable
srols1std <- lm.beta(srols1)
srols2std <- lm.beta(srols2)
srols3std <- lm.beta(srols3)
srols4std <- lm.beta(srols4)
srols5std <- lm.beta(srols5)
srols6std <- lm.beta(srols6)

##generate output table for Project Sunroof OLS - SFH variable 
stargazer(srols1std, srols2std, srols3std, srols4std, srols5std, srols6std,
          type = "html",
          coef = list(srols1std$standardized.coefficients, srols2std$standardized.coefficients, srols3std$standardized.coefficients, srols4std$standardized.coefficients, srols5std$standardized.coefficients, srols6std$standardized.coefficients),
          p = list(coef(summary(srols1std))[,5], coef(summary(srols2std))[,5], coef(summary(srols3std))[,5], coef(summary(srols4std))[,5], coef(summary(srols5std))[,5], coef(summary(srols6std))[,5]),
          omit = "Constant",
          omit.labels = "Intercept",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_sr_std.htm")

####################################################
##PHASE 2: SAME MODELS FOR OWNER-OCCUPIED VARIABLE##
####################################################

##deepsolar logit regressions - owner-occupied variables
dslog1own <- glm(deepsolar_logit ~ log1p(n_units), family = "binomial", data = final_tract)
dslog2own <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, family = "binomial", data = final_tract)
dslog3own <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, family = "binomial", data = final_tract)
dslog4own <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_own + pct_lep, family = "binomial", data = final_tract)   
dslog5own <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_own + pct_lep, family = "binomial", data = final_tract)   
dslog6own <- glm(deepsolar_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_own + pct_lep, family = "binomial", data = final_tract)

##generate output table for DeepSolar logit regressions
stargazer(dslog1own, dslog2own, dslog3own, dslog4own, dslog5own, dslog6own,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_logit_ds_own.htm")
         
         
##sunroof logit regressions - homeowner variables
srlog1own <- glm(sunroof_logit ~ log1p(n_units), family = "binomial", data = final_tract)
srlog2own <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, family = "binomial", data = final_tract)
srlog3own <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, family = "binomial", data = final_tract)     
srlog4own <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, family = "binomial", data = final_tract)     
srlog5own <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_sfh + pct_lep, family = "binomial", data = final_tract)    
srlog6own <- glm(sunroof_logit ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_sfh+ pct_lep, family = "binomial", data = final_tract)
        
##generate output table for Project Sunroof logit regressions
stargazer(srlog1own, srlog2own, srlog3own, srlog4own, srlog5own, srlog6own,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_logit_sr_own.htm")
         

##deepsolar OLS regressions - homeowner variable
dsols1own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units), data = final_tract)
dsols2own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
dsols3own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
dsols4own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_own + pct_lep, data = final_tract)
dsols5own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_own + pct_lep, data = final_tract)
dsols6own <- lm(log1p(deepsolar_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_own + pct_lep, data = final_tract)
         
##generate output table for DeepSolar OLS - homeowner variable
stargazer(dsols1own, dsols2own, dsols3own, dsols4own, dsols5own, dsols6own,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_ds_own.htm")
 
##sunroof OLS regressions - single-family home variable
srols1own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units), data = final_tract)
srols2own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
srols3own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
srols4own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_own + pct_lep, data = final_tract)
srols5own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + log1p(med_income) + pct_own + pct_lep, data = final_tract)     
srols6own <- lm(log1p(sunroof_res_installs) ~ log1p(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + log1p(med_income) + pct_own + pct_lep, data = final_tract)

#generate output table for project sunroof - homeowner variable
stargazer(srols1own, srols2own, srols3own, srols4own, srols5own, srols6own,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("ln(Total Number of Units + 1)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","ln(Median Income + 1)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_sr_own.htm")
         
##generate standardized coefficients for Project Sunroof OLS - SFH variable
srols1stdow <- lm.beta(srols1own)
srols2stdow <- lm.beta(srols2own)
srols3stdow <- lm.beta(srols3own)
srols4stdow <- lm.beta(srols4own)
srols5stdow <- lm.beta(srols5own)
srols6stdow <- lm.beta(srols6own)

#########################################################################
##PHASE 3: OLS REGRESSIONS USING INVERSE HYPERBOLIC SINE TRANSFORMATION##
#########################################################################

dsols1sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units), data = final_tract)
dsols2sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
dsols3sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
dsols4sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, data = final_tract)
dsols5sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + asinh(med_income) + pct_sfh + pct_lep, data = final_tract)
dsols6sin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + asinh(med_income) + pct_sfh+ pct_lep, data = final_tract)
         
##generate output table for DeepSolar OLS - SFH variable
stargazer(dsols1sin, dsols2sin, dsols3sin, dsols4sin, dsols5sin, dsols6sin,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("asinh(Total Number of Units)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","asinh(Median Income)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_ds_asin.htm")

srols1sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units), data = final_tract)
srols2sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
srols3sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
srols4sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_sfh + pct_lep, data = final_tract)
srols5sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + asinh(med_income) + pct_sfh + pct_lep, data = final_tract)     
srols6sin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + asinh(med_income) + pct_sfh + pct_lep, data = final_tract)

#generate output table for project sunroof - sfh variable
stargazer(srols1sin, srols2sin, srols3sin, srols4sin, srols5sin, srols6sin,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("asinh(Total Number of Units)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","asinh(Median Income)","% Detached Single Family Homes", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_sr_asin.htm")
    
##regressions with asinh and owner variable
dols1osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units), data = final_tract)
dols2osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
dols3osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
dols4osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_own + pct_lep, data = final_tract)
dols5osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + asinh(med_income) + pct_own + pct_lep, data = final_tract)
dols6osin <- lm(asinh(deepsolar_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + asinh(med_income) + pct_own + pct_lep, data = final_tract)
         
##generate output table for DeepSolar OLS - asinh/homeowner variable
stargazer(dols1osin, dols2osin, dols3osin, dols4osin, dols5osin, dols6osin,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("DeepSolar Residential Installations"),
          covariate.labels = c("asinh(Total Number of Units)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","asin(Median Income)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_ds_own_asin.htm")
 
##sunroof OLS regressions - homeowner/asinh variable
sols1osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units), data = final_tract)
sols2osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj, data = final_tract)
sols3osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive, data = final_tract)
sols4osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + pct_own + pct_lep, data = final_tract)
sols5osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + asinh(med_income) + pct_own + pct_lep, data = final_tract)     
sols6osin <- lm(asinh(sunroof_res_installs) ~ asinh(n_units) + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + asinh(med_income) + pct_own + pct_lep, data = final_tract)

#generate output table for project sunroof - homeowner/asinh variable
stargazer(sols1osin, sols2osin, sols3osin, sols4osin, sols5osin, sols6osin,
          type = "html",
          digits = 2,
          star.char = c("*","**","***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels = c("Sunroof Residential Installations"),
          covariate.labels = c("asinh(Total Number of Units)","Majority-Black","Majority-Asian","Majority-Hispanic","No Majority","Average Daily Solar Radiation", "Average Residential Electricity Price","Total Number of State-Level Residential Solar Incentives","Median Age","% with Bachelor's or Higher","asinh(Median Income)","% of Population in Owner-Occupied Housing", "% of Limited English Proficient Population"),
          out = "~/Documents/final_ols_sr_own_asin.htm")

