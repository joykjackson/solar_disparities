##########################
##STEP 0: GENERAL SET UP##
##########################

##load in packages
library(tidyverse)
library(tidycensus)
library(tigris)
library(dplyr)
library(stringr)
library(hablar)
library(ggplot2)
library(lmtest)
library(sandwich)
library(pscl)
library(MASS)
library(boot)
library(viridis)
library(stargazer)
library(corrplot)    
library(betareg)      
library(forcats)     
library(purrr)       
library(readr)       
library(tidyr)       
library(tibble)     
library(ggridges)   
library(msm)              
library(viridisLite)      
library(zoo)         
library(hrbrthemes)
library(car)
library(lm.beta)
  
##LOAD IN RAW DATAFILES## 
#this includes DeepSolar, Tracking the Sun, and Project Sunroof at the Tract-level#


###############################################
##STEP ONE: GENERATE ACS DATA FROM TIDYCENSUS##
###############################################
##create a list of all the states
states <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN",
    "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
    "VT", "VA", "WA", "WV", "WI", "WY")

##generate list of variables from ACS data for the analysis
race_vars <- c(
    white = "B03002_003E",
    black = "B03002_004E",
    asian = "B03002_006E",
    hispanic = "B03002_012E",
    pop_total = "B01001_001E")

housing_vars <- c(
    n_units = "B25001_001E",
    n_own = "B25008_002E",
    n_rent = "B25008_003E",
    tot_hh = "B11001_001E",
    n_dsf_units = "B25024_002E"
    )

dem_vars <- c(
    med_income = "B19013_001E",
    med_age = "S0101_C01_030E",
    n_bach = "B06009_005E",
    n_grad = "B06009_006E",
    tot_educ = "B06009_001E"
    )

lep_vars <- c(
    tot_lep = "B16002_001E",
    spanish_lep = "B16002_004E",
    indoeuro_lep = "B16002_007E",
    asian_lep = "B16002_010E",
    other_lep = "B16002_013E")

##combine all ACS variables into one list
all_acs_vars <- c(race_vars, housing_vars, dem_vars, lep_vars)

##extract all of the relevant tract-level ACS variables - use this as the base as it includes EVERY census tract##
acs15_table <- get_acs (
    geography = "tract",
    state = states,
    year = 2015,
    variables = all_acs_vars,
    output = "wide",
    geometry = TRUE
    )

##extract the relevant variables, add county fips to the dataframe
trdat_15 <- acs15_table %>%
  mutate(
      year = 2015,
      cofips = substr(GEOID, 1,5),
      pct_rent = n_rent / pop_total,
      pct_own = n_own / pop_total,
      pct_sfh = n_dsf_units / n_units,
      pct_educ = (n_bach + n_grad) / tot_educ,
      pct_white = white / pop_total,
      pct_black = black / pop_total,
      pct_asian = asian / pop_total,
      pct_hispanic = hispanic / pop_total,
        n_lep = spanish_lep + indoeuro_lep + asian_lep + other_lep,
        pct_lep = n_lep / tot_lep) %>%
    rename(fips = GEOID,
          name = NAME) %>%
  dplyr::select(year, name, fips, cofips, geometry, pop_total, tot_hh, med_age, med_income, white, pct_white, black, pct_black, asian, pct_asian, hispanic, pct_hispanic, n_units, n_rent, pct_rent, n_own, pct_own, n_dsf_units, pct_sfh, tot_educ, n_bach, n_grad, pct_educ, n_lep, pct_lep) %>%
  arrange(cofips, fips)

final_tract <- trdat_15

#################################################
##STEP TWO: PREPARE THE SOLAR INSTALLATION DATA##
#################################################

##read in tract-level files **update file paths
deepsolar_tract <- read.csv("~/Documents/School/MIT/research/thesis/rwork/deepsolar/deepsolar_tract.csv")
sunroof_tract <- read.csv("~/Documents/School/MIT/research/thesis/rwork/sunroof/project-sunroof-census_tract.csv")

##create extracts
deepsolar_extract <- deepsolar_tract[, c(5,70,75,99,159:169)]
sunroof_extract <- sunroof_tract[, c(1, 31)]

#convert fips codes to characters, so that we can merge to the census data
deepsolar_extract$fips <- as.character(deepsolar_extract$fips)
sunroof_extract$region_name <- as.character(sunroof_extract$region_name)

#standardize formatting of the census tracts --> ensure that tracts have 11 characters
deepsolar_extract$fips<-str_pad(deepsolar_extract$fips, width=11, pad="0")
sunroof_extract$region_name<-str_pad(sunroof_extract$region_name, width=11, pad="0")

sunroof_extract <- sunroof_extract %>%
    rename(
        fips = region_name,
        sunroof_res_installs = existing_installs_count)

sunroof_extract <- sunroof_extract %>%
    group_by(fips) %>% 
    filter(sunroof_res_installs==max(sunroof_res_installs))

##merge solar data, get rid of duplicates, and excess spaces in the column names, resulting from merging
solar_data <- left_join(x=deepsolar_extract, y=sunroof_extract, by="fips")
solar_data <- unique(solar_data)
final_tract_solar <- left_join(x=final_tract, y=solar_data, by="fips")

names(final_tract_solar) <- gsub(" ", "", names(final_tract_solar))

##clean up the variables and generate binary variables for analysis
final_tract_solar <- final_tract_solar %>%
    rename(
        corporate_tax_yrs = cooperate_tax,
        deepsolar_res_installs = solar_system_count_residential,
        res_electricity_price = electricity_price_residential,
        tot_res_incentive = incentive_count_residential,
        tot_state_res_incentive = incentive_residential_state_level,
        net_metering_yrs = net_metering,
        feedin_tariff_yrs = feedin_tariff,
        property_tax_yrs = property_tax,
        sales_tax_yrs = sales_tax,
        rebate_yrs = rebate) %>%
    mutate(
        tract_majority = ifelse(pct_white >= 0.5, "majority-white",
        ifelse(pct_black >= 0.5, "majority-black",
        ifelse(pct_asian >= 0.5, "majority-asian",
        ifelse(pct_hispanic >= 0.5, "majority-hispanic", "no majority")))))

final_tract_solar <- final_tract_solar %>%
    mutate(
        black_maj = ifelse(tract_majority=="majority-black", 1, 0),
        white_maj = ifelse(tract_majority=="majority-white", 1, 0),
        asian_maj = ifelse(tract_majority=="majority-asian", 1, 0),
        hispanic_maj = ifelse(tract_majority == "majority-hispanic", 1, 0),
        no_maj = ifelse(tract_majority=="no majority", 1, 0),
        net_metering = ifelse(net_metering_yrs > 0, 1, 0),
        feedin_tariff = ifelse(feedin_tariff_yrs > 0, 1, 0),
        property_tax = ifelse(property_tax_yrs > 0, 1, 0),
        sales_tax = ifelse(sales_tax_yrs > 0, 1, 0),
        rebate = ifelse(rebate_yrs > 0, 1, 0))

##add state fips to tract data
final_tract_solar$statefips <- substr(final_tract_solar$fips, 1, 2)

##add in the logit variables
final_tract$deepsolar_logit <- if_else(final_tract$deepsolar_res_installs>0,1,0)
final_tract$sunroof_logit <- if_else(final_tract$sunroof_res_installs>0,1,0)

##quality checked data is now in the final data frame - regression analysis can begin
final_tract <- final_tract_solar
