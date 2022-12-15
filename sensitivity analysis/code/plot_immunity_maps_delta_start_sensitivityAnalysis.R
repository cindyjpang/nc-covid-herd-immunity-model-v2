##
#### Create maps for Delta wave COVID-19 paper
##
#### Built from Paul's code, plot_immunity_maps_delta_start.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(dplyr)
## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
immunity_est <- read_excel("exported data/immunity_est.xlsx")


## Read start dates for each scenario
scenarios <- c("S1", "S2", "S3", "S4", "S5", "S6")
start_dates <- data.frame()
for(scenario in scenarios){
  read_file <- paste0("./sensitivity analysis/outputs/", scenario,"/delta_county_summary_", scenario, ".xlsx")
  
  
  file_dat <- read_excel(read_file)
  
  ## clean file
  temp <- file_dat %>%
    select(COUNTY, start_date)%>%
    mutate(scenario = scenario)%>%
    group_by(COUNTY)%>%
    summarize(start_date = min(start_date))%>%
    mutate(scenario = scenario)
  print(temp)
  start_dates <- rbind(start_dates, temp)
  
}


##
##
##
## CONSTRUCT SCENARIOS 
##
##
##

immunity_scenarios_raw <- immunity_est %>%
  mutate( ### CDC MULTIPLIER SCENARIOS
         overall_cdc_indep = cdc_case_vacc_obs_imm, 
         overall_cdc_upper = cdc_case_vacc_obs_imm_up, 
         overall_cdc_lower = cdc_case_vacc_obs_imm_lower, # max(cum_cdc_multiplier_cases, cum_vacc_est_obs)
         infection_cdc_indep = ((cum_cdc_multiplier_cases-joint_cdc_case_obs)/Population)*100,
         infection_cdc_upper = (cum_cdc_multiplier_cases/Population)*100,
         vaccination_cdc_indep = ((cum_vacc_est_obs - joint_cdc_case_obs)/Population)*100, 
         vaccination_cdc_upper = (cum_vacc_est_obs/Population)*100, 
         
         ### DEATH INF SCENARIOS
         overall_death_indep = death_inf_vacc_obs_imm, 
         overall_death_upper = death_inf_vacc_obs_imm_up, 
         overall_death_lower = death_inf_vacc_obs_imm_lower, 
         infection_death_indep = ((cum_death_inf_cases - joint_death_inf_obs)/Population)*100,
         infection_death_upper = (cum_death_inf_cases/Population)*100,
         vaccination_death_indep = ((cum_vacc_est_obs - joint_death_inf_obs)/Population)*100,
         vaccination_death_upper = (cum_vacc_est_obs/Population)*100)

# HELPER FNC FOR 
find_max_var <- function(diff){
  str = ""
  
  if(diff > 0){
    str = "Infection"
  }else if(diff < 0){
    str = "Vaccination"
  }else{
    str = "Equal"
  }
  return(str)
}

# GET LOWER BOUND DATA 
lower_bound_sort <- immunity_est %>%
  mutate(
    joint_cdc_count = pmin(cum_cdc_multiplier_cases, cum_vacc_est_obs), 
    joint_death_count = pmin(cum_death_inf_cases, cum_vacc_est_obs),
    diff_cdc = cum_cdc_multiplier_cases - cum_vacc_est_obs, 
    diff_death = cum_death_inf_cases - cum_vacc_est_obs, 
    max_var_cdc = lapply(diff_cdc, find_max_var),
    max_var_death = lapply(diff_death, find_max_var),
    infection_cdc_only = ifelse(max_var_cdc == "Infection", diff_cdc, 0), 
    infection_death_only = ifelse(max_var_death == "Infection", diff_death, 0),
    vaccination_cdc_only = ifelse(max_var_cdc == "Vaccination", -diff_cdc, 0), 
    vaccination_death_only = ifelse(max_var_death == "Vaccination", -diff_death, 0),
    
    # GET percentages immunity
    overall_imm_cdc_lower = cdc_case_vacc_obs_imm_lower, 
    overall_imm_death_lower = death_inf_vacc_obs_imm_lower, 
    
    ## JOINT %
    infection_and_vaccination_cdc_pct_lower = (joint_cdc_count/Population)*100,
    infection_and_vaccination_death_pct_lower = (joint_death_count/Population)*100,
    
    ## INFECTION, ONLY %
    infection_only_cdc_pct_lower = (infection_cdc_only/Population)*100,
    infection_only_death_pct_lower = (infection_death_only/Population)*100, 
    
    ## VACCINATION, ONLY %
    vaccination_only_cdc_pct_lower = (vaccination_cdc_only/Population)*100, 
    vaccination_only_death_pct_lower = (vaccination_death_only/Population)*100
  )
  


##
##
##
## PARTITION DATA BY SCENARIO
##
##
##

## CDC INDEP
S1_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_cdc_indep, infection_cdc_indep, vaccination_cdc_indep)%>%
  merge(filter(start_dates, scenario == "S1"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)

## CDC UPPER
S2_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_cdc_upper, infection_cdc_upper, vaccination_cdc_upper)%>%
  merge(filter(start_dates, scenario == "S2"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)

## CDC LOWER
S3_DAT <- lower_bound_sort %>%
  select(COUNTY, DATE, overall_imm_cdc_lower, infection_and_vaccination_cdc_pct_lower, infection_only_cdc_pct_lower, vaccination_only_cdc_pct_lower)%>%
  merge(filter(start_dates, scenario == "S3"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  mutate(excess = infection_only_cdc_pct_lower - vaccination_only_cdc_pct_lower)

## DEATH INDEP
S4_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_death_indep, infection_death_indep, vaccination_death_indep)%>%
  merge(filter(start_dates, scenario == "S4"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)

## DEATH UPPER 
S5_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_death_upper, infection_death_upper, vaccination_death_upper)%>%
  merge(filter(start_dates, scenario == "S5"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)

## DEATH LOWER
S6_DAT <- lower_bound_sort %>%
  select(COUNTY, DATE, overall_imm_death_lower, infection_and_vaccination_death_pct_lower, infection_only_death_pct_lower, vaccination_only_death_pct_lower)%>%
  merge(filter(start_dates, scenario == "S6"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  mutate(excess = infection_only_death_pct_lower - vaccination_only_death_pct_lower)



