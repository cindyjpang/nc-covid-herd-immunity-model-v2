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
## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
immunity_est <- read_excel("exported data/immunity_est.xlsx")

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
         infection_death_upper = (cum_death_inf_cases/Population)*100)