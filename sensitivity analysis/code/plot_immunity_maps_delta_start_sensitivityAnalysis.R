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
  mutate(overall_cdc_indep = cdc_case_vacc_obs_imm, 
         overall_cdc_upper = cdc_case_vacc_obs_imm_up, 
         overall_cdc_lower = cdc_case_vacc_obs_imm_lower,
         infection_cdc_indep = ((cum_cdc_multiplier_cases-joint_cdc_case_obs)/Population)*100,
         infection_cdc_upper = (cum_cdc_multiplier_cases/Population)*100,
         joint_cdc_vacc_lower_pct = cdc_case_vacc_obs_imm_lower - infection_cdc_upper - pct_vacc_est_obs)