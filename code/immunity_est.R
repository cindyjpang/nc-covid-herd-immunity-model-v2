### Analysis: Estimate Immunity Levels by County 
###
### Description: aggregate vaccination and case estimates data 
###
###
### Written by: Cindy Pang

# import packages
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)

# import files 
death_inf_est <- read_excel("exported data/death_inf_est.xlsx")
reported_case_inf_est <- read_excel('exported data/reported_case_inf_est.xlsx')
vaccinations <- read_excel('exported data/vaccinations.xlsx')
population <- read_excel('data/census2020_county_pop.xlsx')
# aggregate and merge data

immunity_dat <- merge(
  reported_case_inf_est[, c('COUNTY', 'DATE', 'total_reported_cases', 'cdc_multiplier')],
  death_inf_est[, c('COUNTY', 'DATE', 'death_est_total_inf')], 
  by = c('COUNTY', 'DATE'), 
  all = TRUE
)

immunity_dat <- merge(
  immunity_dat, 
  vaccinations[, c('COUNTY', 'DATE', 'vacc_immunity_est_t', 'vacc_immunity_est_obs')], 
  by = c('COUNTY', 'DATE'), 
  all = TRUE
)

immunity_dat <- merge(
  immunity_dat, 
  population[, c('County', 'Population')], 
  by.x = 'COUNTY',
  by.y = 'County',
  all = TRUE
)

immunity_dat <- immunity_dat[order(immunity_dat$COUNTY, immunity_dat$DATE), ]
# fill NA with 0 to make immunity calculations easier
immunity_dat[is.na(immunity_dat)] <- 0 

immunity_dat <- mutate(
  immunity_dat, 
  cum_reported_cases = ave(total_reported_cases, COUNTY, FUN = cumsum), 
  cum_cdc_multiplier_cases = ave(cdc_multiplier, COUNTY, FUN = cumsum), 
  cum_death_inf_cases = ave(death_est_total_inf, COUNTY, FUN = cumsum),
  cum_vacc_est_t = ave(vacc_immunity_est_t, COUNTY, FUN = cumsum), 
  cum_vacc_est_obs = ave(vacc_immunity_est_obs, COUNTY, FUN = cumsum), 
  
  # check percent vaccinated
  pct_vacc_est_t = cum_vacc_est_t/Population,
  pct_vacc_est_obs = cum_vacc_est_obs/Population,
  
  ## joint probability: assume P(case), P(vacc) are independent events, therefore by assumption of independence, 
  ## P(case & vacc) = P(case)*P(vacc)
  ### define: P(case) = case/population, P(vacc) = vaccination/population
  ###         P(case & vacc) = (case*vaccination)/population^2, but this only gives a proportion
  ### # of joint = P(case & vacc)*Population
  ###            = (case*vaccination)/Population
  
  joint_rep_case_t = (cum_reported_cases*cum_vacc_est_t)/Population, 
  joint_rep_case_obs = (cum_reported_cases*cum_vacc_est_obs)/Population, 
  joint_cdc_case_t = (cum_cdc_multiplier_cases*cum_vacc_est_t)/Population, 
  joint_cdc_case_obs = (cum_cdc_multiplier_cases*cum_vacc_est_obs)/Population, 
  joint_death_inf_t = (cum_death_inf_cases*cum_vacc_est_t)/Population, 
  joint_death_inf_obs = (cum_death_inf_cases*cum_vacc_est_obs)/Population,
  
  ## create upper bound limits 
  rep_case_vacc_t_imm_up = (cum_reported_cases + cum_vacc_est_t)*100/Population,
  rep_case_vacc_obs_imm_up = (cum_reported_cases + cum_vacc_est_obs)*100/Population,
  cdc_case_vacc_t_imm_up = (cum_cdc_multiplier_cases + cum_vacc_est_t)*100/Population,
  cdc_case_vacc_obs_imm_up = (cum_cdc_multiplier_cases + cum_vacc_est_obs)*100/Population,
  death_inf_vacc_t_imm_up = (cum_death_inf_cases + cum_vacc_est_t)*100/Population,
  death_inf_vacc_obs_imm_up = (cum_death_inf_cases + cum_vacc_est_obs)*100/Population, 
  
  ## most likely bounds
  rep_case_vacc_t_imm = (cum_reported_cases + cum_vacc_est_t - joint_rep_case_t)*100/Population,
  rep_case_vacc_obs_imm = (cum_reported_cases + cum_vacc_est_obs - joint_rep_case_obs)*100/Population,
  cdc_case_vacc_t_imm = (cum_cdc_multiplier_cases + cum_vacc_est_t - joint_cdc_case_t)*100/Population,
  cdc_case_vacc_obs_imm = (cum_cdc_multiplier_cases + cum_vacc_est_obs - joint_cdc_case_obs)*100/Population,
  death_inf_vacc_t_imm = (cum_death_inf_cases + cum_vacc_est_t - joint_death_inf_t)*100/Population,
  death_inf_vacc_obs_imm = (cum_death_inf_cases + cum_vacc_est_obs - joint_death_inf_obs)*100/Population,
  
  ## pure infections daily count, no joint probabilities
  
)
immunity_dat$immunity_mean = rowMeans(immunity_dat[,c(
  'cdc_case_vacc_t_imm',
  'cdc_case_vacc_obs_imm',
  'death_inf_vacc_t_imm',
  'death_inf_vacc_obs_imm'
)])



# write out to excel file 
write_xlsx(immunity_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\immunity_est.xlsx')

