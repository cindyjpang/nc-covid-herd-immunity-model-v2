### Analysis: Generate Immunity Estimates from Vaccinations
###
### Description: see above
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

vacc_csv <-
  read.csv("data/NC_COUNTY_vaccination_daily_tidy_2021-08-09.csv")

# vaccination cleaning
vacc_dat <-
  vacc_csv[, c('DATE', 'COUNTY', 'DOSE_1', 'DOSE_2', 'DOSE_s')]

vacc_dat$COUNTY = as.factor(vacc_csv$COUNTY)
vacc_dat$DATE = as.Date(vacc_csv$DATE, '%m/%d/%Y')

counties <- unique(vacc_dat$COUNTY)

# create temp frame for storing dates and times for eventual merging

temp <- vacc_dat[, c('DATE', 'COUNTY')]

# process theoretical, assuming perfect behavior
temp$DOSE_1_immunity <- vacc_dat$DOSE_1 * 0.82
temp$DOSE_1_immunity_date <- vacc_dat$DATE + 14

temp$twoDOSE_immunity_date <- vacc_dat$DATE + 28
temp$twoDOSE_total_immunity <- vacc_dat$DOSE_1 * 0.94
temp$DOSE_2_immunity <-
  temp$twoDOSE_total_immunity - temp$DOSE_1_immunity
temp$check <-
  temp$twoDOSE_total_immunity == (temp$DOSE_1_immunity + temp$DOSE_2_immunity)

# based on observational data. Multiply by 0.12 and lag by 7 days
temp$DOSE_2_immunity_obs <- vacc_dat$DOSE_2 * 0.12
temp$DOSE_2_lag_obs <- vacc_dat$DATE + 7

# finally, johnson and johnson, multiply by 0.70 and lag by 14 days
temp$DOSE_s_immunity <- vacc_dat$DOSE_s * 0.70
temp$DOSE_s_lag <- vacc_dat$DATE + 14


# merge 1st dose date
vacc_dat <- merge(
  vacc_dat,
  temp[, c('COUNTY', 'DOSE_1_immunity', 'DOSE_1_immunity_date')],
  by.x = c('COUNTY', 'DATE'),
  by.y = c('COUNTY', 'DOSE_1_immunity_date'),
  all = TRUE
)

# merge 2nd dose date
vacc_dat <- merge(
  vacc_dat,
  temp[, c('COUNTY', 'DOSE_2_immunity', 'twoDOSE_immunity_date')],
  by.x = c('COUNTY', 'DATE'),
  by.y = c('COUNTY', 'twoDOSE_immunity_date'),
  all = TRUE
)

# merge observational data
vacc_dat <- merge(
  vacc_dat,
  temp[, c('COUNTY', 'DOSE_2_immunity_obs', 'DOSE_2_lag_obs')],
  by.x = c('COUNTY', 'DATE'),
  by.y = c('COUNTY', 'DOSE_2_lag_obs'),
  all = TRUE
)

# merge j&j data
vacc_dat <- merge(
  vacc_dat,
  temp[, c('COUNTY', 'DOSE_s_immunity', 'DOSE_s_lag')],
  by.x = c('COUNTY', 'DATE'),
  by.y = c('COUNTY', 'DOSE_s_lag'),
  all = TRUE
)

# '_t' notation: theoretical, assuming perfect behavior, '_obs' denotes observational data
vacc_dat$vacc_immunity_est_t <-
  rowSums(vacc_dat[, c('DOSE_1_immunity', 'DOSE_2_immunity', 'DOSE_s_immunity')], na.rm = TRUE)
vacc_dat$vacc_immunity_est_obs <-
  rowSums(vacc_dat[, c('DOSE_1_immunity', 'DOSE_2_immunity_obs', 'DOSE_s_immunity')], na.rm = TRUE)
vacc_dat$t_obs_diff <-
  abs(vacc_dat$vacc_immunity_est_t - vacc_dat$vacc_immunity_est_obs)

# calculate cumulative vaccinations for the estimates, per county
vacc_dat$cum_vacc_est_t <- ave(vacc_dat$vacc_immunity_est_t, vacc_dat$COUNTY, FUN = cumsum)
vacc_dat$cum_vacc_est_obs <- ave(vacc_dat$vacc_immunity_est_obs, vacc_dat$COUNTY, FUN = cumsum)

# write out to excel file
write_xlsx(vacc_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\vaccinations.xlsx')

