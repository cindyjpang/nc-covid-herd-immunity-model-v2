### Analysis: Calculate Case Estimates from Death Numbers and CDC Multiplier 
###
### Description: see above
###
###
### Written by: Cindy Pang

setwd("C:/Users/cindy/nc-covid-herd-immunity-model-v2")
# import packages
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(data.table)

# load data
case_dat <- read_excel("data/CasesDemographics_Age_121421.xlsx")

# rename columns
old_names <- names(case_dat)
new_names <- c('Index', 'DATE', 'COUNTY', 'Cases0_17', 'Deaths0_17', 'Cases18_24', 'Deaths18_24', 'Cases25_49', 'Deaths25_49', 'Cases50_64', 'Deaths50_64', 'Cases65_74', 'Deaths65_74', 'Cases75_', 'Deaths75_', 'Supr_Cases', 'Supr_Deaths', 'Miss_Cases', 'Miss_Deaths')

setnames(case_dat,old_names, new_names)

# delete index and col names
case_dat <- case_dat[-c(1), -c(1)]

# set date to datetime obj, factor counties
case_dat$DATE <- as.Date(case_dat$DATE, "%m/%d/%Y") + 7
case_dat$COUNTY <- as.factor(case_dat$COUNTY)

case_dat <- case_dat[order(case_dat$COUNTY, case_dat$DATE), ]
case_dat <- cbind(case_dat[, c('DATE', 'COUNTY')], lapply(case_dat[, -c(1,2)], as.numeric))

# calculated reported cases and multiplier 
case_dat$total_reported_cases <- rowSums(case_dat[, c('Cases0_17', 'Cases18_24', 'Cases25_49', 'Cases50_64', 'Cases65_74', 'Cases75_', 'Supr_Cases', 'Miss_Cases')], na.rm = TRUE)
case_dat$cdc_multiplier <- case_dat$total_reported_cases*4.0

###################################################################### death-infection estimates
death_dat <- case_dat[, c('DATE', 'COUNTY', 'Deaths0_17', 'Deaths18_24', 'Deaths25_49', 'Deaths50_64', 'Deaths65_74', 'Deaths75_', 'Supr_Deaths', 'Miss_Deaths')]

### DEFINE CONSTANTS ###
# Source: Centers for Disease Control and Prevention. COVID-19 pandemic planning scenarios. Updated July 1, 2020.
# Accessed November 12, 2020. https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios-h.pdf

IFR <- 0.0065  # infection-fatality-rate

# TIME-BASED CONSTANTS, accounts for lags
incubation_period <- 6 # mean time from exposure to symptom onset

symptom_death_18.49 <- 15 # median # of days from symptom onset to death, 18-49 y. IQR = (9, 23)
symptom_death_50.64 <- 15 # median # of days from symptom onset to death, 50-64 y. IQR = (9, 25)
symptom_death_65_ <- 12 # median # of days from symptom onset to death, 65+ y. IQR = (7, 19)
symptom_death_def <- 15 # default set

death_report_18.49 <- 7 # median # of days from death to reporting, 18-49 y. IQR = (3, 18)
death_report_50.64 <- 7 # median # of days from death to reporting, 50-64 y. IQR = (2, 19)
death_report_65_ <- 6   # median # of days from death to reporting, 65+ y. IQR = (2, 18)
death_report_def <- 7   # default set

total_time_lag_18.49 <- incubation_period + symptom_death_18.49 + death_report_18.49
total_time_lag_50.64 <- incubation_period + symptom_death_50.64 + death_report_50.64
total_time_lag_65_ <- incubation_period + symptom_death_65_ + death_report_65_
total_time_lag_def <- incubation_period + symptom_death_def + death_report_def

### Construct Buckets to Capture Variation in time lags
death_dat$Deaths_18.49 <- rowSums(death_dat[, c('Deaths18_24', 'Deaths25_49')], na.rm = TRUE)
death_dat$Deaths65_ <- rowSums(death_dat[, c('Deaths65_74', 'Deaths75_')], na.rm = TRUE)
death_dat$Deaths_oth <- rowSums(death_dat[, c('Deaths0_17','Supr_Deaths','Miss_Deaths' )], na.rm = TRUE)

est_val <- data.frame(
  WeekDate = death_dat$DATE,
  County = death_dat$COUNTY,
  est_inf_18.49 = death_dat$Deaths_18.49/IFR,
  est_inf_50.64 = death_dat$Deaths50_64/IFR,
  est_inf_65_ = death_dat$Deaths65_/IFR, 
  est_inf_oth = death_dat$Deaths_oth/IFR,
  WeekDate_lag_18.49 = death_dat$DATE - total_time_lag_18.49, 
  WeekDate_lag_50.64 = death_dat$DATE - total_time_lag_50.64,
  WeekDate_lag_65_ = death_dat$DATE - total_time_lag_65_, 
  WeekDate_lag_oth = death_dat$DATE - total_time_lag_def
)
death_dat <-merge(
  death_dat,
  est_val[c('County', 'WeekDate_lag_18.49', 'est_inf_18.49')],
  by.x = c("COUNTY", "DATE"),
  by.y = c("County", "WeekDate_lag_18.49"),
  all.x = TRUE,
  all.y = TRUE
)
death_dat <-merge(
  death_dat,
  est_val[c('County', 'WeekDate_lag_50.64', 'est_inf_50.64')],
  by.x = c("COUNTY", "DATE"),
  by.y = c("County", "WeekDate_lag_50.64"),
  all.x = TRUE,
  all.y = TRUE
)

death_dat <-merge(
  death_dat,
  est_val[c('County', 'WeekDate_lag_65_', 'est_inf_65_')],
  by.x = c("COUNTY", "DATE"),
  by.y = c("County", "WeekDate_lag_65_"),
  all.x = TRUE,
  all.y = TRUE
)

death_dat <-merge(
  death_dat,
  est_val[c('County', 'WeekDate_lag_oth', 'est_inf_oth')],
  by.x = c("COUNTY", "DATE"),
  by.y = c("County", "WeekDate_lag_oth"),
  all.x = TRUE,
  all.y = TRUE
)

death_dat$death_est_total_inf <- rowSums(death_dat[, c('est_inf_18.49', 'est_inf_50.64', 'est_inf_65_', 'est_inf_oth')], na.rm = TRUE)

death_dat$cum_death_inf_est <- ave(death_dat$death_est_total_inf, death_dat$COUNTY, FUN = cumsum)


# write out files 
write_xlsx(case_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\reported_case_inf_est.xlsx')
write_xlsx(death_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\death_inf_est.xlsx')


