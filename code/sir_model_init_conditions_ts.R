## ---------------------------
##
## Script name: SIR Model Initial Conditions as Time Series 
##
## Purpose of script: plot SIR observational data, these conditions specify initial condition
##
## Author: Cindy Pang
##
## Date Created: 2021-08-25
##
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes: majority of this script is used as a sanity check, initial conditions that will be used for eventual projections two weeks into 
##   
##
## ---------------------------

# import libraries 
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)

# import files 
immunity_est <- read_excel("exported data/immunity_est.xlsx")
immunity_est$infection_count <- rowMeans(immunity_est[, c('cdc_multiplier', 'death_est_total_inf')], na.rm = TRUE)

# plotting infection counts as a sanity check
# df <- immunity_est %>%
#   filter(COUNTY %in% unique(immunity_est$COUNTY))
# 
# p <- ggplot(df, aes(x = DATE, y = infection_count)) +
#   geom_bar(
#     aes(color = COUNTY, fill = COUNTY),
#     stat = "identity", position = position_dodge(0.8),
#     width = 0.7
#   )
# 
# p

sir_model <- immunity_est[, c('COUNTY', 'DATE', 'Population','immunity_mean', 'infection_count')]
sir_model <- mutate(
  sir_model, 
  N = Population, 
  I = infection_count, 
  R = Population*(immunity_mean/100) - I, 
  S = N - I - R
)


# plotting susceptible, recovery curves as a sanity check 
for(county in unique(sir_model$COUNTY)){
  county_dat <- filter(sir_model, COUNTY == county)
  plot(county_dat$DATE, county_dat$S, type = 'l', col = 'green', main = county, xlab = 'Date', ylab = 'Counts', ylim = c(0, county_dat$Population[1]) )
  lines(county_dat$DATE, county_dat$I, col = 'red', ylim = c(0, county_dat$Population[1]))
  lines(county_dat$DATE, county_dat$R, col = 'blue', ylim = c(0, county_dat$Population[1]))                     
}
sir_model <- subset(sir_model, COUNTY != 'Missing')
# write out excel files with SIR Initial Conditions Time-Series 
write_xlsx(sir_model, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\sir_model_ts_initial_conditions.xlsx')