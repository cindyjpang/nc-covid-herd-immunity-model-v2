## ---------------------------
##
## Script name: variant_observational_data
##
## Purpose of script: find beta and gamma terms to calculate the r0 value
##
## Author: Cindy Pang
##
## Date Created: 2021-12-15
##
## Email: pangcind@live.unc.edu
##
## ---------------------------
##    
## Code Ref: https://kingaa.github.io/thid/odes/ODEs_in_R.pdf
## Methods: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7570398/
## ---------------------------

setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model")
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)
require(deSolve)
library(zoo)
library(ggpmisc)
library(pomp)

# import data 
nc_dat <- read_excel("exported data/sir_model_ts_initial_conditions.xlsx")
nc_dat <- filter(nc_dat, I!=0)[, c('COUNTY', 'DATE', 'N', 'I', 'R', 'S')]

##########################################################
#                                                        #
#                                                        #
# PART A: DATA CLEANING - Find Start & End Dates for Delta and Omicron, 
# Split Analysis based on variant
#                                                        #
#                                                        #
##########################################################
## transform the data to reflect days since the start of the pandemic/when data collection started
min_dates <- aggregate(DATE ~ COUNTY, data = nc_dat, FUN = min)
nc_dat <- merge(
  nc_dat,
  min_dates,
  by = "COUNTY",
  all = TRUE
)
nc_dat <- nc_dat %>% 
  rename(
    DATE = DATE.x,
    start_date = DATE.y) %>% 
    mutate(days_since_start = as.numeric(difftime(DATE, start_date, units = c('days'))))
# plot county data

counties <- unique(nc_dat$COUNTY)
smoothed_inf <- c()

# look at the smoothed curves 
for(county in counties){
  df <- filter(nc_dat, COUNTY == county)
  loessMod_op <- loess(I ~ days_since_start, data=df, span=0.2)
  smoothed_op <- predict(loessMod_op)
  smoothed_inf <- append(smoothed_inf, smoothed_op)
  print(ggplot(data = df)+geom_col(aes(DATE, I), alpha=0.7) + geom_line(aes(DATE, smoothed_op), col = 'red')+ggtitle(county))
}
nc_dat$smoothed_inf <- smoothed_inf
nc_dat <- nc_dat %>% group_by(COUNTY) %>% mutate(minima = ggpmisc:::find_peaks(-smoothed_inf, na.rm = TRUE))
nc_dat[nc_dat$COUNTY == 'Graham' & nc_dat$minima == TRUE & nc_dat$days_since_start == 462, "minima"] <- FALSE

#sanity check - plot the minimum points via ggplot
for(county in counties){
  df <- filter(nc_dat, COUNTY == county)
  print(paste(county, sep=" : ",df$DATE[which(df$minima == TRUE)] ))
  c <- df$DATE[which(df$minima == TRUE)]
  print(c)
  q <- ggplot(df) + geom_col(aes(DATE, I), alpha = 0.7) +geom_line(aes(DATE, smoothed_inf), col = 'red') + ggtitle(county)
  print(q + geom_vline(xintercept = c))
}


# test for the best minima, screens out noise, out of the three which has the min(smoothed_inf)
test <- nc_dat %>% group_by(COUNTY)%>%filter(minima == TRUE)%>%summarise(min_dates = tail(DATE,2))
test <- test[test$min_dates >= as.Date('2021-04-30'), ]
test <- test %>% add_count(COUNTY) # filter the dates which are strictly delta vs omicron waves
View(test)

## build start date and end date df for delta analysis 

delta_dates <- test %>% group_by(COUNTY)%>%summarise(delta_start = min(min_dates),
                                                     delta_end = max(min_dates)) %>% mutate(no_omicron = (delta_start == delta_end))
## build start date and end date df for omicron analysis 
omicron_dates <- delta_dates %>% group_by(COUNTY) %>% filter(no_omicron == FALSE) %>% select(COUNTY, delta_end) %>% rename(omicron_start = delta_end)


## build out observational data by variant
delta_obs_dat <- data.frame()
omicron_obs_dat <- data.frame()
for(county in counties){
  delta_start <- filter(delta_dates, COUNTY == county)$delta_start
  no_omicron <- filter(delta_dates, COUNTY == county)$no_omicron
  delta_end <- filter(delta_dates, COUNTY == county)$delta_end
  if(no_omicron == FALSE){ ## omicron variant present
    df_delta <-  nc_dat %>%
      filter(COUNTY == county & between(DATE, delta_start, delta_end, incbounds = TRUE)) %>%
      select(COUNTY, DATE,N, I, R, S)
    df_omicron <- nc_dat %>%
      filter(COUNTY == county & DATE >= delta_end) %>%
      select(COUNTY, DATE,N, I, R, S)
  }else{ ## no omicron variant yet...
    df_delta <-  nc_dat %>%
      filter(COUNTY == county & DATE >= delta_start) %>%
      select(COUNTY, DATE, N, I, R, S)
  }

    df_delta$days <- lapply(df_delta$DATE, function(x) {return(difftime(x, delta_start, units=c('days')))})
    df_delta$days <- as.numeric(unlist(df_delta$days))
    df_omicron$days <- lapply(df_omicron$DATE, function(x) {return(difftime(x, delta_end, units=c('days')))})
    df_omicron$days <- as.numeric(unlist(df_omicron$days))
    
    delta_obs_dat <- rbind(delta_obs_dat, df_delta)
    omicron_obs_dat <- rbind(omicron_obs_dat, df_omicron)

}

## write out observational data for fitting 
write_xlsx(delta_obs_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\delta_obs_dat.xlsx')
write_xlsx(omicron_obs_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\omicron_obs_dat.xlsx')
