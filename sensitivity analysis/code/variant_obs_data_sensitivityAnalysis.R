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

setwd("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2")
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)
require(deSolve)
library(zoo)
library(ggpmisc)


nc_dat <- read.csv("./exported data/sensitivity analysis data export/SIR Initial Condition/sir_initCond_death_inf_vacc_obs_imm.csv")
nc_dat <- filter(nc_dat, I!=0)[, c('COUNTY', 'DATE', 'N', 'I', 'R', 'S')]
nc_dat$DATE <- as.Date(nc_dat$DATE)


## Set minimum date to May 1st 2021
start_search <- as.Date("2021-04-01")
nc_dat <- nc_dat %>%
  #filter(DATE >= start_search)%>%
  group_by(COUNTY)%>%
  mutate(days_since_start = as.numeric(difftime(DATE, start_search, units = c('days'))))
#)

# counties <- unique(nc_dat$COUNTY)
# 
# for(county in counties){
#   df <- filter(nc_dat, COUNTY == county)
#   print(ggplot(data = df)+geom_col(aes(DATE, I), alpha=0.7) + geom_line(aes(DATE, mav), col = 'red')+ggtitle(county))
# }

counties <- unique(nc_dat$COUNTY)
smoothed_inf <- c()

# look at the smoothed curves 
for(county in counties){
  df <- filter(nc_dat, COUNTY == county)
  loessMod_op <- loess(I ~ days_since_start, data=df, span=0.3)
  smoothed_op <- predict(loessMod_op)
  smoothed_inf <- append(smoothed_inf, smoothed_op)
  print(ggplot(data = df)+geom_col(aes(DATE, I), alpha=0.7) + geom_line(aes(DATE, smoothed_op), col = 'red')+ggtitle(county))
}
nc_dat$smoothed_inf <- smoothed_inf
nc_dat <- nc_dat %>% group_by(COUNTY) %>% mutate(minima = ggpmisc:::find_peaks(-smoothed_inf, na.rm = TRUE))
#nc_dat[nc_dat$COUNTY == 'Graham' & nc_dat$minima == TRUE & nc_dat$days_since_start == 462, "minima"] <- FALSE

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
test <- test[test$min_dates >= as.Date('2021-04-01'), ]
test <- test %>% add_count(COUNTY) # filter the dates which are strictly delta vs omicron waves
View(test)



























# plot county data

# counties <- unique(nc_dat$COUNTY)
# smoothed_inf <- c()
# smoothed_df <- data.frame()
# # look at the smoothed curves 
# for(county in counties){
#   df <- filter(nc_dat, COUNTY == county)
#   if(length(df$COUNTY) > 4){
#     poly_df <- lm(df$I ~ poly(df$days_since_start, 4))
#     
#     # loessMod_op <- loess(I ~ days_since_start, data=df, span=1)
#     smoothed_op <- poly_df$fitted.values
#     smoothed_inf <- append(smoothed_inf, smoothed_op)
#     print(ggplot(data = df)+geom_col(aes(DATE, I), alpha=0.7) + geom_line(aes(DATE, smoothed_op), col = 'red')+ggtitle(county))
#   }else{
#     smoothed_inf <- append(smoothed_inf, df$I)
#   } 
#     
# }
# 
# nc_dat$smoothed_inf <- smoothed_inf
#nc_dat <- nc_dat %>% group_by(COUNTY) %>% mutate(minima = ggpmisc:::find_peaks(-mav, na.rm = TRUE))

# get the delta start dates, if minimia = TRUE, get the earliest date, else (i.e all minima false) -> pick the date with the smallest value
# for(county in counties){
#   df <- filter(nc_dat, COUNTY == county)
#   if(any(df$minima ==TRUE)){
#     print(filter(df, minima == TRUE))
#   }else{
#     print(df)
#   }
# }
# 




# 
# #sanity check - plot the minimum points via ggplot
# for(county in counties){
#   df <- filter(nc_dat, COUNTY == county)
#   print(paste(county, sep=" : ",df$DATE[which(df$minima == TRUE)] ))
#   c <- df$DATE[which(df$minima == TRUE)]
#   print(c)
#   q <- ggplot(df) + geom_col(aes(DATE, I), alpha = 0.7) +geom_line(aes(DATE, smoothed_inf), col = 'red') + ggtitle(county)
#   print(q + geom_vline(xintercept = c))
# }
