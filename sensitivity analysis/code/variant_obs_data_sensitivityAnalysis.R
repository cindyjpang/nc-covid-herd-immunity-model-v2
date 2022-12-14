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


scenarios <- c("S1/sir_initCond_cdc_case_vacc_obs_imm.csv", "S2/sir_initCond_cdc_case_vacc_obs_imm_up.csv", "S3/sir_initCond_cdc_case_vacc_obs_imm_lower.csv", "S4/sir_initCond_death_inf_vacc_obs_imm.csv", "S5/sir_initCond_death_inf_vacc_obs_imm_up.csv", "S6/sir_initCond_death_inf_vacc_obs_imm_lower.csv")
# 
for(scenario in scenarios){
  read_file_path <- paste0("./sensitivity analysis/outputs/", scenario)
  print(read_file_path)
  
  nc_dat <- read.csv(read_file_path)
  nc_dat <- filter(nc_dat, I > 0)[, c('COUNTY', 'DATE', 'N', 'I', 'R', 'S')]
  nc_dat$DATE <- as.Date(nc_dat$DATE)
  
  
  ## Set minimum date to May 1st 2021
  #start_search <- as.Date("2021-04-01")
  
  if(scenario == "S4/sir_initCond_death_inf_vacc_obs_imm.csv" | scenario == "S5/sir_initCond_death_inf_vacc_obs_imm_up.csv" | scenario == "S6/sir_initCond_death_inf_vacc_obs_imm_lower.csv"){
    nc_dat <- nc_dat %>%
      group_by(COUNTY)%>%
      mutate(days_since_start = as.numeric(difftime(DATE, as.Date("2020-01-01"), units = c('days'))))%>%
      filter(COUNTY != "Missing")
  }else{ 
    nc_dat <- nc_dat %>%
    group_by(COUNTY)%>%
    mutate(days_since_start = as.numeric(difftime(DATE, as.Date("2020-01-01"), units = c('days'))))%>%
    filter(COUNTY != "Missing", DATE >= as.Date("2021-01-01"))
  }
 
  
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
  nc_dat$COUNTY <- as.factor(nc_dat$COUNTY)
  test <- nc_dat %>% 
    group_by(COUNTY)%>%
    filter(minima == TRUE & DATE >= as.Date("2021-04-30") & DATE <= as.Date("2021-10-01"))%>%
    summarise(min_dates = tail(DATE,2))
  #test <- test[test$min_dates >= as.Date('2021-04-30'), ]
  #test <- test %>% add_count(COUNTY) # filter the dates which are strictly delta vs omicron waves
  View(test)
  
  delta_dates <- test %>%
    group_by(COUNTY)%>%
    summarize(delta_start = min(min_dates))
    
  
  missing_counties <- setdiff(counties, delta_dates$COUNTY)
  
  # number of times to replicate start date
  rep_num <- length(missing_counties)
  
  # build dataframe for binding
  if(rep_num > 0){
    temp_df <- data.frame(COUNTY = missing_counties, 
                          delta_start = rep(as.Date("2021-05-01"), rep_num))
    
    delta_dates <- rbind(delta_dates, temp_df)
  }

  
  delta_obs_dat <- data.frame()
  for(county in counties){
    df <- filter(delta_dates, COUNTY == county)
    delta_start <- df$delta_start
    df_delta <- nc_dat %>%
      filter(COUNTY == county & DATE >= delta_start)%>%
      select(COUNTY, DATE, N, I, R, S)
    df_delta$days <- lapply(df_delta$DATE, function(x) {return(difftime(x, delta_start, units=c('days')))})
    df_delta$days <- as.numeric(unlist(df_delta$days))
    
    delta_obs_dat <- rbind(delta_obs_dat, df_delta)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  path_str <- "./sensitivity analysis/outputs/"
  if(scenario == "S1/sir_initCond_cdc_case_vacc_obs_imm.csv"){
      write_path <- paste0(path_str, "S1/delta_obs_dat_S1.xlsx")
      write_xlsx(delta_obs_dat, write_path)
  }else if(scenario == "S2/sir_initCond_cdc_case_vacc_obs_imm_up.csv"){
    write_path <- paste0(path_str, "S2/delta_obs_dat_S2.xlsx")
    write_xlsx(delta_obs_dat, write_path)
  }else if(scenario == "S3/sir_initCond_cdc_case_vacc_obs_imm_lower.csv"){
    write_path <- paste0(path_str, "S3/delta_obs_dat_S3.xlsx")
    write_xlsx(delta_obs_dat, write_path)
  }else if(scenario == "S4/sir_initCond_death_inf_vacc_obs_imm.csv"){
    write_path <- paste0(path_str, "S4/delta_obs_dat_S4.xlsx")
    write_xlsx(delta_obs_dat, write_path)
  }else if(scenario == "S5/sir_initCond_death_inf_vacc_obs_imm_up.csv"){
    write_path <- paste0(path_str, "S5/delta_obs_dat_S5.xlsx")
    write_xlsx(delta_obs_dat, write_path)
  }else{
    write_path <- paste0(path_str, "S6/delta_obs_dat_S6.xlsx")
    write_xlsx(delta_obs_dat, write_path)
  }
  
  
  
}










#write_xlsx(delta_obs_dat, "./sensitivity analysis/outputs/S1/delta_obs_dat_cdc_case_vacc_obs_imm_up.xlsx")

