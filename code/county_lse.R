## ---------------------------
##
## Script name: county_lse
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
## https://rpubs.com/choisy/sir
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
library(plyr)
library(purrr)

# import data for both omicron and delta, fit delta first, do predictive modeling for omicron 
delta_obs_dat <- read_excel("exported data/delta_obs_dat.xlsx")
omicron_obs_dat <- read_excel("exported data/omicron_obs_dat.xlsx")
delta_obs_dat$week <- delta_obs_dat$days/7

## delta 
counties <- unique(delta_obs_dat$COUNTY)
delta_params <- data.frame()
delta_proj_dat <- data.frame()
for(county in counties){
  county_df <- delta_obs_dat %>%
    filter(COUNTY == county)
  county_infections <- delta_obs_dat %>%
    filter(COUNTY == county) %>%
    select(week, I) 
  names(county_infections)[2]<- 'infections'
  
  pomp(
    data=county_infections,
    times="week",t0=0,
    skeleton=vectorfield(
      Csnippet("
      DS = -Beta*S*I/N;
      DI = Beta*S*I/N-gamma*I;
      DR = gamma*I;")),
    rinit=Csnippet("
      S = S_0;
      I = I_0;
      R = N-S_0-I_0;"),
    statenames=c("S","I","R"),
    paramnames=c("Beta","gamma","N","S_0","I_0")) -> sir_frame
  
  f1 <- function (beta,gamma) {
    params <- c(Beta=beta,gamma=gamma,N=county_df$N[1],S_0= county_df$S[1],I_0=county_df$I[1])
    x <- trajectory(sir_frame,params=params)
    discrep <- x["I",,]-obs(sir_frame)
    sum(discrep^2)
  }
  print(county)
  
  beta <- seq(from=0,to=10,by=0.2)
  gamma <- seq(from = 0, to = 2, by=0.1)
  param_grid <- expand.grid(beta,gamma)
  param_grid$SSE <- with(param_grid, map2(Var1, Var2, f1))
  param_grid$SSE <- as.numeric(param_grid$SSE)
  best_fit_params <- param_grid[param_grid$SSE == min(param_grid$SSE), ]
  
  print(best_fit_params)
  
  beta.hat <- best_fit_params$Var1[1]
  gamma.hat <- best_fit_params$Var2[1]
  
  coef(sir_frame) <- c(Beta = beta.hat, gamma = gamma.hat, N=county_df$N[1],S_0= county_df$S[1],I_0=county_df$I[1])
  x <- trajectory(sir_frame,format="data.frame")
  dat <- join(as.data.frame(sir_frame),x,by='week')
  dat <- dat %>%
    mutate(COUNTY = county) %>%
    relocate(COUNTY, .before = week)
  print(dat)
  p <- ggplot(dat,aes(x=week))+
    geom_line(aes(y=infections),color='black')+
    geom_line(aes(y=I),color='red') +
    ggtitle(county)## better fit
  print(p)
  delta_params <- rbind(delta_params, c(county, beta.hat, gamma.hat, beta.hat/gamma.hat))
  delta_proj_dat <- rbind(delta_proj_dat, dat)
  
}
names(delta_params)[1]<- 'COUNTY'
names(delta_params)[2]<- 'beta.hat'
names(delta_params)[3]<- 'gamma.hat'
names(delta_params)[4]<- 'r0.hat'

delta_proj_dat <- merge(delta_proj_dat, 
                        delta_obs_dat[, c('COUNTY', 'DATE', 'week')], 
                        by = c('COUNTY', 'week'), 
                        all = TRUE)

delta_summary_dat <- delta_proj_dat %>%
  group_by(COUNTY) %>%
  filter(I == max(I))%>%
  select(COUNTY,DATE, I)

names(delta_summary_dat)[2] <- 'peak_date'
names(delta_summary_dat)[3] <- 'I_proj_peak'

delta_start <- delta_proj_dat %>%
  group_by(COUNTY) %>%
  filter(DATE == min(DATE)) %>%
  select(COUNTY, DATE,I)
names(delta_start)[2] <- 'start_date'
names(delta_start)[3] <- 'I_start'

delta_summary_dat <- merge(delta_summary_dat, 
                           delta_start,
                           by = c('COUNTY'),
                           all = TRUE)
delta_summary_dat <- delta_summary_dat %>%
  mutate(start_peak_diff = as.numeric(difftime(peak_date, start_date, units = c('days'))),
         infection_change = I_proj_peak - I_start,
         slope = infection_change/start_peak_diff)
# final merge with params (r0, beta, and gamma)
delta_summary_dat <- merge(delta_summary_dat, 
                          delta_params,
                          by = c('COUNTY'),
                          all = TRUE)
write_xlsx(delta_summary_dat, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\delta_county_summary.xlsx')
#write_xlsx(delta_params, 'C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\delta_params.xlsx')
delta_proj_dat$COUNTY <- as.factor(delta_proj_dat$COUNTY)
delta_proj_dat%>%
  ggplot(aes(x=DATE, y=I, group=COUNTY, color=COUNTY))+
  geom_line()

