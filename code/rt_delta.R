## Rt calculation
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tmap)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model")

delta_obs_dat <- read_excel("exported data/delta_obs_dat.xlsx")
delta_params <- read_excel("exported data/delta_county_summary.xlsx")
delta_obs_dat$week <- delta_obs_dat$days/7
delta_params$r0.hat <- as.numeric(delta_params$r0.hat)
## susceptible curves
ggplot(delta_obs_dat, aes(x=days,y=S,group=COUNTY, color=COUNTY)) + geom_line()

delta_rt <- merge(delta_obs_dat,
                  delta_params,
                  by = c("COUNTY"),
                  all=TRUE)
delta_rt$st <- delta_rt$S/delta_rt$N
delta_rt$rt <- delta_rt$st*delta_rt$r0.hat

p <- ggplot(delta_rt, aes(x=DATE,y=rt,group=COUNTY, color=COUNTY)) + geom_line()
ggplotly(p)

#get rt at start date, get rt at peak date
delta_rt_start <- delta_rt%>%
  group_by(COUNTY) %>%
  filter(DATE == min(DATE)) %>%
  select(COUNTY, DATE,rt)

delta_rt_peak <- delta_rt%>%
  group_by(COUNTY) %>%
  filter(I == max(I)) %>%
  select(COUNTY, DATE,rt)