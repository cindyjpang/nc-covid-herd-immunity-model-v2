## ---------------------------
##
## Script name: map_county_delta
##
## Purpose of script: mapping delta wave by county, produce maps for figures in manuscript
##
## Author: Cindy Pang
##
## Date Created: 2021-12-18
##
## Email: pangcind@live.unc.edu
##
## ---------------------------
##    
##
## ---------------------------

setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2")
library(tmap)
library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
library(dplyr)

shp <- st_read("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2\\data\\nc shapefile\\counties.shp")
dat <- read_excel("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2\\exported data\\delta_county_summary.xlsx")



## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'


nc_dat <- merge(
  shp, 
  dat, 
  by = 'CO_NAME', 
  all = TRUE
)

#tmap_mode("view")
r0_map <- tm_shape(nc_dat) + 
  tm_polygons("r0.hat",
              style = 'cont',
              palette = 'YlOrRd')
r0_map

start_date_map <- tm_shape(nc_dat) + 
  tm_polygons("start_date",
              palette = c('#2c7fb8', '#7fcdbb', '#edf8b1'))
start_date_map

peak_date_map <-tm_shape(nc_dat)+
  tm_polygons("peak_date", 
              palette = c('#e34a33', '#fdbb84', '#fee8c8'))
peak_date_map

slope_map <- tm_shape(nc_dat)+
  tm_polygons('slope', 
              style = 'fixed',
              breaks = c(0,5,10,15,20,30,40,100),
              palette = 'PuRd')
slope_map

time_to_peak <- tm_shape(nc_dat)+
  tm_polygons('start_peak_diff',
              style = 'fixed',
              breaks = c(0,60,65,70,75,80,85,90,100,130),
              palette = c('#31a354','#addd8e','#f7fcb9' )) #darker colors = shorter time to peak
time_to_peak
#tmap_options(max.categories = )