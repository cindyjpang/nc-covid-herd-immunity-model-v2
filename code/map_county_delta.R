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

setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model")
library(tmap)
library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
library(dplyr)

shp <- st_read("C:\\Users\\cindy\\nc-covid-herd-immunity-model\\data\\nc shapefile\\counties.shp")
dat <- read_excel("C:\\Users\\cindy\\nc-covid-herd-immunity-model\\exported data\\delta_county_summary.xlsx")


## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'

nc_dat <- merge(
  shp, 
  dat, 
  by = 'CO_NAME', 
  all = TRUE
)


tm_shape(nc_dat) + 
  tm_polygons("r0.hat", 
              style = 'cont')