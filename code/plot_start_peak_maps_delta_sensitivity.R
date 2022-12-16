##
#### Create maps for Delta wave COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(magrittr)
library(lubridate)


## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
immunity_est <- read_excel("exported data/immunity_est.xlsx")
immunity_dat <- read_excel("exported data/immunity_vc.xlsx")

immunity_dat$COUNTY <- toupper(immunity_dat$COUNTY)
names(immunity_dat)[1] <- 'CO_NAME'
immunity_est$COUNTY <- toupper(immunity_est$COUNTY)
names(immunity_est)[1] <- 'CO_NAME'
shp$CO_NAME <- toupper(shp$NAME)


## Start looping through sensitivity analysis
for (s in c(paste0("S", 1:6))) {
  
  dat <- read_excel(paste0("sensitivity analysis/outputs/",
                           s,
                           "/delta_county_summary_",
                           s,
                           ".xlsx"))
  
  ## change and merge data
  dat$COUNTY <- toupper(dat$COUNTY)
  names(dat)[1] <- 'CO_NAME'
  
  ## If many entries for peak date, pick middle?
  if (nrow(dat) > 100) {
    
    # Get middle date for each
    dat_summary <- dat %>% group_by(CO_NAME) %>%
      summarize(peak_date_median = median(peak_date))
    
    # Join, subset
    dat %<>% merge(dat_summary,
                   by = "CO_NAME")
    dat %<>% filter(peak_date == peak_date_median)
    
  }
  
  ## merge
  nc_dat <- merge(shp,
                  dat[, c("CO_NAME", "start_date", "peak_date")],
                  by = 'CO_NAME',
                  all = TRUE)

## calculations/conversions
nc_dat$start_date <- as.Date(nc_dat$start_date)
nc_dat$peak_date <- as.Date(nc_dat$peak_date)

## Create state polygon layer
nc_st_poly <- shp %>% summarize()


###'
###'
###'
###' Plot start date map
###' 
###' 
###' 


start_date_map <- 
  tm_shape(nc_dat) + 
  tm_polygons("start_date",
              palette = "YlGnBu", 
              # style = "quantile",
              title = "Start date",
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(legend.outside = TRUE,
            # legend.height = 0.5,
            # legend.text.size = 1,
            # legend.title.size = 1.75,
            inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(start_date_map, 
          filename = paste0("sensitivity analysis/maps/", s, "/date_start_delta_", s, ".png"),
          width = 1000,
          dpi = 140)


###'
###'
###'
###' Plot peak date map
###' 
###' 
###' 

### Some magic to fix the legend issue
# Create new bbox from original layer
bbox_new <- st_bbox(nc_dat)
# Add some to Y max and Y min
# Y min
bbox_new[2] <- bbox_new[2] - 25000
# Y max
bbox_new[4] <- bbox_new[4] + 25000
# Create sf object
bbox_new %<>% st_as_sfc()

peak_date_map <-
  tm_shape(nc_dat,
           bbox = bbox_new) + 
  tm_polygons("peak_date",
              palette = "OrRd", 
              # style = "quantile",
              title = "Peak date",
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(legend.outside = TRUE,
            legend.height = 0.2,
            # legend.text.size = 1,
            # legend.title.size = 1.75,
            inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(peak_date_map, 
          filename = paste0("sensitivity analysis/maps/", s, "/date_peak_delta_", s, ".png"),
          width = 1000,
          dpi = 140)

}
