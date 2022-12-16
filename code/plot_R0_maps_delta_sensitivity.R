##
#### Create maps for R0 and Herd Immunity Delta wave COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(magrittr)


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
      dplyr::summarize(peak_date_median = median(peak_date))
    
    # Join, subset
    dat %<>% merge(dat_summary,
                   by = "CO_NAME")
    dat %<>% filter(peak_date == peak_date_median)
    
  }
  
  ## merge
  nc_dat <- merge(shp,
                  dat,
                  by = 'CO_NAME',
                  all = TRUE)
  
  ## calculations/conversions
  nc_dat$start_date <- as.Date(nc_dat$start_date)
  nc_dat$peak_date <- as.Date(nc_dat$peak_date)
  
## merge
# nc_dat <- merge(nc_dat,
#                 immunity_dat,
#                 by = 'CO_NAME',
#                 all = TRUE)

## Create state polygon layer
nc_st_poly <- shp %>% summarize()

### Get some values for mapping
r0_max <- max(nc_dat$r0.hat, na.rm = TRUE)
r0_min <- min(nc_dat$r0.hat, na.rm = TRUE)
norm_breaks <- c(-Inf, 2.5, 3, 3.5, 4, Inf)
norm_colors <- brewer.pal(5, "BuPu")



###'
###'
###'
###' Plot R0
###' 
###' 
###' 

nc_r0_map <- 
  tm_shape(nc_dat) +                   ## The R object
  tm_polygons("r0.hat",                      ## Column with the data
              title = "",  ## Legend title 
#              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = FALSE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_add_legend(type = "fill", 
                labels = c("Less than 2.5 (< 60.00%)", 
                           "2.5 - 3.0 (60.00% - 66.67%)", 
                           "3.0 - 3.5 (66.67% - 71.43%)", 
                           "3.5 - 4.0 (71.43% - 75.00%)", 
                           "Greater than 4.0 (> 75.00%)"),
                col = norm_colors,
                border.lwd = 0.5,
                title = expression('R'[0]*' (HIT)')) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(nc_r0_map, 
          filename = paste0("sensitivity analysis/maps/", s, "/r0_delta_", s, ".png"), 
          width = 1000,
          dpi = 140)

}

