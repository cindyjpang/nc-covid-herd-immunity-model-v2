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


## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
dat <- read_excel("exported data/delta_county_summary.xlsx")
immunity_est <- read_excel("exported data/immunity_est.xlsx")
immunity_dat <- read_excel("exported data/immunity_vc.xlsx")


## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'
immunity_dat$COUNTY <- toupper(immunity_dat$COUNTY)
names(immunity_dat)[1] <- 'CO_NAME'
immunity_est$COUNTY <- toupper(immunity_est$COUNTY)
names(immunity_est)[1] <- 'CO_NAME'
shp$CO_NAME <- toupper(shp$NAME)
## merge
nc_dat <- merge(shp,
                dat[, c("CO_NAME", "start_date")],
                by = 'CO_NAME',
                all = TRUE)
## merge
nc_dat <- merge(nc_dat,
                immunity_dat[, c("CO_NAME", "immunity_mean")],
                by = 'CO_NAME',
                all = TRUE)

## calculations/conversions
nc_dat$immunity_pct <- nc_dat$immunity_mean*100


## Merge data for component mapping 
immunity_components <- merge(nc_dat, 
                             immunity_est[, c('CO_NAME', 'DATE', 'immunity_by_inf', 'immunity_by_vacc')], 
                             by.x = c('CO_NAME', 'start_date'),
                             by.y = c('CO_NAME', 'DATE'), 
                             all.x = TRUE,
                             all.y = FALSE)

## Create state polygon layer
nc_st_poly <- shp %>% summarize()

### Get some values for mapping
imm_max <- max(immunity_components$immunity_pct)
imm_min <- min(c(immunity_components$immunity_by_inf,
                 immunity_components$immunity_by_vacc))
norm_breaks <- c(-Inf, 20, 30, 40, 50, 60, 70, Inf)
norm_colors <- brewer.pal(7, "YlGn")



###'
###'
###'
###' Plot immunity via infection
###' 
###' 
###' 

nc_imm_inf_map <- 
  tm_shape(immunity_components) +                   ## The R object
  tm_polygons("immunity_by_inf",                      ## Column with the data
              title = "",  ## Legend title 
#              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(nc_imm_inf_map, 
          filename = "maps/imm_inf_delta_start.png", 
          width = 1000,
          dpi = 140)



###'
###'
###'
###' Plot immunity via vaccination
###' 
###' 
###' 

nc_imm_vac_map <- 
  tm_shape(immunity_components) +                   ## The R object
  tm_polygons("immunity_by_vacc",                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(nc_imm_vac_map, 
          filename = "maps/imm_vac_delta_start.png", 
          width = 1000,
          dpi = 140)




###'
###'
###'
###' Plot immunity via vaccination
###' 
###' 
###' 

nc_imm_all_map <- 
  tm_shape(immunity_components) +                   ## The R object
  tm_polygons("immunity_pct",                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(nc_imm_all_map, 
          filename = "maps/imm_all_delta_start.png", 
          width = 1000,
          dpi = 140)
