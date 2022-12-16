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
                dat,
                by = 'CO_NAME',
                all = TRUE)
## merge
nc_dat <- merge(nc_dat,
                immunity_dat[, c("CO_NAME", "Vc", "immunity_mean", "Population", "thr_hit", "discrepency", "need_vaccination", "pct_discrepency", "vacc_date")],
                by = 'CO_NAME',
                all = TRUE)

## calculations/conversions
nc_dat$immunity_pct <- nc_dat$immunity_mean*100
nc_dat$hit_pct <- nc_dat$Vc*100
nc_dat$beta.hat <- as.numeric(nc_dat$beta.hat)
nc_dat$gamma.hat <- as.numeric(nc_dat$gamma.hat)
nc_dat$r0.hat <- as.numeric(nc_dat$r0.hat)
nc_dat$start_date <- as.Date(nc_dat$start_date)
nc_dat$peak_date <- as.Date(nc_dat$peak_date)

## Snap dates to beginning of each week for display purpose
nc_dat$start_date_snap <- floor_date(nc_dat$start_date, "weeks")
nc_dat$peak_date_snap <- floor_date(nc_dat$peak_date, "weeks")

## Merge data for component mapping 
immunity_components <- merge(nc_dat, 
                             immunity_est[, c('CO_NAME', 'DATE', 'immunity_by_inf', 'immunity_by_vacc')], 
                             by.x = c('CO_NAME', 'start_date'),
                             by.y = c('CO_NAME', 'DATE'), 
                             all = FALSE)


## Things that I want to report
# Peak infection rate of Delta
# Number/proportion people infected during Delta

### Peak weekly infections
immunity_components$peak_inf_rate <- immunity_components$I_proj_peak / immunity_components$Population


## Create empty holder for total estimated infections during Delta
delta_dat <- NULL

## Filter data to only post beginning of delta wave for each county
for (i in 1:nrow(immunity_dat)) {
  
  ## Create subset
  temp <- immunity_est %>% filter(CO_NAME == immunity_dat$CO_NAME[i] & 
                                    DATE >= immunity_dat$start_date[i]-7 &
                                    DATE < as.Date("2021-12-01"))
  
  ## Calculate mean number of estimated infections
  ##   Using CDC multiplier and Deaths
  temp$cum_inf_estimate <- temp %>% 
    select(cum_cdc_multiplier_cases, cum_death_inf_cases) %>%
    rowMeans()
  
  ## Subtract last row from first row
  delta_inf <- temp$cum_inf_estimate[nrow(temp)] - temp$cum_inf_estimate[1]
  
  ## Select columns and bind to holder
  delta_dat <- bind_rows(delta_dat,
                         tibble(COUNTY = immunity_dat$CO_NAME[i],
                                inf_est_delta = delta_inf,
                                Population = temp$Population[1]))
  
}

## Calculate proportion
delta_dat$inf_est_delta_prop <- delta_dat$inf_est_delta / delta_dat$Population


## Merge Delta infections with data
immunity_components <- merge(immunity_components,
                             delta_dat[,-c(3)],
                             by.x = "CO_NAME",
                             by.y = "COUNTY",
                             all.x = TRUE)


## Create state polygon layer
nc_st_poly <- immunity_components %>% summarize()


###'
###'
###'
###' Plot peak infection rate map
###' 
###' 
###' 
###' 

### Some magic to fix a legend issue
# Create new bbox from original layer
bbox_new <- st_bbox(immunity_components)
# Add some to Y max and Y min
# Y min
bbox_new[2] <- bbox_new[2] - 80000
# Y max
# bbox_new[4] <- bbox_new[4] + 25000
# Create sf object
bbox_new %<>% st_as_sfc()

## Calculate per 10,000
immunity_components$peak_inf_rate10000 <- immunity_components$peak_inf_rate * 10000

### Get some values for mapping
inf_max <- max(immunity_components$peak_inf_rate10000)
inf_min <- min(immunity_components$peak_inf_rate10000)
norm_breaks <- c(-Inf, 75, 100, 125, 150, 200, Inf)
norm_colors <- brewer.pal(7, "YlOrBr")

peak_infrate_map <- 
  tm_shape(immunity_components,
           bbox = bbox_new) + 
  tm_polygons("peak_inf_rate10000",
              palette = norm_colors, 
              breaks = norm_breaks,
              # style = "jenks",
              title = "Peak Weekly Infection \nRate per 10,000 people",
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(#legend.outside = TRUE,
            # title.size = 5,
            # legend.height = 0.5,
            # legend.text.size = 1,
            # legend.title.size = 1,
            inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(peak_infrate_map, 
          filename = paste0("sensitivity analysis/maps/", s, "/peak_inf_rate_", s, ".png"), 
          width = 800,
          dpi = 140)


###'
###'
###'
###' Plot total infections map
###' 
###' 
###' 
###' 

### Some magic to fix a legend issue
# Create new bbox from original layer
bbox_new <- st_bbox(immunity_components)
# Add some to Y max and Y min
# Y min
bbox_new[2] <- bbox_new[2] - 40000
# Y max
# bbox_new[4] <- bbox_new[4] + 25000
# Create sf object
bbox_new %<>% st_as_sfc()

## Calculate percent
immunity_components$inf_est_delta_pct <- immunity_components$inf_est_delta_prop * 100

### Get some values for mapping
inf_max <- max(immunity_components$inf_est_delta_pct)
inf_min <- min(immunity_components$inf_est_delta_pct)
norm_breaks <- c(-Inf, 10, 15, 20, 25, Inf)
norm_colors <- brewer.pal(5, "RdPu")


delta_inf_map <-
  tm_shape(immunity_components,
           bbox = bbox_new) + 
  tm_polygons("inf_est_delta_pct",
              palette = norm_colors, 
              breaks = norm_breaks,
              # style = "quantile",
              title = "Total Infected (%)",
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(#legend.outside = TRUE,
            #legend.height = 0.2,
            # legend.text.size = 1,
            # legend.title.size = 1.75,
            inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)

tmap_save(delta_inf_map, 
          filename = paste0("sensitivity analysis/maps/", s, "/total_inf_delta_", s, ".png"), 
          width = 800,
          dpi = 140)

}
