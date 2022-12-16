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
library(ggExtra)


## Assumes working directory is "code_vc" (top level!)

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
                  immunity_dat[, c("CO_NAME", "Population")],
                  by = 'CO_NAME',
                  all = TRUE)
  
  ## calculations/conversions
  nc_dat$beta.hat <- as.numeric(nc_dat$beta.hat)
  nc_dat$gamma.hat <- as.numeric(nc_dat$gamma.hat)
  nc_dat$r0.hat <- as.numeric(nc_dat$r0.hat)
  nc_dat$start_date <- as.Date(nc_dat$start_date)
  nc_dat$peak_date <- as.Date(nc_dat$peak_date)
  
  ## Snap dates to beginning of each week for display purpose
  nc_dat$start_date_snap <- floor_date(nc_dat$start_date, "weeks")
  nc_dat$peak_date_snap <- floor_date(nc_dat$peak_date, "weeks")
  
  
  ## Things that I want to report
  # Peak infection rate of Delta
  # Number/proportion people infected during Delta
  
  ### Peak weekly infections
  nc_dat$peak_inf_rate <- nc_dat$I_proj_peak / nc_dat$Population
  
  
  ## Create empty holder for total estimated infections during Delta
  delta_dat <- NULL
  
  ## Read in data for calcualtion
  inf_delta <- read_xlsx(paste0("sensitivity analysis/outputs/",
                                s,
                                "/delta_obs_dat_",
                                s,
                                ".xlsx"))
  
  ## change and merge data
  inf_delta$COUNTY <- toupper(inf_delta$COUNTY)
  names(inf_delta)[1] <- 'CO_NAME'
  
  ## Filter data to only post beginning of delta wave for each county
  for (i in 1:nrow(nc_dat)) {
    
    ## Create subset
    temp <- inf_delta %>% filter(CO_NAME == nc_dat$CO_NAME[i] & 
                                   DATE < as.Date("2021-12-01"))
    
    ## Calculate number of estimated infections
    delta_inf <- sum(temp$I, na.rm = TRUE)
    
    ## Select columns and bind to holder
    delta_dat <- bind_rows(delta_dat,
                           tibble(COUNTY = immunity_dat$CO_NAME[i],
                                  inf_est_delta = delta_inf,
                                  Population = temp$N[1]))
    
  }
  
  ## Calculate proportion
  delta_dat$inf_est_delta_prop <- delta_dat$inf_est_delta / delta_dat$Population
  
  
  ## Merge Delta infections with data
  immunity_components <- merge(nc_dat,
                               delta_dat[,-c(3)],
                               by.x = "CO_NAME",
                               by.y = "COUNTY",
                               all.x = TRUE)
  


###'
###'
###'
###' Scatterplot with histograms
###' 
###' 
###' 

## Calculate per 10,000
immunity_components$peak_inf_rate10000 <- immunity_components$peak_inf_rate * 10000

## Calculate percent
immunity_components$inf_est_delta_pct <- immunity_components$inf_est_delta_prop * 100

## Calculate correlation
inf_peak_tot_cor <- cor.test(immunity_components$peak_inf_rate, immunity_components$inf_est_delta_pct)

mult_value <- 0.055
text_size <- 3.5

inf_stdate <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = peak_inf_rate10000,
             y = inf_est_delta_pct)) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value)) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Peak Weekly Infection Rate (per 10,000 people)") +
  ylab("Total Infected (%)") +
  geom_text(x = max(immunity_components$peak_inf_rate10000)*0.75,
            y = (min(immunity_components$inf_est_delta_pct)*1.25)+2,
            hjust = 0,
            label = paste0("R = ", round(inf_peak_tot_cor$estimate, 2)),
            size = text_size) +
  geom_text(x = max(immunity_components$peak_inf_rate10000)*0.75,
            y = (min(immunity_components$inf_est_delta_pct)*1.25),
            hjust = 0,
            label = "p < 0.05",
            size = text_size) +
  theme_bw() +  
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))

inf_stdate_h <- ggMarginal(inf_stdate, 
                           type = "histogram",
                           fill = "grey",
                           color = "grey50")

ggsave(plot = inf_stdate_h, 
       filename = paste0("scatter_peakinf_totalinf_delta_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
       device = "png",
       width = 1200,
       height = 1000,
       units = "px")

}
