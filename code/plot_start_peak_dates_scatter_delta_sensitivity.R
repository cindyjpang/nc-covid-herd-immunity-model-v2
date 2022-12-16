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
library(ggExtra)


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
                  dat[, c("CO_NAME", "start_date", "peak_date")],
                  by = 'CO_NAME',
                  all = TRUE)
  
  ## calculations/conversions
  nc_dat$start_date <- as.Date(nc_dat$start_date)
  nc_dat$peak_date <- as.Date(nc_dat$peak_date)



###'
###'
###'
###' Scatterplot 
###' 
###' 
###' 

mult_value <- 0.055
text_size <- 3.5

date <-  
  ggplot(data = st_drop_geometry(nc_dat),
         aes(x = start_date,
             y = peak_date)) +
  geom_point() +
  scale_y_date(expand = expansion(mult = mult_value)) +
  scale_x_date(expand = expansion(mult = mult_value)) +
  xlab("Start Date") +
  ylab("Peak Date") +
  # geom_text(x = 210,
  #           y = 8,
  #           hjust = 0,
  #           label = paste0("R = ", round(inf_peak_tot_cor$estimate, 2)),
  #           size = text_size) +
  # geom_text(x = 210,
  #           y = 6.25,
  #           hjust = 0,
  #           label = "p < 0.01",
  #           size = text_size) +
  theme_bw() +  
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))


ggsave(plot = date, 
       filename = paste0("scatter_dates_delta_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
       device = "png",
       width = 1300,
       height = 900,
       units = "px")

}

