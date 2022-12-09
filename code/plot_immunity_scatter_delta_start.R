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
library(ggExtra)


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


###'
###'
###'
###' Scatterplot with histograms
###' 
###' 
###' 

mult_value <- 0.055
text_size <- 3.5

imm <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_inf,
             y = immunity_by_vacc)) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value)) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity via Infection (%)") +
  ylab("Immunity via Vaccination (%)") +
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

imm_h <- ggMarginal(imm, 
                           type = "histogram",
                           fill = "grey",
                           color = "grey50")
ggsave(plot = imm, 
       filename = "scatter_imminf_immvacc_delta.png", 
       path = "images", 
       device = "png",
       width = 1300,
       height = 900,
       units = "px")

