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
  geom_text(x = 210,
            y = 8,
            hjust = 0,
            label = paste0("R = ", round(inf_peak_tot_cor$estimate, 2)),
            size = text_size) +
  geom_text(x = 210,
            y = 6.25,
            hjust = 0,
            label = "p < 0.01",
            size = text_size) +
  theme_bw() +  
  theme(axis.text.y = element_text(margin = margin(r = 2)),
                     plot.margin = margin(10, 4, 7, 4))
  
inf_stdate_h <- ggMarginal(inf_stdate, 
                         type = "histogram",
                         fill = "grey",
                         color = "grey50")
ggsave(plot = inf_stdate_h, 
       filename = "scatter_peakinf_totalinf_delta.png", 
       path = "images", 
       device = "png",
       width = 1200,
       height = 1000,
       units = "px")
