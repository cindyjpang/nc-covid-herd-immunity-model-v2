##
#### Create histograms for Delta wave COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)

## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/nc shapefile/counties.shp")
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

## Merge data for component mapping 
immunity_components <- merge(nc_dat %>% st_drop_geometry(), 
                             immunity_est[, c('CO_NAME', 'DATE', 'immunity_by_inf', 'immunity_by_vacc')], 
                             by.x = c('CO_NAME', 'start_date'),
                             by.y = c('CO_NAME', 'DATE'), 
                             all = FALSE)


### Get some values for plotting
imm_max <- max(immunity_components$immunity_pct)
imm_min <- min(c(immunity_components$immunity_by_inf,
                 immunity_components$immunity_by_vacc))
norm_breaks <- seq(10, 75, by = 2.5)


###'
###'
###'
###' Plot immunity via infection
###' 
###' 
###' 

immunity_inf_hist <- ggplot(immunity_components, 
                            aes(x = immunity_by_inf)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = c(0, 21),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Immunity by Infection (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_inf_hist, 
       filename = "hist_imm_inf_delta_start.png", 
       path = "images", 
       device = "png",
       width = 1200,
       height = 900,
       units = "px")


###'
###'
###'
###' Plot immunity via vaccination
###' 
###' 
###' 

immunity_vac_hist <- ggplot(immunity_components, 
                            aes(x = immunity_by_vacc)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = c(0, 21),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Immunity by Vaccination (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_vac_hist, 
       filename = "hist_imm_vacc_delta_start.png", 
       path = "images", 
       device = "png",
       width = 1200,
       height = 900,
       units = "px")


###'
###'
###'
###' Plot immunity via both
###' 
###' 
###' 

immunity_all_hist <- ggplot(immunity_components, 
                            aes(x = immunity_pct)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = c(0, 21),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Overall Immunity (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_all_hist, 
       filename = "hist_imm_all_delta_start.png", 
       path = "images", 
       device = "png",
       width = 1200,
       height = 900,
       units = "px")


###'
###'
###'
###' Plot immunity via infection vs immunity via vaccination
###' 
###' 
###' 
###' 
text_size <- 5

cor.result <- cor.test(immunity_components$immunity_by_inf, immunity_components$immunity_by_vacc)
pval <- cor.result$p.value
rval <- round(cor.result$estimate, digits = 2)


immunity_compare <- ggplot(immunity_components, aes(x = immunity_by_inf, y = immunity_by_vacc))+
  geom_point()+
  xlab("Immunity by Infection (%)")+
  ylab("Immunity by Vaccination (%)")+
  theme_pander()+
  geom_text(x = 45, 
            y = 60, 
            hjust = 0, 
            label = paste0("R = ", rval), 
            size = text_size)+
  geom_text(x = 45, 
            y = 57, 
            hjust = 0, 
            label = "p < 0.01", 
            size = text_size)
ggsave(plot = immunity_compare, 
       filename = "scatterplots_immunity_vaccination_infection.png", 
       path = "images", 
       device = "png",
       width = 1200,
       height = 900,
       units = "px")

###'
###'
###'
###' Plot start vs peak date, response to R.
###' 
###' 
###' 
###
cor.start.peak <- cor.test(immunity_components$start_date, immunity_components$peak_date)
delta_start_peak_cor <- ggplot(immunity_components, aes(x = start_date, y = peak_date))+
  geom_point()+
  xlab("Start Date")+
  ylab("Peak Date")+
  theme_pander()+
  geom_text(x = 45, 
            y = 60, 
            hjust = 0, 
            label = paste0("R = ", round(cor.start.peak$estimate)), 
            size = text_size)+
  geom_text(x = 45, 
            y = 57, 
            hjust = 0, 
            label = "p < 0.01", 
            size = text_size)
  