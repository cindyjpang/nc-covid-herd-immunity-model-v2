##
#### Create density plot for start/peak Delta wave COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(magrittr)
library(plyr)
library(ggthemes)


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
                dat[, c("CO_NAME", "start_date", "peak_date")],
                by = 'CO_NAME',
                all = TRUE)

## calculations/conversions
nc_dat$start_date <- as.Date(nc_dat$start_date)
nc_dat$peak_date <- as.Date(nc_dat$peak_date)

## Prep data
start <- data.frame(group = "Start Date", dates = nc_dat$start_date)
peak <- data.frame(group = "Peak Date", dates = nc_dat$peak_date)

## Summarize data to get mean
mu <- ddply(bind_rows(start, peak), 
            "group", 
            summarise, 
            grp.mean = mean(dates))

## Create plot

start_peak_plot <-
  ggplot(bind_rows(start, peak), 
         aes(x = dates, 
             fill = group)) +
  scale_fill_manual(values = c("#4BB3B3", "#F26A42")) +
  geom_density(color = "NA",
               alpha = 0.4) +
  stat_density(geom = "line", 
               color = "black", 
               alpha = 0.25) +
  geom_vline(data = mu, 
             aes(xintercept = grp.mean, 
                 color = group),
             linetype = "dashed",
             alpha = 0.75) +
  geom_text(data = mu, 
            aes(x = grp.mean, 
                y = 0.01,
                label = as.character(grp.mean)), 
            color = c("#1B328A", "#6A0001"))+
  scale_color_manual(values = c("#4BB3B3", "#F26A42")) +
  scale_y_continuous(expand = expansion(add = 0.000)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%Y",
               expand = expansion(add = 1)) +
  xlab("Date") +
  ylab("Density") +
  theme_pander() +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.text.y = element_text(margin = margin(r = 2)),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.text.x = element_text(margin = margin(t = 2.5)),
        axis.line.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.18, "cm"),
        axis.ticks.x = element_line(color = "black", size = 0.6),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(6, 8, 6, 6)) 

ggsave(plot=start_peak_plot, filename = "B.1.617.2_StartPeak_dPlot.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")
