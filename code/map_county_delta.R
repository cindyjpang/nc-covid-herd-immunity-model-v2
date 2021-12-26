## ---------------------------
##
## Script name: map_county_delta
##
## Purpose of script: mapping delta wave by county, produce maps for figures in manuscript
##
## Author: Cindy Pang
##
## Date Created: 2021-12-18
##
## Email: pangcind@live.unc.edu
##
## ---------------------------
##    
##
## ---------------------------

setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2")
library(tmap)
library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
shp <- st_read("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2\\data\\nc shapefile\\counties.shp")
dat <- read_excel("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2\\exported data\\delta_county_summary.xlsx")
immunity_est <- read_excel("exported data\\immunity_est.xlsx")
immunity_dat <- read_excel("exported data\\immunity_vc.xlsx")


## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'

immunity_dat$COUNTY <- toupper(immunity_dat$COUNTY)
names(immunity_dat)[1] <- 'CO_NAME'

nc_dat <- merge(
  shp, 
  dat, 
  by = 'CO_NAME', 
  all = TRUE
)

nc_dat <- merge(
  nc_dat,
  immunity_dat[, c("CO_NAME", "Vc", "immunity_mean", "Population", "thr_hit", "discrepency", "need_vaccination", "pct_discrepency", "vacc_date")],
  by = 'CO_NAME',
  all = TRUE
)

nc_dat$immunity_pct <- nc_dat$immunity_mean*100
nc_dat$hit_pct <- nc_dat$Vc*100
nc_dat$beta.hat <- as.numeric(nc_dat$beta.hat)
nc_dat$gamma.hat <- as.numeric(nc_dat$gamma.hat)
nc_dat$r0.hat <- as.numeric(nc_dat$r0.hat)
#tmap_mode("view")
r0_map <- tm_shape(nc_dat) + 
  tm_polygons("r0.hat",
              style = 'fixed',
              breaks = c(0,2,2.5,3.0,3.5,4.0, 100),
              palette = 'Reds')
r0_map

start_date_map <- tm_shape(nc_dat) + 
  tm_polygons("start_date",
              palette = c('#2c7fb8', '#7fcdbb', '#edf8b1'))
start_date_map

peak_date_map <-tm_shape(nc_dat)+
  tm_polygons("peak_date", 
              palette = c('#e34a33', '#fdbb84', '#fee8c8'))
peak_date_map

## slope map not super useful bc not scaled to pop'n
# slope_map <- tm_shape(nc_dat)+
#   tm_polygons('slope', 
#               style = 'fixed',
#               breaks = c(0,5,10,15,20,30,40,100),
#               palette = 'PuRd')
# slope_map


time_to_peak <- tm_shape(nc_dat)+
  tm_polygons('start_peak_diff',
              style = 'fixed',
              breaks = c(0,60,70,80,90,130),
              palette = c("#3182bd", "#9ecae1", "#deebf7"),
              title = "Time to Peak (days)") #darker colors = shorter time to peak
time_to_peak

immunity_map <- tm_shape(nc_dat) + 
  tm_polygons("immunity_pct",
              title = "% Immune",
              palette = 'BuGn')+
  tm_layout(main.title = "NC Immunity % before Delta Wave")
immunity_map


hit_map <- tm_shape(nc_dat) + 
  tm_polygons("hit_pct",
              title = "Herd Immunity Threshold (HIT) %",
              palette = 'BuPu')+
  tm_layout(main.title = "Herd Immunity Threshold (HIT) for Counties in NC for B.1.617.2")

hit_map

pct_disc <- tm_shape(nc_dat) + 
  tm_polygons("pct_discrepency",
              title = "Percent Difference: HIT (%) - % Immune at Start",
              palette = 'Greys')
pct_disc
need_vaccination <- tm_shape(nc_dat) + 
  tm_polygons("need_vaccination",
              title = "Number of People Needed to Vaccinate",
              palette = 'BuPu')+
  tm_layout(main.title = "Number Needed to Vaccinate 2 Weeks before B.1.617.2 Wave to Reach HIT")
need_vaccination


### SCATTERPLOT ANALYSIS 

# r0 vs immunity: hypothesis - no association, maybe higher immunit
r0_immunity <- ggplot(nc_dat, aes(x = immunity_pct, y = r0.hat))+
  geom_point()
r0_immunity

## higher immunity, higher r0.hat

# r0 vs start date 
r0_start <- ggplot(nc_dat, aes(x=r0.hat, y=start_date)) + geom_point()
r0_start

ggplot(nc_dat, aes(x=immunity_pct, y=start_date)) + geom_point()

ggplot(nc_dat, aes(x=r0.hat, y=peak_date)) + geom_point()

ggplot(nc_dat, aes(x=immunity_pct, y=peak_date)) + geom_point()

ggplot(nc_dat, aes(x=immunity_pct, y=start_peak_diff)) + geom_point()

ggplot(nc_dat, aes(x=r0.hat, y=start_peak_diff)) + geom_point()

ggplot(nc_dat, aes(x=Population, y=r0.hat)) + geom_point()

ggplot(nc_dat, aes(x=Population, y=immunity_pct)) + geom_point()

## r0 frame for boxplots 
beta <- data.frame(group = "Beta", value = nc_dat$beta.hat)
gamma <- data.frame(group = "Gamma", value = nc_dat$gamma.hat)
r0 <- data.frame(group = "R0", value = nc_dat$r0.hat)
r0_boxplot <- rbind(beta,gamma,r0)
r0_boxplot$group <- as.factor(r0_boxplot$group)

ggviolin(r0_boxplot, x = "group", y = "value",
          color = "group", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "boxplot")+
  ggtitle("Distributions for R0 parameters for B.1.617.2 (Delta), North Carolina Counties (N = 100)")+
  theme(plot.title = element_text(hjust = 0.5))+
  rremove("legend")+
  xlab("Parameter")+ylab("Value")

# ggsummarystats(
#   r0_boxplot, x = "group", y = "value", 
#   ggfunc = ggviolin, add = c("boxplot", "median_iqr"),
#   color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07")
# )

### TIME SERIES IMMUNITY EST ###
immunity_est$COUNTY <- toupper(immunity_est$COUNTY)
names(immunity_est)[1] <- 'CO_NAME'
immunity_ts <- merge(shp, 
                     immunity_est, 
                     by = "CO_NAME", 
                     all = TRUE)

# get DATES for faceting 
dates <- c("2020-12-27", "2021-01-31", "2021-02-28", "2021-03-28", "2021-04-25")
date_distr <- data.frame()
for(date in dates){
  #print(date)
  date_dat <- immunity_ts %>%
    filter(DATE == as.Date(date))%>%
    select(CO_NAME, DATE, immunity_mean)
  date_distr <- rbind(date_distr,date_dat)
}

tm_shape(date_distr)+
  tm_polygons("immunity_mean",
              title = "% Immune",
              palette = 'BuGn')+
  tm_facets(by = "DATE")
  
plot_dates <- as.data.frame(date_distr)[, c("DATE", "immunity_mean")]

ggviolin(plot_dates, x = "DATE", y = "immunity_mean",
         add = c("boxplot"))+
  ggtitle("% Immune for North Carolina Counties (N = 100) from Start Vaccination to Before B.1.617.2")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("% Immune")

# boxplots for start & peak dates
start <- data.frame(group = "Start Date", dates = nc_dat$start_date)
peak <- data.frame(group = "Peak Date", dates = nc_dat$peak_date)

ggboxplot(rbind(start,peak), x = "group", y = "dates")+
  xlab("")+
  ylab("Month (2021)")+
  ggtitle("Start and Peak Date Distribution for B.1.617.2 in North Carolina Counties (N=100)")+
  rremove("legend")+
  theme(plot.title = element_text(hjust = 0.5))
  

# ggsummarystats(
#   plot_dates, x = "DATE", y = "immunity_mean",
#   ggfunc = ggviolin, add = c("boxplot", "median_iqr"),
#   color = "DATE"
# )
