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

setwd("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2")
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
shp <- st_read("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\data\\nc shapefile\\counties.shp")
dat <- read_excel("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\exported data\\delta_county_summary.xlsx")
immunity_est <- read_excel("exported data\\immunity_est.xlsx")
immunity_dat <- read_excel("exported data\\immunity_vc.xlsx") #Vc is critical threshold based on R0 values 


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

nc_dat$start_date <- as.Date(nc_dat$start_date)
nc_dat$peak_date <- as.Date(nc_dat$peak_date)

r0_map <- tm_shape(nc_dat) + 
  tm_polygons("r0.hat",
              style = 'fixed',
              breaks = c(0,2,2.5,3.0,3.5,4.0, 100),
              palette = 'Reds', 
              title = "R0 estimates")+
  tm_layout(main.title = "R0 estimates of B.1.617.2 Wave",
            main.title.position = c("left", "top"), 
            main.title.size = 1.15,
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(r0_map, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_R0_Map.png")

start_date_map <- tm_shape(nc_dat) + 
  tm_polygons("start_date",
              palette = c('#2c7fb8', '#7fcdbb', '#edf8b1'), 
              style = "cont",
              title = "Start Date")+
  tm_layout(main.title = "Start Date of B.1.617.2 Wave",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)

tmap_save(start_date_map, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_StartDate_Map.png")

peak_date_map <-tm_shape(nc_dat)+
  tm_polygons("peak_date", 
              palette = c('#e34a33', '#fdbb84', '#fee8c8'), 
              title = "Peak Date")+
  tm_layout(main.title = "Peak Date of B.1.617.2 Wave",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(peak_date_map, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_PeakDate_Map.png")

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
              title = "Time to Peak (days)")+
  tm_layout(main.title = "Time to Peak of B.1.617.2 Wave",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(time_to_peak, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_TimeToPeak_Map.png")


immunity_map <- tm_shape(nc_dat) + 
  tm_polygons("immunity_pct",
              title = "% Immune",
              palette = 'PuBuGn')+
  tm_layout(main.title = "NC Immunity % before B.1.617.2 Wave",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(immunity_map, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_ImmunityPrior_Map.png")
  
## histogram for immunity prior to delta 
ggplot(nc_dat, aes(x=immunity_pct))+
  geom_histogram(alpha = 0.3, color = "black")+
  labs(title="Immunity % Prior to B.1.617.2",x="% Immune", y = "Count", subtitle = "NC Counties (N=100)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#ggsave(plot=pct_diff, filename = "B.1.617.2_ImmunityPct_Hist.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")

hit_map <- tm_shape(nc_dat) + 
  tm_polygons("hit_pct",
              title = "Herd Immunity Threshold (HIT) %",
              palette = 'BuPu')+
  tm_layout(main.title = "Herd Immunity Threshold (HIT) for Counties in NC for B.1.617.2",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(hit_map, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_HIT_Map.png")


pct_disc <- tm_shape(nc_dat) + 
  tm_polygons("pct_discrepency",
              title = "Percent Difference: HIT (%) - % Immune at Start",
              palette = 'Greys')+
  tm_layout(title = "Percent Difference: HIT (%) - % Immune at Start")
pct_disc

# make histogram of percent differences and compare with the histogram of need vaccination - symmetric
pct_diff <- ggplot(nc_dat, aes(x=pct_discrepency))+
  geom_histogram(alpha = 0.3, color = "black")+
  labs(title="Percent Difference = HIT (%) - % Immune at Start ",x="% Difference", y = "Count", subtitle = "NC Counties (N=100)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggsave(plot=pct_diff, filename = "B.1.617.2_PctDiff_Hist.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")

need_vaccination <- tm_shape(nc_dat) + 
  tm_polygons("need_vaccination",
              title = "Number of People Needed to Vaccinate",
              palette = 'BuPu')+
  tm_layout(main.title = "Number Needed to Vaccinate 2 Weeks before B.1.617.2 Wave to Reach HIT",
            main.title.position = c("left", "top"), 
            legend.outside = TRUE,
            frame = FALSE)+
  tm_scale_bar(color.dark = "gray60", 
               position = c("left", "bottom"),
               text.size = 0.8)
tmap_save(need_vaccination, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_NNV_Map.png")

# histogram of need vaccination - skew 
nnv_hist <- ggplot(nc_dat, aes(x=need_vaccination))+
  geom_histogram(alpha = 0.3, color = "black")+
  labs(title="Number of People Needed to Vaccinate 2 Weeks before B.1.617.2 Wave to Reach HIT ",x="Number of People Needed to Vaccinate", y = "Count", subtitle = "NC Counties (N=100)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave(plot=nnv_hist, filename = "B.1.617.2_NNV_Hist.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")




## r0 frame for boxplots 
beta <- data.frame(group = "Beta", value = nc_dat$beta.hat)
gamma <- data.frame(group = "Gamma", value = nc_dat$gamma.hat)
r0 <- data.frame(group = "R0", value = nc_dat$r0.hat)
hit <- data.frame(group = "HIT", value = 1-1/nc_dat$r0.hat)
recovery_time <- data.frame(group = "Recovery Time", value = (1/nc_dat$gamma.hat)*7)
r0_boxplot_ab <- rbind(beta, gamma, r0)
r0_boxplot <- rbind(beta,gamma,r0, hit, recovery_time)
r0_boxplot$group <- as.factor(r0_boxplot$group)

r0_bp <- ggviolin(r0_boxplot_ab, x = "group", y = "value",
          color = "group", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "boxplot")+
  ggtitle("Distributions for R0 parameters for B.1.617.2 (Delta), North Carolina Counties (N = 100)")+
  theme(plot.title = element_text(hjust = 0.5))+
  rremove("legend")+
  xlab("Parameter")+ylab("Value")
ggsave(plot=r0_bp, filename = "B.1.617.2_R0_Params_vPlot.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")


## get summary statistics for r0 parameters 
r0_boxplot$group <- as.factor(r0_boxplot$group)
r0_summary <- r0_boxplot %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    iqr = IQR(value)
  )
names(r0_summary)[1] <- "Parameter"
r0_summary




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

# plot immunity over time
immunity_ts <- tm_shape(date_distr)+
  tm_polygons("immunity_mean",
              title = "% Immune",
              palette = 'BuGn')+
  tm_facets(by = "DATE")+
  tm_layout(main.title = "% Immune Prior to B.1.617.2",
            main.title.position = c("left", "top"), 
            main.title.size = 1.5)
tmap_save(immunity_ts, filename = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\maps\\B.1.617.2_Immunity_TS_Map.png")

plot_dates <- as.data.frame(date_distr)[, c("DATE", "immunity_mean")]
plot_dates$DATE <- as.character(plot_dates$DATE) #makes appending easier
prior_delta <- data.frame(DATE = "Start_Delta*", immunity_mean = nc_dat$immunity_pct)
plot_dates <- rbind(plot_dates, prior_delta)
immunity_ts_vplot <- ggviolin(plot_dates, x = "DATE", y = "immunity_mean",
         add = c("boxplot"))+
  ggtitle("% Immune for North Carolina Counties (N = 100) from Start Vaccination to Before B.1.617.2")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("% Immune")+
  labs(caption = "*dates range from 2021-05-09 to 2021-07-04")
ggsave(plot=immunity_ts_vplot, filename = "B.1.617.2_Immunity_TS_vPlot.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")

# get summary data
plot_dates$DATE <- as.factor(plot_dates$DATE)
immunity_ts_summary <- plot_dates %>%
  group_by(DATE) %>%
  summarise(
    mean = mean(immunity_mean),
    sd = sd(immunity_mean),
    median = median(immunity_mean),
    iqr = IQR(immunity_mean)
  )
immunity_ts_summary


# boxplots for start & peak dates
start <- data.frame(group = "Start Date", dates = nc_dat$start_date)
peak <- data.frame(group = "Peak Date", dates = nc_dat$peak_date)


library(plyr)
mu <- ddply(rbind(start,peak), "group", summarise, grp.mean=mean(dates))
head(mu)

start_peak_plot <-ggplot(rbind(start,peak), aes(x=dates, fill=group))+
  geom_density(alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype='dashed')+
  geom_text(data = mu, aes(x=grp.mean, label = as.character(grp.mean), y=0.01), color = c('red', 'blue'))+
  xlab("Month")+
  ylab("Density")+
  ggtitle("Start and Peak Date Distribution for B.1.617.2 in North Carolina Counties (N=100)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())
ggsave(plot=start_peak_plot, filename = "B.1.617.2_StartPeak_dPlot.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")

### CASE RATE Analysis 
nc_dat <- nc_dat %>%
  mutate(case_rate_to_peak = infection_change/Population)

# plot case rate to peak against immunity_pct 
ggplot(nc_dat, aes(x = immunity_pct, y = case_rate_to_peak)) + geom_point()
model <- lm(case_rate_to_peak ~ immunity_pct, data = nc_dat)
summary(model)
## results: R^2_Adjusted = -0.006113, p-value = 0.5293, VERY BAD FIT
delta_proj <- read_excel("exported data\\delta_proj.xlsx")
cum_sum_df <- delta_proj %>% 
  group_by(COUNTY)%>%
  mutate(cum_sum = cumsum(infections))%>%
  filter(DATE == max(DATE))
cum_sum_df$COUNTY <- toupper(cum_sum_df$COUNTY)
nc_dat <- merge(nc_dat,
                cum_sum_df[, c("COUNTY", "cum_sum")],
                by.x = c("CO_NAME"),
                by.y = c("COUNTY"),
                all = TRUE)
nc_dat$cum_case_rate <- nc_dat$cum_sum/nc_dat$Population
ggplot(nc_dat, aes(x = immunity_pct, y = cum_case_rate)) + geom_point()
model2 <- lm(cum_case_rate ~ immunity_pct, data = nc_dat)
summary(model2)

hist(nc_dat$Population)

ggplot(nc_dat, aes(x=Population, y=need_vaccination))+geom_point()
ggplot(nc_dat, aes(x=Population, y=need_vaccination))+geom_point()