##
#### Get some information for Delta wave COVID-19 paper
##
#### Used Cindy's code, map_county_delta.R
##

library(readxl)
library(tidyverse)
library(magrittr)


## Assumes working directory is "code_vc" (top level!)

## Read data
immunity_est <- read_excel("exported data/immunity_est.xlsx")

##
## Remove extra days from table
##
## Convert to date
immunity_est$DATE <- as.Date(immunity_est$DATE)
## Create weekly vector using min/max date
inf_dates_weekly <- seq.Date(from = min(immunity_est$DATE),
                             to = max(immunity_est$DATE),
                             by = "week")
## Filter data
immunity_est %<>% filter(DATE %in% inf_dates_weekly)


##
## Read model output data
##
immunity_dat <- read_excel("exported data/immunity_vc.xlsx")

## Convert to date
immunity_dat$start_date <- as.Date(immunity_dat$start_date)

## Create empty holder
delta_dat <- NULL

## Filter data to only post beginning of delta wave for each county
for (i in 1:nrow(immunity_dat)) {
  
  ## Create subset
  temp <- immunity_est %>% filter(COUNTY == immunity_dat$COUNTY[i] & DATE >= immunity_dat$start_date[i]-7)
  
  ## Calculate new infections per 10000
  temp$new_inf <- c(NA, diff(temp$immunity_by_inf, lag = 1)) * 100
  
  ## Remove extra
  temp %<>% filter(DATE >= immunity_dat$start_date[i])
  
  ## Select columns and bind to holder
  delta_dat <- bind_rows(delta_dat,
                         temp %>% select(COUNTY,
                                         DATE,
                                         Population,
                                         immunity_by_inf,
                                         immunity_by_vacc,
                                         immunity_mean,
                                         new_inf))
  
}

## Calculate state values
delta_dat_st <- immunity_est
delta_dat_st$new_inf_count_est <- (delta_dat_st$cdc_multiplier + delta_dat_st$death_est_total_inf) / 2
## Consolidate by date
delta_dat_st_summary <- delta_dat_st %>% group_by(DATE) %>% summarize(new_inf_count_est = sum(new_inf_count_est))

## Get state population
st_pop <- delta_dat_st %>% group_by(DATE) %>% summarize(Population = sum(Population))
st_pop <- max(st_pop$Population)

## Calculate state rates
delta_dat_st_summary$new_inf <- 10000 * delta_dat_st_summary$new_inf_count_est / st_pop

## Subset to county data dates
delta_dat_st_summary %<>% filter(DATE >= min(delta_dat$DATE))

## Create plot
new_inf_plot <- 
ggplot(delta_dat, aes(x = DATE,
                      y = new_inf)) +
  geom_line(aes(group = COUNTY),
            color = "black",
            alpha = 0.07) +
  geom_line(data = delta_dat_st_summary,
            aes(x = DATE,
                y = new_inf),
            size = 0.75,
            linetype = "longdash") +
  scale_y_continuous(expand = expansion(add = 10)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%Y",
               expand = expansion(add = 1)) +
  xlab("Date") +
  ylab("New Weekly Infections (per 10,000)") +
  theme_pander() +
  theme(axis.title.y = element_text(margin = margin(r = 5)),
        axis.text.y = element_text(margin = margin(r = 2)),
        axis.text.x = element_text(margin = margin(b = 2)),
        axis.line.x = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0, 4, 6, 6))


ggsave(plot = new_inf_plot, 
       filename = "new_inf_time_delta.png", 
       path = "images", 
       device = "png",
       width = 1800,
       height = 1100,
       units = "px")
