##
#### Evaluate relationships, make plots Delta wave COVID-19 paper
##
#### Used Cindy's code, map_county_delta.R
##

library(readxl)
library(tidyverse)
library(magrittr)
library(sf)
library(lubridate)
library(ggthemes)
library(egg)

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

## Read new data
all_scenarios_immunity <- read_xlsx("sensitivity analysis/outputs/all_scenarios_start_immunity.xlsx")

## Replace above 100
all_scenarios_immunity$immunity_all[which(all_scenarios_immunity$immunity_all > 100)] <- 100

all_scenarios_immunity$COUNTY <- toupper(all_scenarios_immunity$COUNTY)


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
  
  ## Subset to scenario
  immunity_components <- all_scenarios_immunity %>% filter(scenario == s)

## Merge data for component mapping 
immunity_components <- merge(nc_dat, 
                             immunity_components, 
                             by.x = c('CO_NAME'),
                             by.y = c('COUNTY'), 
                             all = TRUE)


## Things that I want to report
# Peak infection rate of Delta
# Number/proportion people infected during Delta

### Peak weekly infections
immunity_components$peak_inf_rate <- immunity_components$I_proj_peak / immunity_components$Population


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
immunity_components <- merge(immunity_components,
                             delta_dat[,-c(3)],
                             by.x = "CO_NAME",
                             by.y = "COUNTY",
                             all.x = TRUE)

## Hypotheses
##    Low immunity lead to earlier start date (high immunity, later start)
##    Low immunity lead to higher weekly peak rate (high immunity, lower peak)
##    Low immunity lead to higher infected proportion (high immunity, lower percent inf)

## Convert dates to day of year for correlations
immunity_components$start_date_doy <- yday(immunity_components$start_date)

## Create vector with col numer for our 3 variables
var_cols <- which(names(immunity_components) %in% c("start_date_doy", "peak_inf_rate", "inf_est_delta_prop"))
imm_cols <- which(names(immunity_components) %in% c("immunity_by_infection", "immunity_by_vaccination", "immunity_all"))

## Make empty list
cor_hold <- vector(mode = "list", 
                   length = 9)

## Counter
count <- 1

## Loop through 3 variables, and 3 types of immunity
## Variables
for (i in 1:3) {
  
  ## Immunity
  for (j in 1:3) {
    
    # Var name
    cor_hold[[count]]$var <- names(immunity_components)[var_cols[i]]
    # Imm var name
    cor_hold[[count]]$imm <- names(immunity_components)[imm_cols[j]]
    # Pearson's R
    cor_hold[[count]]$R <- cor.test(immunity_components[,var_cols[i]] %>% st_drop_geometry() %>% unlist(),
                                    immunity_components[,imm_cols[j]] %>% st_drop_geometry() %>% unlist())$estimate
    # p value
    cor_hold[[count]]$p <- cor.test(immunity_components[,var_cols[i]] %>% st_drop_geometry() %>% unlist(),
                                    immunity_components[,imm_cols[j]] %>% st_drop_geometry() %>% unlist())$`p.value`
    
    # Advance counter
    count <- count + 1
 
  }
}

##
## Create plots
##
#               Imm Inf    Imm Vacc    Imm Overall
#  Start Date
#  Peak Inf R
#  Delta Inf P

mult_value <- 0.055
text_size <- 3.5

inf_stdate <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_infection,
             y = start_date_snap)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
  geom_point() +
  scale_y_date(expand = expansion(mult = mult_value),
               limits = range(immunity_components$start_date_snap),
               oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Infection (%)") +
  ylab("Start date") +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = max(immunity_components$start_date_snap),
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[8]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = max(immunity_components$start_date_snap)-10,
            hjust = 0,
            label = if_else(cor_hold[[8]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[8]]$p, 2))),
            size = text_size) +
  theme_bw()

vac_stdate <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_vaccination,
             y = start_date_snap)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_date(expand = expansion(mult = mult_value),
               limits = range(immunity_components$start_date_snap),
               oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Vaccination (%)") +
  ylab("Start date") +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = max(immunity_components$start_date_snap),
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[9]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = max(immunity_components$start_date_snap)-10,
            hjust = 0,
            label = if_else(cor_hold[[9]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[9]]$p, 2))),
            size = text_size) +
  theme_bw()  

ovr_stdate <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_all,
             y = start_date_snap)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_date(expand = expansion(mult = mult_value),
               limits = range(immunity_components$start_date_snap),
               oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Total Immunity (%)") +
  ylab("Start date") +
  geom_text(x = min(immunity_components$immunity_all),
            y = max(immunity_components$start_date_snap),
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[7]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_all),
            y = max(immunity_components$start_date_snap)-10,
            hjust = 0,
            label = if_else(cor_hold[[7]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[7]]$p, 2))),
            size = text_size) +
  theme_bw()  


inf_peakinf <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_infection,
             y = peak_inf_rate * 10000)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$peak_inf_rate * 10000),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Infection (%)") +
  ylab("Peak infection rate (per 10,000)") +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = max(immunity_components$peak_inf_rate) * 10000,
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[2]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = (max(immunity_components$peak_inf_rate) * 10000)-30,
            hjust = 0,
            label = if_else(cor_hold[[2]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[2]]$p, 2))),
            size = text_size) +
  theme_bw()

vac_peakinf <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_vaccination,
             y = peak_inf_rate * 10000)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$peak_inf_rate * 10000),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Vaccination (%)") +
  ylab("Peak infection rate (per 10,000)") +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = max(immunity_components$peak_inf_rate) * 10000,
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[3]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = (max(immunity_components$peak_inf_rate) * 10000)-30,
            hjust = 0,
            label = if_else(cor_hold[[3]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[3]]$p, 2))),
            size = text_size) +
  theme_bw()  

ovr_peakinf <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_all,
             y = peak_inf_rate * 10000)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$peak_inf_rate * 10000),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Total Immunity (%)") +
  ylab("Peak infection rate (per 10,000)") +
  geom_text(x = min(immunity_components$immunity_all),
            y = (max(immunity_components$peak_inf_rate) * 10000),
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[1]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_all),
            y = (max(immunity_components$peak_inf_rate) * 10000)-30,
            hjust = 0,
            label = if_else(cor_hold[[1]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[1]]$p, 2))),
            size = text_size)  +
  theme_bw()  


inf_prop <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_infection,
             y = inf_est_delta_prop * 100)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$inf_est_delta_prop * 100),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Infection (%)") +
  ylab("Population infected (%)") +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = max(immunity_components$inf_est_delta_prop) * 100,
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[5]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_infection),
            y = (max(immunity_components$inf_est_delta_prop) * 100)-2,
            hjust = 0,
            label = if_else(cor_hold[[5]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[5]]$p, 2))),
            size = text_size) +
  theme_bw()

vac_prop <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_by_vaccination,
             y = inf_est_delta_prop * 100)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$inf_est_delta_prop * 100),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity by Vaccination (%)") +
  ylab("Population infected (%)") +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = max(immunity_components$inf_est_delta_prop) * 100,
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[6]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_by_vaccination),
            y = (max(immunity_components$inf_est_delta_prop) * 100)-2,
            hjust = 0,
            label = if_else(cor_hold[[6]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[6]]$p, 2))),
            size = text_size)  +
  theme_bw()  

ovr_prop <-  
  ggplot(data = st_drop_geometry(immunity_components),
         aes(x = immunity_all,
             y = inf_est_delta_prop * 100)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              size = 0,
              alpha = 0.2) +
  geom_line(stat = "smooth", method = "loess",
            color = "black",
            linetype = "longdash",
            size = 0.5,
            alpha = 0.2) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value),
                     limits = range(immunity_components$inf_est_delta_prop * 100),
                     oob = scales::oob_keep) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Total Immunity (%)") +
  ylab("Population infected (%)") +
  geom_text(x = min(immunity_components$immunity_all),
            y = max(immunity_components$inf_est_delta_prop) * 100,
            hjust = 0,
            label = paste0("R = ", round(cor_hold[[4]]$R, 2)),
            size = text_size) +
  geom_text(x = min(immunity_components$immunity_all),
            y = (max(immunity_components$inf_est_delta_prop) * 100)-2,
            hjust = 0,
            label = if_else(cor_hold[[4]]$p < 0.01,
                            "p < 0.01 ",
                            paste0("p = ", round(cor_hold[[4]]$p, 2))),
            size = text_size) +
  theme_bw()  


grl1 <- ggarrange(inf_stdate + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  vac_stdate + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank()) + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  ovr_stdate + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank()) + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  inf_peakinf + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  vac_peakinf + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank()) + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  ovr_peakinf + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank()) + 
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.x = element_blank()),
                  inf_prop,
                  vac_prop + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank() ),
                  ovr_prop + 
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank() ),
                  nrow = 3,
                  ncol = 3)

ggsave(plot = grl1, 
       filename = paste0("scatterplots_immunity_REV1_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
       device = "png")

}








