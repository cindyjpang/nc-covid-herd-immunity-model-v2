##
#### Create maps for Delta wave COVID-19 paper
##
#### Built from Paul's code, plot_immunity_maps_delta_start.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(dplyr)
## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
immunity_est <- read_excel("exported data/immunity_est.xlsx")


## Read start dates for each scenario
scenarios <- c("S1", "S2", "S3", "S4", "S5", "S6")
start_dates <- data.frame()
for(scenario in scenarios){
  read_file <- paste0("./sensitivity analysis/outputs/", scenario,"/delta_county_summary_", scenario, ".xlsx")
  
  
  file_dat <- read_excel(read_file)
  
  ## clean file
  temp <- file_dat %>%
    select(COUNTY, start_date)%>%
    mutate(scenario = scenario)%>%
    group_by(COUNTY)%>%
    summarize(start_date = min(start_date))%>%
    mutate(scenario = scenario)
  print(temp)
  start_dates <- rbind(start_dates, temp)
  
}


##
##
##
## CONSTRUCT SCENARIOS 
##
##
##

immunity_scenarios_raw <- immunity_est %>%
  mutate( ### CDC MULTIPLIER SCENARIOS
         overall_cdc_indep = cdc_case_vacc_obs_imm, 
         overall_cdc_upper = cdc_case_vacc_obs_imm_up, 
         overall_cdc_lower = cdc_case_vacc_obs_imm_lower, # max(cum_cdc_multiplier_cases, cum_vacc_est_obs)
         infection_cdc_indep = ((cum_cdc_multiplier_cases)/Population)*100,
         infection_cdc_upper = (cum_cdc_multiplier_cases/Population)*100,
         vaccination_cdc_indep = ((cum_vacc_est_obs)/Population)*100, 
         vaccination_cdc_upper = (cum_vacc_est_obs/Population)*100, 
         
         ### DEATH INF SCENARIOS
         overall_death_indep = death_inf_vacc_obs_imm, 
         overall_death_upper = death_inf_vacc_obs_imm_up, 
         overall_death_lower = death_inf_vacc_obs_imm_lower, 
         infection_death_indep = ((cum_death_inf_cases)/Population)*100,
         infection_death_upper = (cum_death_inf_cases/Population)*100,
         vaccination_death_indep = ((cum_vacc_est_obs)/Population)*100,
         vaccination_death_upper = (cum_vacc_est_obs/Population)*100)

# HELPER FNC FOR 
find_max_var <- function(diff){
  str = ""
  
  if(diff > 0){
    str = "Infection"
  }else if(diff < 0){
    str = "Vaccination"
  }else{
    str = "Equal"
  }
  return(str)
}

# GET LOWER BOUND DATA 
lower_bound_sort <- immunity_est %>%
  mutate(
    joint_cdc_count = pmin(cum_cdc_multiplier_cases, cum_vacc_est_obs), 
    joint_death_count = pmin(cum_death_inf_cases, cum_vacc_est_obs),
    diff_cdc = cum_cdc_multiplier_cases - cum_vacc_est_obs, 
    diff_death = cum_death_inf_cases - cum_vacc_est_obs, 
    max_var_cdc = lapply(diff_cdc, find_max_var),
    max_var_death = lapply(diff_death, find_max_var),
    infection_cdc_only = ifelse(max_var_cdc == "Infection", diff_cdc, 0), 
    infection_death_only = ifelse(max_var_death == "Infection", diff_death, 0),
    vaccination_cdc_only = ifelse(max_var_cdc == "Vaccination", -diff_cdc, 0), 
    vaccination_death_only = ifelse(max_var_death == "Vaccination", -diff_death, 0),
    
    # GET percentages immunity
    overall_imm_cdc_lower = cdc_case_vacc_obs_imm_lower, 
    overall_imm_death_lower = death_inf_vacc_obs_imm_lower, 
    
    ## JOINT %
    infection_and_vaccination_cdc_pct_lower = (joint_cdc_count/Population)*100,
    infection_and_vaccination_death_pct_lower = (joint_death_count/Population)*100,
    
    ## INFECTION, ONLY %
    infection_only_cdc_pct_lower = (infection_cdc_only/Population)*100,
    infection_only_death_pct_lower = (infection_death_only/Population)*100, 
    
    ## VACCINATION, ONLY %
    vaccination_only_cdc_pct_lower = (vaccination_cdc_only/Population)*100, 
    vaccination_only_death_pct_lower = (vaccination_death_only/Population)*100
  )
  


##
##
##
## PARTITION DATA BY SCENARIO
##
##
##

col_names <- c("COUNTY", "DATE", "immunity_all", "immunity_by_infection", "immunity_by_vaccination")
## CDC INDEP
S1_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_cdc_indep, infection_cdc_indep, vaccination_cdc_indep)%>%
  merge(filter(start_dates, scenario == "S1"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  select(-scenario)
colnames(S1_DAT) <- col_names

## CDC UPPER
S2_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_cdc_upper, infection_cdc_upper, vaccination_cdc_upper)%>%
  merge(filter(start_dates, scenario == "S2"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  select(-scenario)

colnames(S2_DAT) <- col_names

## CDC LOWER
S3_DAT <- lower_bound_sort %>%
  select(COUNTY, DATE, overall_imm_cdc_lower, infection_and_vaccination_cdc_pct_lower, infection_only_cdc_pct_lower, vaccination_only_cdc_pct_lower)%>%
  merge(filter(start_dates, scenario == "S3"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  mutate(excess = infection_only_cdc_pct_lower - vaccination_only_cdc_pct_lower,
         infection_imm_cdc_lower = infection_and_vaccination_cdc_pct_lower + infection_only_cdc_pct_lower,
         vaccination_imm_cdc_lower = infection_and_vaccination_cdc_pct_lower + vaccination_only_cdc_pct_lower)%>%
  select(COUNTY, DATE, overall_imm_cdc_lower, infection_imm_cdc_lower, vaccination_imm_cdc_lower)
  
colnames(S3_DAT) <- col_names

## DEATH INDEP
S4_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_death_indep, infection_death_indep, vaccination_death_indep)%>%
  merge(filter(start_dates, scenario == "S4"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  select(-scenario)

colnames(S4_DAT) <- col_names

## DEATH UPPER 
S5_DAT <- immunity_scenarios_raw %>%
  select(COUNTY, DATE, overall_death_upper, infection_death_upper, vaccination_death_upper)%>%
  merge(filter(start_dates, scenario == "S5"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  select(-scenario)

colnames(S5_DAT) <- col_names

## DEATH LOWER
S6_DAT <- lower_bound_sort %>%
  select(COUNTY, DATE, overall_imm_death_lower, infection_and_vaccination_death_pct_lower, infection_only_death_pct_lower, vaccination_only_death_pct_lower)%>%
  merge(filter(start_dates, scenario == "S6"),
        by.x = c("COUNTY", "DATE"), 
        by.y = c("COUNTY", "start_date"), 
        all = FALSE)%>%
  mutate(excess = infection_only_death_pct_lower - vaccination_only_death_pct_lower, 
         infection_imm_death_lower = infection_and_vaccination_death_pct_lower + infection_only_death_pct_lower,
         vaccination_imm_death_lower = infection_and_vaccination_death_pct_lower + vaccination_only_death_pct_lower)%>%
  select(COUNTY, DATE, overall_imm_death_lower, infection_imm_death_lower, vaccination_imm_death_lower)
colnames(S6_DAT) <- col_names

write_xlsx(S1_DAT, "./sensitivity analysis/outputs/S1/S1_start_immunity.xlsx")
write_xlsx(S2_DAT, "./sensitivity analysis/outputs/S2/S2_start_immunity.xlsx")
write_xlsx(S3_DAT, "./sensitivity analysis/outputs/S3/S3_start_immunity.xlsx")
write_xlsx(S4_DAT, "./sensitivity analysis/outputs/S4/S4_start_immunity.xlsx")
write_xlsx(S5_DAT, "./sensitivity analysis/outputs/S5/S5_start_immunity.xlsx")
write_xlsx(S6_DAT, "./sensitivity analysis/outputs/S6/S6_start_immunity.xlsx")


















##
##
##
## MAPPING ALL SCENARIOS
## 
##
##

## Create state polygon layer
nc_st_poly <- shp %>% summarize()

### Get some values for mapping
#imm_max <- max(immunity_components$immunity_pct)
#imm_min <- min(c(immunity_components$immunity_by_inf,
#                 immunity_components$immunity_by_vacc))
norm_breaks <- c(-Inf, 20, 30, 40, 50, 60, 70, Inf)
norm_colors <- brewer.pal(7, "YlGn")


###'
###'
###'
###' Merge shapefile + data for all scenarios 
###' 
###' 
###' 

S1_MAP_DAT <- merge(shp, 
                  S1_DAT, 
                  by.x = "NAME", 
                  by.y = "COUNTY", 
                  all = TRUE)

S2_MAP_DAT <- merge(shp, 
                    S2_DAT, 
                    by.x = "NAME", 
                    by.y = "COUNTY", 
                    all = TRUE)

S3_MAP_DAT <- merge(shp, 
                    S3_DAT, 
                    by.x = "NAME", 
                    by.y = "COUNTY", 
                    all = TRUE)


S4_MAP_DAT <- merge(shp, 
                    S4_DAT, 
                    by.x = "NAME", 
                    by.y = "COUNTY", 
                    all = TRUE)

S5_MAP_DAT <- merge(shp, 
                    S5_DAT, 
                    by.x = "NAME", 
                    by.y = "COUNTY", 
                    all = TRUE)

S6_MAP_DAT <- merge(shp, 
                    S6_DAT, 
                    by.x = "NAME", 
                    by.y = "COUNTY", 
                    all = TRUE)










###'
###'
###'
###' Plot overall immunity
###' 
###' 
###' 

s <- c("immunity_all", "immunity_by_infection", "immunity_by_vaccination")

# S1
nc_imm_overall_map_S1 <- 
  tm_shape(S1_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S1

tmap_save(nc_imm_overall_map_S1, 
          filename = "sensitivity analysis/maps/S1/imm_all_delta_start_S1.png", 
          width = 1000,
          dpi = 140)

# S2
nc_imm_overall_map_S2 <- 
  tm_shape(S2_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S2

tmap_save(nc_imm_overall_map_S2, 
          filename = "sensitivity analysis/maps/S2/imm_all_delta_start_S2.png", 
          width = 1000,
          dpi = 140)

# S3

nc_imm_overall_map_S3 <- 
  tm_shape(S3_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S3

tmap_save(nc_imm_overall_map_S3, 
          filename = "sensitivity analysis/maps/S3/imm_all_delta_start_S3.png", 
          width = 1000,
          dpi = 140)

# S4
nc_imm_overall_map_S4 <- 
  tm_shape(S4_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S4

tmap_save(nc_imm_overall_map_S4, 
          filename = "sensitivity analysis/maps/S4/imm_all_delta_start_S4.png", 
          width = 1000,
          dpi = 140)

# S5
nc_imm_overall_map_S5 <- 
  tm_shape(S5_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S5

tmap_save(nc_imm_overall_map_S5, 
          filename = "sensitivity analysis/maps/S5/imm_all_delta_start_S5.png", 
          width = 1000,
          dpi = 140)

# s6
nc_imm_overall_map_S6 <- 
  tm_shape(S6_MAP_DAT) +                   ## The R object
  tm_polygons(s[1],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_overall_map_S6

tmap_save(nc_imm_overall_map_S6, 
          filename = "sensitivity analysis/maps/S6/imm_all_delta_start_S6.png", 
          width = 1000,
          dpi = 140)



###'
###'
###'
###' Plot immunity via infection
###' 
###' 
###' 

nc_imm_inf_map_S1 <- 
  tm_shape(S1_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S1

tmap_save(nc_imm_inf_map_S1, 
          filename = "sensitivity analysis/maps/S1/imm_inf_delta_start_S1.png", 
          width = 1000,
          dpi = 140)

# S2
nc_imm_inf_map_S2 <- 
  tm_shape(S2_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S2

tmap_save(nc_imm_inf_map_S2, 
          filename = "sensitivity analysis/maps/S2/imm_inf_delta_start_S2.png", 
          width = 1000,
          dpi = 140)

# S3
nc_imm_inf_map_S3 <- 
  tm_shape(S3_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S3

tmap_save(nc_imm_inf_map_S3, 
          filename = "sensitivity analysis/maps/S3/imm_inf_delta_start_S3.png", 
          width = 1000,
          dpi = 140)

# S4

nc_imm_inf_map_S4 <- 
  tm_shape(S4_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S4

tmap_save(nc_imm_overall_map_S4, 
          filename = "sensitivity analysis/maps/S4/imm_inf_delta_start_S4.png", 
          width = 1000,
          dpi = 140)

# S5

nc_imm_inf_map_S5 <- 
  tm_shape(S5_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S5

tmap_save(nc_imm_inf_map_S5, 
          filename = "sensitivity analysis/maps/S5/imm_inf_delta_start_S5.png", 
          width = 1000,
          dpi = 140)

# S6
nc_imm_inf_map_S6 <- 
  tm_shape(S6_MAP_DAT) +                   ## The R object
  tm_polygons(s[2],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_inf_map_S6

tmap_save(nc_imm_inf_map_S6, 
          filename = "sensitivity analysis/maps/S6/imm_inf_delta_start_S6.png", 
          width = 1000,
          dpi = 140)
###'
###'
###'
###' Plot immunity via vaccination
###' 
###' 
###

nc_imm_vacc_map_S1 <- 
  tm_shape(S1_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S1

tmap_save(nc_imm_vacc_map_S1, 
          filename = "sensitivity analysis/maps/S1/imm_vac_delta_start_S1.png", 
          width = 1000,
          dpi = 140)

# S2
nc_imm_vacc_map_S2 <- 
  tm_shape(S2_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S2

tmap_save(nc_imm_vacc_map_S2, 
          filename = "sensitivity analysis/maps/S2/imm_vac_delta_start_S2.png", 
          width = 1000,
          dpi = 140)

# S2
nc_imm_vacc_map_S3 <- 
  tm_shape(S3_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S3

tmap_save(nc_imm_vacc_map_S3, 
          filename = "sensitivity analysis/maps/S3/imm_vac_delta_start_S3.png", 
          width = 1000,
          dpi = 140)

# S4

nc_imm_vacc_map_S4 <- 
  tm_shape(S4_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S4

tmap_save(nc_imm_overall_map_S4, 
          filename = "sensitivity analysis/maps/S4/imm_vac_delta_start_S4.png", 
          width = 1000,
          dpi = 140)

# S5

nc_imm_vacc_map_S5 <- 
  tm_shape(S5_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S5

tmap_save(nc_imm_vacc_map_S5, 
          filename = "sensitivity analysis/maps/S5/imm_vac_delta_start_S5.png", 
          width = 1000,
          dpi = 140)

# S6
nc_imm_vacc_map_S6 <- 
  tm_shape(S6_MAP_DAT) +                   ## The R object
  tm_polygons(s[3],                      ## Column with the data
              title = "",  ## Legend title 
              #              style = "pretty",
              breaks = norm_breaks,
              palette = norm_colors,          ## Color ramp for the polygon fills
              alpha = 1,                   ## Transparency for the polygon fills
              border.col = "black",        ## Color for the polygon lines
              border.alpha = 0.75,          ## Transparency for the polygon lines
              lwd = 0.6,
              legend.show = TRUE) +
  tm_shape(nc_st_poly) +
  tm_borders(col = "black", lwd = 1, alpha = 0.85) +
  tm_layout(inner.margins = rep(0.015, 4),
            outer.margins = c(0.03,0,0.01,0),
            frame = FALSE)
nc_imm_vacc_map_S6

tmap_save(nc_imm_vacc_map_S6, 
          filename = "sensitivity analysis/maps/S6/imm_vac_delta_start_S6.png", 
          width = 1000,
          dpi = 140)


