## ---------------------------
##
## Script name: Sensitivity Analysis  
##
## Purpose of script:
##
## Author: Cindy J. Pang
##
## Date Created: 2022-12-07
##
## Copyright (c) Cindy Pang, 2022
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(lubridate)

sensitivity_dat <- readxl::read_xlsx("./exported data/immunity_est.xlsx")%>%
  mutate(joint_rep_case_t_pct = joint_rep_case_t*100/Population,
         joint_rep_case_obs_pct = joint_rep_case_obs*100/Population, 
         joint_cdc_case_t_pct = joint_cdc_case_t*100/Population, 
         joint_cdc_case_obs_pct = joint_cdc_case_obs*100/Population, 
         joint_death_inf_t_pct = joint_death_inf_t*100/Population, 
         joint_death_inf_obs_pct = joint_death_inf_obs*100/Population)
delta_summary <- readxl::read_xlsx("./exported data/delta_county_summary.xlsx")

#################################################
# CONSTRUCT TIME SERIES                         # 
#################################################

sensitivity_ts <- sensitivity_dat[, c("DATE","rep_case_vacc_t_imm",      
                                       "rep_case_vacc_obs_imm" ,    "cdc_case_vacc_t_imm",       "cdc_case_vacc_obs_imm",     "death_inf_vacc_t_imm",     
                                      "death_inf_vacc_obs_imm" ,   "immunity_mean" )] %>%
  arrange(DATE)


##### GET STATISTICS FOR ERROR BOUNDS per METHOD ############

# Mean
sensitivity_ts_summary_stats_mean <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(across(everything(), mean))%>%
  gather(key = "method", value = "mean", rep_case_vacc_t_imm, rep_case_vacc_obs_imm, cdc_case_vacc_t_imm, cdc_case_vacc_obs_imm, death_inf_vacc_t_imm, death_inf_vacc_obs_imm, immunity_mean)

# Std
sensitivity_ts_summary_stats_sd <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(across(everything(), sd))%>%
  gather(key = "method", value = "sd", rep_case_vacc_t_imm, rep_case_vacc_obs_imm, cdc_case_vacc_t_imm, cdc_case_vacc_obs_imm, death_inf_vacc_t_imm, death_inf_vacc_obs_imm, immunity_mean)

# obs count
sensitivity_ts_summary_stats_N <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(N = n())


### SCALE JOINT PROBABILITY PCT ####

joint_summary <- sensitivity_dat[, c("DATE", "joint_rep_case_t_pct", "joint_rep_case_obs_pct", "joint_cdc_case_t_pct", "joint_cdc_case_obs_pct", "joint_death_inf_t_pct", "joint_death_inf_obs_pct")]

# Mean 
joint_ts_mean <- joint_summary %>%
  group_by(DATE)%>%
  summarize(across(everything(), mean))%>%
  gather(key = "method", value = "mean", joint_rep_case_t_pct, joint_rep_case_obs_pct, joint_cdc_case_t_pct, joint_cdc_case_obs_pct, joint_death_inf_t_pct, joint_death_inf_obs_pct)
joint_ts_sd <- joint_summary %>%
  group_by(DATE)%>%
  summarize(across(everything(), sd))%>%
  gather(key = "method", value = "sd", joint_rep_case_t_pct, joint_rep_case_obs_pct, joint_cdc_case_t_pct, joint_cdc_case_obs_pct, joint_death_inf_t_pct, joint_death_inf_obs_pct)

joint_ts_N <- joint_summary %>%
  group_by(DATE)%>%
  summarize(N = n())

###### MERGE STATISTICS INTO ONE TABLE #################
# Construct 95% CI by mean +/- 1.96*sd/sqrt(N)         #   
########################################################

# INDEPENDENT
sensitivity_ts_summary <- merge(sensitivity_ts_summary_stats_mean, 
                                sensitivity_ts_summary_stats_sd, 
                                by = c("DATE", "method"),
                                all = TRUE)%>%
  merge(sensitivity_ts_summary_stats_N, 
        by = "DATE", 
        all = TRUE)%>%
  mutate(ME = ifelse(sd>0, 1.96*sd/sqrt(N), 0),
         upper.bound = mean + ME, 
         lower.bound = mean - ME)


# JOINT CHECK - INDEP. Assumption 
joint_ts_summary <- merge(joint_ts_mean, 
                                joint_ts_sd, 
                                by = c("DATE", "method"),
                                all = TRUE)%>%
  merge(sensitivity_ts_summary_stats_N, 
        by = "DATE", 
        all = TRUE)%>%
  mutate(ME = ifelse(sd>0, 1.96*sd/sqrt(N), 0),
         upper.bound = mean + ME, 
         lower.bound = mean - ME)





#####################################################################################################################
### ALSO ANOTHER POINT OF ANALYSIS - the multiplier percentage that our estimates are above the reported estimates ## 
#####################################################################################################################
sensitivity_ts_long <- sensitivity_ts_summary[, c("DATE", "method", "mean")]%>%
  spread(key = "method", value = "mean")%>%
  mutate(reported_case_immunity = (rep_case_vacc_obs_imm+rep_case_vacc_t_imm)/2, 
         cdc_reported_ratio = ifelse(reported_case_immunity > 0, (cdc_case_vacc_obs_imm + cdc_case_vacc_t_imm)/(2*reported_case_immunity), NA), 
         death_inf_reported_ratio = ifelse(reported_case_immunity >0 , (death_inf_vacc_obs_imm+death_inf_vacc_t_imm)/(2*reported_case_immunity), NA),
         immunity_mean_reported_ratio = ifelse(reported_case_immunity > 0, immunity_mean/reported_case_immunity, NA))


#################################################
# GENERATE TIME SERIES PLOT                     # 
#################################################



plt_labels <- c("CDC Case Multiplier + Observational Vaccine Estimate",
                "CDC Case Multiplier + Theoretical Vaccine Estimate",
                "Infection by Death backsolve + Observational Vaccine Estimate",
                "Infection by Death backsolve + Theoretical Vaccine Estimate", 
                "Averaged Immunity Estimates",
                "Reported Cases + Observational Vaccine Estimate",
                "Reported Cases + Theoretical Vaccine Estimate")

## SET COLORS 
## ObsVac -> Lighter, 
manual_colors <- c("#fd8d3c", "#e31a1c", "#67a9cf", "#02818a", "#525252", "#f768a1", "#ae017e")



# INDEPENDENT ASSUMPTION
sensitivity_ts_plot <- ggplot(sensitivity_ts_summary, aes(x = DATE, group = method, fill = method))+
  geom_line(aes(y = mean), size = 1)+
  geom_ribbon(aes(ymin = lower.bound, ymax = upper.bound), alpha = 0.5)+
  theme_bw()+
  scale_fill_manual(
    values = manual_colors,
    name   = "Immunity Estimation Method",
    labels = plt_labels
  )+
  xlab("Date")+
  ylab("Mean % Immune \n [95% CI]")


# JOINT INDEPENDENT PCT - BOTH VACC + INFECTED

plt_labels_j <- c("CDC Case Multiplier + Observational Vaccine Estimate",
                "CDC Case Multiplier + Theoretical Vaccine Estimate",
                "Infection by Death backsolve + Observational Vaccine Estimate",
                "Infection by Death backsolve + Theoretical Vaccine Estimate", 
                "Reported Cases + Observational Vaccine Estimate",
                "Reported Cases + Theoretical Vaccine Estimate")

## SET COLORS 
## ObsVac -> Lighter, 
manual_colors_j <- c("#fd8d3c", "#e31a1c", "#67a9cf", "#02818a", "#f768a1", "#ae017e")



joint_ts_plot <- ggplot(joint_ts_summary, aes(x = DATE, group = method, fill = method))+
  geom_line(aes(y = mean), size = 1)+
  geom_ribbon(aes(ymin = lower.bound, ymax = upper.bound), alpha = 0.5)+
  theme_bw()+
  scale_fill_manual(
    values = manual_colors_j,
    name   = "Immunity Estimation Method",
    labels = plt_labels_j
  )+
  xlab("Date")+
  ylab("Mean % Immune and Vaccinated \n [95% CI]")

joint_ts_plot

#### MULTIPLIER OVER REPORTED CASES #####
multiplier_model <- sensitivity_ts_long %>%
  select(DATE, cdc_reported_ratio, death_inf_reported_ratio, immunity_mean_reported_ratio)%>%
  gather(key = "method", value = "value", na.rm = TRUE,cdc_reported_ratio, death_inf_reported_ratio, immunity_mean_reported_ratio)
  

est_ratio_plot <- ggplot(filter(multiplier_model, DATE > as.Date("2020-04-15")), aes(x=DATE, col = method))+
  geom_line(aes(y = value, col = method), size = 1)+
  scale_color_manual(
    values = c("Red", "Blue", "Black"),
    name   = "Immunity Estimation Method",
    labels = c("CDC Case Multiplier", "Infection by Death backsolve", "Immunity Mean (Our Estimate)")
  )+
  theme_bw()+
  xlab("Date")+
  ylab("Immunity Estimate Method to Reported Cases Ratio")


###################################################
# SAVE PLOTS                                      #
###################################################

ggsave(plot = sensitivity_ts_plot, 
       filename = "sensitivity_ts_indep_plot.png", 
       path = "images", 
       device = "png")

ggsave(plot = joint_ts_plot, 
       filename = "sensitivity_joint_ts_plot.png", 
       path = "images", 
       device = "png")
ggsave(plot = est_ratio_plot, 
       filename = "sensitivity_ratio_ts_plot.png", 
       path = "images", 
       device = "png")


