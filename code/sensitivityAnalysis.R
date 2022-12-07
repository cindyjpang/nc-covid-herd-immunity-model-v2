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

sensitivity_dat <- readxl::read_xlsx("./exported data/immunity_est.xlsx")


#################################################
# CONSTRUCT TIME SERIES + PLOT ERROR BOUNDS     # 
#################################################

sensitivity_ts <- sensitivity_dat[, c("DATE","rep_case_vacc_t_imm",      
                                       "rep_case_vacc_obs_imm" ,    "cdc_case_vacc_t_imm",       "cdc_case_vacc_obs_imm",     "death_inf_vacc_t_imm",     
                                      "death_inf_vacc_obs_imm" ,   "immunity_mean" )] %>%
  arrange(DATE)


##### GET STATISTICS FOR ERROR BOUNDS per METHOD ############

sensitivity_ts_summary_stats_mean <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(across(everything(), mean))%>%
  gather(key = "method", value = "mean", rep_case_vacc_t_imm, rep_case_vacc_obs_imm, cdc_case_vacc_t_imm, cdc_case_vacc_obs_imm, death_inf_vacc_t_imm, death_inf_vacc_obs_imm, immunity_mean)


sensitivity_ts_summary_stats_sd <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(across(everything(), sd))%>%
  gather(key = "method", value = "sd", rep_case_vacc_t_imm, rep_case_vacc_obs_imm, cdc_case_vacc_t_imm, cdc_case_vacc_obs_imm, death_inf_vacc_t_imm, death_inf_vacc_obs_imm, immunity_mean)

sensitivity_ts_summary_stats_N <- sensitivity_ts %>%
  group_by(DATE)%>%
  summarize(N = n())

###### MERGE STATISTICS INTO ONE TABLE #################

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

sensitivity_ts_plot



# sensitivity_ts_summary_stats <- sensitivity_ts %>%
#   group_by(DATE, method)%>%
#   summarise( N = n(), 
#             rep_case_vacc_t_imm_mean = mean(rep_case_vacc_t_imm), 
#             rep_case_vacc_t_imm_sd = sd(rep_case_vacc_t_imm), 
#             cdc_case_vacc_t_imm_mean = mean(cdc_case_vacc_t_imm), 
#             cdc_case_vacc_t_imm_sd = sd(cdc_case_vacc_t_imm) 
#             )
# 
# sensitivity_ts_plot <- ggplot(sensitivity_ts_summary_stats, aes(x = DATE))+
#   geom_line(aes(y = rep_case_vacc_t_imm_mean))+
#   geom_ribbon(aes(ymin = rep_case_vacc_t_imm_mean-rep_case_vacc_t_imm_sd, ymax = rep_case_vacc_t_imm_mean+rep_case_vacc_t_imm_sd), alpha = 0.5)+ 
#   
# 
# sensitivity_ts_plot
