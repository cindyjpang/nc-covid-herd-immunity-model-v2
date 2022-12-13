## ---------------------------
##
## Script name: SIR Model Initial Conditions as Time Series 
##
## Purpose of script: plot SIR observational data, these conditions specify initial condition
##
## Author: Cindy Pang
##
## Date Created: 2021-08-25
##
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes: majority of this script is used as a sanity check, initial conditions that will be used for eventual projections two weeks into 
##   
##
## ---------------------------

# import libraries 
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)

# import files 
immunity_est <- read_excel("exported data/immunity_est.xlsx")
immunity_est$infection_count <- rowMeans(immunity_est[, c('cdc_multiplier', 'death_est_total_inf')], na.rm = TRUE)



# CDC_multiplier = CDC Infection Count 
# dead_est_total_inf = Death Infection Count
# immunity_est <- immunity_est %>%
#   mutate()
# plotting infection counts as a sanity check
# df <- immunity_est %>%
#   filter(COUNTY %in% unique(immunity_est$COUNTY))
# 
# p <- ggplot(df, aes(x = DATE, y = infection_count)) +
#   geom_bar(
#     aes(color = COUNTY, fill = COUNTY),
#     stat = "identity", position = position_dodge(0.8),
#     width = 0.7
#   )
# 
# p

sir_model <- immunity_est[, c('COUNTY', 'DATE', 'Population','immunity_mean', 'infection_count')]
sir_model <- mutate(
  sir_model,
  N = Population,
  I = infection_count,
  R = Population*(immunity_mean/100) - I,
  S = N - I - R
)

# use obs 
##############################################################
#                                                            #
#               SENSITIVITY SCENARIOS                        #
# {cdc_case_vacc_obs_imm, cdc_vacc_obs_imm_up, cdc_vacc_obs_imm_lower}x {cdc_multiplier}
# {death_inf_vacc_obs_imm, death_inf_vacc_obs_imm_up, death_inf_obs_imm_lower} x {death_est_total_inf}
# {}
##############################################################




sir_initCond_cdc_case_vacc_obs_imm <- immunity_est[, c('COUNTY', 'DATE', 'Population','cdc_multiplier', 'cdc_case_vacc_obs_imm')]
sir_initCond_cdc_case_vacc_obs_imm <- mutate(
  sir_initCond_cdc_case_vacc_obs_imm,
  N = Population,
  I = cdc_multiplier,
  R = Population*(cdc_case_vacc_obs_imm/100) - I,
  S = N - I - R
)


sir_initCond_cdc_case_vacc_obs_imm_up <- immunity_est[, c('COUNTY', 'DATE', 'Population','cdc_multiplier', 'cdc_case_vacc_obs_imm_up')]
sir_initCond_cdc_case_vacc_obs_imm_up <- mutate(
  sir_initCond_cdc_case_vacc_obs_imm_up,
  N = Population,
  I = cdc_multiplier,
  R = Population*(cdc_case_vacc_obs_imm_up/100) - I,
  S = N - I - R
)

sir_initCond_cdc_case_vacc_obs_imm_lower <- immunity_est[, c('COUNTY', 'DATE', 'Population','cdc_multiplier', 'cdc_case_vacc_obs_imm_lower')]
sir_initCond_cdc_case_vacc_obs_imm_lower <- mutate(
  sir_initCond_cdc_case_vacc_obs_imm_lower,
  N = Population,
  I = cdc_multiplier,
  R = Population*(cdc_case_vacc_obs_imm_lower/100) - I,
  S = N - I - R
)

# death_inf scenarios
sir_initCond_death_inf_vacc_obs_imm <- immunity_est[, c('COUNTY', 'DATE', 'Population','death_est_total_inf', 'death_inf_vacc_obs_imm')]
sir_initCond_death_inf_vacc_obs_imm  <- mutate(
  sir_initCond_death_inf_vacc_obs_imm ,
  N = Population,
  I = death_est_total_inf,
  R = Population*(death_inf_vacc_obs_imm/100) - I,
  S = N - I - R
)

sir_initCond_death_inf_vacc_obs_imm_up <- immunity_est[, c('COUNTY', 'DATE', 'Population','death_est_total_inf', 'death_inf_vacc_obs_imm_up')]
sir_initCond_death_inf_vacc_obs_imm_up  <- mutate(
  sir_initCond_death_inf_vacc_obs_imm_up ,
  N = Population,
  I = death_est_total_inf,
  R = Population*(death_inf_vacc_obs_imm_up/100) - I,
  S = N - I - R
)

sir_initCond_death_inf_vacc_obs_imm_lower <- immunity_est[, c('COUNTY', 'DATE', 'Population','death_est_total_inf', 'death_inf_vacc_obs_imm_lower')]
sir_initCond_death_inf_vacc_obs_imm_lower  <- mutate(
  sir_initCond_death_inf_vacc_obs_imm_lower ,
  N = Population,
  I = death_est_total_inf,
  R = Population*(death_inf_vacc_obs_imm_lower/100) - I,
  S = N - I - R
)



# write csv for all 6 scenarios 
write.csv(sir_initCond_cdc_case_vacc_obs_imm,"C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S1\\sir_initCond_cdc_case_vacc_obs_imm.csv")
write.csv(sir_initCond_cdc_case_vacc_obs_imm_up,"C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S2\\sir_initCond_cdc_case_vacc_obs_imm_up.csv")
write.csv(sir_initCond_cdc_case_vacc_obs_imm_lower,"C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S3\\sir_initCond_cdc_case_vacc_obs_imm_lower.csv")
write.csv(sir_initCond_death_inf_vacc_obs_imm, "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S4\\sir_initCond_death_inf_vacc_obs_imm.csv")
write.csv(sir_initCond_death_inf_vacc_obs_imm_up, "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S5\\sir_initCond_death_inf_vacc_obs_imm_up.csv")
write.csv(sir_initCond_death_inf_vacc_obs_imm_lower, "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\sensitivity analysis\\outputs\\S6\\sir_initCond_death_inf_vacc_obs_imm_lower.csv")





















# infection_method <- c("cdc_multiplier", "death_est_total_inf", "total_reported_cases")
# 
# for(i in infection_method){
#   if(i == "cdc_multiplier"){
#     cdc_immunity_bounds <- c("cdc_case_vacc_obs_imm", "cdc_case_vacc_obs_imm_up", "cdc_case_vacc_obs_imm_lower")
#     for(j in cdc_immunity_bounds){
#       sir_model <- immunity_est[, c("COUNTY", "DATE", "Population", i, j)]
#       N <- sir_model[3] # Population
#       I <- sir_model[4] # i
#       R <- N*(sir_model[5]/100)-I
#       S <- N-I-R
#       # sir_model$S <- S
#       # sir_model$I <- I
#       # sir_model$R <- R
#       # sir_model$N <- N
#       # 
#       # print(sir_model)
#       ## export file
#       # file_name <- paste0("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\exported data\\sensitivity analysis data export\\sir_initCond_", j, ".csv")
#       # write.csv(sir_model, file_name)
#       # print(paste0("File: ", j, " is exported!"))
#     }
#   }else if(i == "death_est_total_inf"){
#     
#   }else{
#     
#   }
#  
# }









# plotting susceptible, recovery curves as a sanity check 
# for(county in unique(sir_model$COUNTY)){
#   county_dat <- filter(sir_model, COUNTY == county)
#   plot(county_dat$DATE, county_dat$S, type = 'l', col = 'green', main = county, xlab = 'Date', ylab = 'Counts', ylim = c(0, county_dat$Population[1]) )
#   lines(county_dat$DATE, county_dat$I, col = 'red', ylim = c(0, county_dat$Population[1]))
#   lines(county_dat$DATE, county_dat$R, col = 'blue', ylim = c(0, county_dat$Population[1]))                     
# }
# sir_model_se <- subset(sir_model, COUNTY != 'Missing')
# write out excel files with SIR Initial Conditions Time-Series 
#write_xlsx(sir_model, 'C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\exported data\\sir_model_ts_initial_conditions.xlsx')