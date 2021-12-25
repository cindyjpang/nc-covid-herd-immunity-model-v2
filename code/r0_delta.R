## r0 analysis 
setwd("C:\\Users\\cindy\\nc-covid-herd-immunity-model-v2")

# libraries lets go!!!
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tmap)
library(sf)
library(RColorBrewer)
library(rsconnect)
library(vtable)
# import files
immunity_est <- read_excel("exported data/immunity_est.xlsx")
delta_proj <- read_excel("exported data/delta_proj.xlsx")
delta_summary <- read_excel("exported data/delta_county_summary.xlsx")
county_pop <- read_excel("data/census2020_county_pop.xlsx")

# housekeeping data
delta_summary$beta.hat <- as.numeric(delta_summary$beta.hat)
delta_summary$gamma.hat <- as.numeric(delta_summary$gamma.hat)
delta_summary$r0.hat <- as.numeric(delta_summary$r0.hat)
delta_summary$recovery_time <- (1/gamma.hat)*7
delta_summary$Vc <- 1-(1/delta_summary$r0.hat)
beta.hat <- delta_summary$beta.hat
gamma.hat <- delta_summary$gamma.hat
r0.hat <- delta_summary$r0.hat

# histograms for R0 params
par(mfrow = c(4,1))
hist(beta.hat, xlab = expression(beta), main = "")
hist(delta_summary$gamma.hat, xlab = expression(gamma), main = "")
hist(delta_summary$recovery_time, xlab = "Recovery Time (days)", main = "")
hist(delta_summary$r0.hat, xlab = "R0", main = "")

# boxplots
par(mfrow = c(4,1))
boxplot(beta.hat, horizontal = TRUE, xlab = expression(beta))
boxplot(gamma.hat, horizontal = TRUE, xlab = expression(gamma))
boxplot(delta_summary$recovery_time, horizontal = TRUE, xlab = "Recovery Time (days)")
boxplot(r0.hat, horizontal = TRUE, xlab = "R0")

# summary tables 
sumtable(delta_summary, vars = c('beta.hat', 'gamma.hat', 'r0.hat', 'recovery_time'), labels = c("Beta","Gamma","R0","Recovery Times (days)" ))

# immunity level hit critical vaccination threshold?
immunity_vc <- merge(delta_summary[, c("COUNTY", "start_date", "r0.hat", "Vc")],
                     immunity_est[, c("COUNTY", "DATE", "immunity_mean")],
                     by.x = c("COUNTY", "start_date"),
                     by.y = c("COUNTY", "DATE"),
                     all = FALSE)
# immunity levels did not hit critical vaccination threshold for each county --> NC unprepared to deal w/ the delta wave and science shows!
immunity_vc$immunity_mean <- immunity_vc$immunity_mean/100

immunity_vc <- merge(immunity_vc,
                     county_pop[, c("County", "Population")],
                     by.x = c("COUNTY"),
                     by.y = c("County"),
                     all = TRUE)

# how many more people would need to be vaccinated to avoid a second wave?
immunity_vc <- immunity_vc %>%
  mutate(thr_hit = Vc < immunity_mean,
         discrepency = Vc-immunity_mean,
         need_vaccination = discrepency*Population,
         pct_discrepency = discrepency*100,
         vacc_date = as.Date(start_date)-14)

# can we do a facet for nc counties and their projections to show fits for the delta wave?


#