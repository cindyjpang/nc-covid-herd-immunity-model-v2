## r0 analysis 
setwd("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2")

# libraries lets go!!!
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(vtable)
library(writexl)

# import files
immunity_est <- read_excel("exported data/immunity_est.xlsx")
delta_proj <- read_excel("exported data/delta_proj.xlsx")
delta_summary <- read_excel("exported data/delta_county_summary.xlsx")
county_pop <- read_excel("data/census2020_county_pop.xlsx")

# housekeeping data
delta_summary$beta.hat <- as.numeric(delta_summary$beta.hat)
delta_summary$gamma.hat <- as.numeric(delta_summary$gamma.hat)
delta_summary$r0.hat <- as.numeric(delta_summary$r0.hat)
delta_summary$recovery_time <- (1/delta_summary$gamma.hat)*7
delta_summary$Vc <- 1-(1/delta_summary$r0.hat)
beta.hat <- delta_summary$beta.hat
gamma.hat <- delta_summary$gamma.hat
r0.hat <- delta_summary$r0.hat

# histograms for R0 params
# par(mfrow = c(4,1))
# hist(beta.hat, xlab = expression(beta), main = "")
# hist(delta_summary$gamma.hat, xlab = expression(gamma), main = "")
# hist(delta_summary$recovery_time, xlab = "Recovery Time (days)", main = "")
# hist(delta_summary$r0.hat, xlab = "R0", main = "")
# 
# # boxplots
# par(mfrow = c(4,1))
# boxplot(beta.hat, horizontal = TRUE, xlab = expression(beta))
# boxplot(gamma.hat, horizontal = TRUE, xlab = expression(gamma))
# boxplot(delta_summary$recovery_time, horizontal = TRUE, xlab = "Recovery Time (days)")
# boxplot(r0.hat, horizontal = TRUE, xlab = "R0")

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

# can we do a facet for nc counties and their projections to show fits for the delta wave? YES!
delta_proj$COUNTY <- as.factor(delta_proj$COUNTY)
delta_proj <- delta_proj %>%
  mutate(N = S+I+R)
p <- ggplot(data = delta_proj, aes(week))+
  geom_line(aes(y=infections/N, color="Observed Infections"))+
  geom_line(aes(y=I/N, color="Fitted Infections SSE")) +
  scale_color_manual("",
                     breaks = c("Observed Infections", "Fitted Infections SSE"),
                     values = c("Observed Infections" = "black", "Fitted Infections SSE" = "red"))+
  xlab("Week")+
  ylab("Case Rate (Infections per Person)")+
  ggtitle("Fitted Infections SSE against Observed Infections for NC Counties (N=100) during B.1.617.2 Wave")+
  facet_wrap(~COUNTY)
ggsave(plot=p, filename = "B.1.617.2_R0_Fit.png", path = "C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\images", device = "png")

# write out file for immunity and vaccination 
write_xlsx(immunity_vc, 'C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\exported data\\immunity_vc.xlsx')