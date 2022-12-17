library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(readxl)
library(writexl)
library(egg)
## read files
# get population counts
immunity_est <- read_xlsx("./exported data/immunity_est.xlsx")%>%
  select(COUNTY, Population)%>%
  group_by(COUNTY)%>%
  summarize(Population = max(Population))%>%
  filter(COUNTY != "Missing")




summary_file <- read_xlsx("./sensitivity analysis/outputs/S4/delta_county_summary_S4.xlsx")
scenario_immunity <- read_xlsx("./sensitivity analysis/outputs/all_scenarios_start_immunity.xlsx")

if (nrow(summary_file) > 100) {
  
  # Get middle date for each
  peaks<- summary_file %>% group_by(COUNTY) %>%
    summarize(peak_date = median(peak_date))
  
  summary <- merge(peaks, 
                   summary_file, 
                   by.x = c("COUNTY", "peak_date"),
                   by.y = c("COUNTY", "peak_date"), 
                   all = FALSE)
}
obs_dat <- read_xlsx("./sensitivity analysis/outputs/S4/delta_obs_dat_S4.xlsx")
pop_infected <- obs_dat %>%
  group_by(COUNTY)%>%
  summarize(delta_total_infected_count = sum(I))%>%
  merge(immunity_est, 
        by = "COUNTY", 
        all = FALSE)%>%
  mutate(pct_total_infected = (delta_total_infected_count/Population)*100)

plot_relationships <- merge(summary[, c("COUNTY", "peak_date", "I_proj_peak", "start_date")],
                            pop_infected[, c("COUNTY","pct_total_infected", "Population")], 
                            by = "COUNTY", 
                            all = TRUE)%>%
  mutate(peak_inf_per_10K = (I_proj_peak/Population)*10000)%>%
  merge(filter(scenario_immunity[, c("COUNTY", "immunity_all", "immunity_by_infection", "immunity_by_vaccination", "scenario")], scenario == "S4"), 
        by = "COUNTY", 
        all = TRUE)
  

