##
#### Create histograms for Delta wave COVID-19 paper
##
#### Built from Paul's code, plot_immunity_histograms_delta_start.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(dplyr)


## Start looping through sensitivity analysis, construct one master dataframe w all parameters
immunity_components <- data.frame()
for (s in c(paste0("S", 1:6))) {
  
  dat <- read_excel(paste0("sensitivity analysis/outputs/",
                           s,
                           "/",
                           s,
                           "_start_immunity.xlsx"))%>%
    mutate(scenario = s)
 
  ## get peaks 
  peaks <- read_excel(paste0("sensitivity analysis/outputs/",
                           s,
                           "/delta_county_summary_",
                           s,
                           ".xlsx"))%>%
    select(COUNTY, peak_date, start_date)
  
  ## If many entries for peak date, pick middle?
  if (nrow(peaks) > 100) {
    
    # Get middle date for each
    peaks <- peaks %>% group_by(COUNTY) %>%
      summarize(peak_date = median(peak_date))
  }else{
    peaks <- peaks %>%
      select(-start_date)
  }
  master_dat <- merge(dat, 
                      peaks, 
                      by = "COUNTY",
                      all = TRUE)
  
   immunity_components <- rbind(immunity_components, master_dat)
    
}


### Get some values for plotting
imm_max <- max(immunity_components$immunity_all)
imm_min <- min(c(immunity_components$immunity_by_infection,
                 immunity_components$immunity_by_vaccination))
norm_breaks <- seq(10, 95, by = 2.5)


for(s in c(paste0("S", 1:6))){
  df <- filter(immunity_components, scenario == s)
  
  
  ###'
  ###'
  ###'
  ###' Plot immunity via infection
  ###' 
  ###' 
  ###' 
  
  immunity_inf_hist <- ggplot(df, 
                              aes(x = immunity_by_infection)) + 
    geom_histogram(fill = "gray", 
                   color = "gray50",
                   breaks = norm_breaks) +
    scale_y_continuous(limits = c(0, 21),
                       expand = expansion(add = 0.5)) +
    scale_x_continuous(expand = expansion(add = 1)) +
    xlab("Immunity by Infection (%)") +
    ylab("Count") +
    theme_pander() +
    theme(axis.text.y = element_text(margin = margin(r = 2)),
          plot.margin = margin(10, 4, 7, 4))
  # immunity_inf_hist
  
  ggsave(plot = immunity_inf_hist, 
         filename = paste0("sensitivity analysis/plots/",s,"/hist_imm_inf_delta_start_",s,".png"), 
         device = "png",
         width = 1200,
         height = 900,
         units = "px")
  
  ###'
  ###'
  ###'
  ###' Plot immunity via vaccination
  ###' 
  ###' 
  ###' 
  
  immunity_vac_hist <- ggplot(df, 
                              aes(x = immunity_by_vaccination)) + 
    geom_histogram(fill = "gray", 
                   color = "gray50",
                   breaks = norm_breaks) +
    scale_y_continuous(limits = c(0, 21),
                       expand = expansion(add = 0.5)) +
    scale_x_continuous(expand = expansion(add = 1)) +
    xlab("Immunity by Vaccination (%)") +
    ylab("Count") +
    theme_pander() +
    theme(axis.text.y = element_text(margin = margin(r = 2)),
          plot.margin = margin(10, 4, 7, 4))
  # immunity_inf_hist
  
  ggsave(plot = immunity_vac_hist, 
         filename = paste0("sensitivity analysis/plots/",s,"/hist_imm_vacc_delta_start_",s,".png"), 
         device = "png",
         width = 1200,
         height = 900,
         units = "px")
  
  ###'
  ###'
  ###'
  ###' Plot immunity via both
  ###' 
  ###' 
  ###' 
  
  immunity_all_hist <- ggplot(df, 
                              aes(x = immunity_all)) + 
    geom_histogram(fill = "gray", 
                   color = "gray50",
                   breaks = norm_breaks) +
    scale_y_continuous(limits = c(0, 21),
                       expand = expansion(add = 0.5)) +
    scale_x_continuous(expand = expansion(add = 1)) +
    xlab("Overall Immunity (%)") +
    ylab("Count") +
    theme_pander() +
    theme(axis.text.y = element_text(margin = margin(r = 2)),
          plot.margin = margin(10, 4, 7, 4))
  # immunity_inf_hist
  
  ggsave(plot = immunity_all_hist, 
         filename = paste0("sensitivity analysis/plots/",s,"/hist_imm_all_delta_start_",s,".png"), 
         device = "png",
         width = 1200,
         height = 900,
         units = "px")
  ###'
  ###'
  ###'
  ###' Plot immunity via infection vs immunity via vaccination
  ###' 
  ###' 
  ###' 
  ###' 
  text_size <- 5
  
  cor.result <- cor.test(df$immunity_by_infection, df$immunity_by_vaccination)
  pval <- cor.result$p.value
  rval <- round(cor.result$estimate, digits = 2)
  
  
  immunity_compare <- ggplot(immunity_components, aes(x = immunity_by_infection, y = immunity_by_vaccination))+
    geom_point()+
    xlab("Immunity by Infection (%)")+
    ylab("Immunity by Vaccination (%)")+
    theme_pander()+
    geom_text(x = 45, 
              y = 60, 
              hjust = 0, 
              label = paste0("R = ", rval), 
              size = text_size)+
    geom_text(x = 45, 
              y = 57, 
              hjust = 0, 
              label = ifelse(pval < 0.01,"p < 0.01", pval), 
              size = text_size)
  ggsave(plot = immunity_compare, 
         filename = paste0("sensitivity analysis/plots/",s,"/scatterplots_immunity_vaccination_infection_",s,".png"), 
         device = "png",
         width = 1200,
         height = 900,
         units = "px")
  ###'
  ###'
  ###'
  ###' Plot start vs peak date, response to R.
  ###' 
  ###' 
  ###' 
  ###
  ref.date <- as.Date("2021-01-01")
  start.peak.cor.df <- data.frame(start_date = df$DATE, 
                                  peak_date = df$peak_date)%>%
    mutate(ref.start = as.numeric(difftime(start_date, ref.date, units = "days")),
           ref.peak = as.numeric(difftime(peak_date, ref.date, units = "days")))
  cor.start.peak <- cor.test(start.peak.cor.df$ref.start, start.peak.cor.df$ref.peak)
  
  # pval = 0.0002100211
  
  delta_start_peak_cor <- ggplot(df, aes(x = DATE, y = peak_date))+
    geom_point()+
    xlab("Start Date")+
    ylab("Peak Date")+
    theme_pander()+
    geom_text(x = as.Date("2021-05-13"),
              y = as.Date("2021-10-01"),
              hjust = 0,
              label = paste0("R = ", round(cor.start.peak$estimate, digits = 2)),
              size = text_size+3)+
    geom_text(x = as.Date("2021-05-13"),
              y = as.Date("2021-09-28"),
              hjust = 0,
              label = ifelse(round(cor.start.peak$p.value, digits = 2) < 0.01,"p < 0.01", round(cor.start.peak$p.value, digits = 2)),
              size = text_size+3)
  
  ggsave(plot = delta_start_peak_cor, 
         filename = paste0("sensitivity analysis/plots/",s,"/scatterplots_start_peak_",s,".png"), 
         device = "png",
         width = 1200,
         height = 800,
         units = "px")
  
}