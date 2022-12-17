##
#### Create histograms for Delta wave COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(ggthemes)
library(ggExtra)

## Assumes working directory is "code_vc" (top level!)

## Read new data
all_scenarios_immunity <- read_xlsx("sensitivity analysis/outputs/all_scenarios_start_immunity.xlsx")

## Replace above 100
all_scenarios_immunity$immunity_all[which(all_scenarios_immunity$immunity_all > 100)] <- 100


## Start looping through sensitivity analysis
for (s in c(paste0("S", 1:6))) {
  
  ## Subset to scenario
  immunity_components <- all_scenarios_immunity %>% filter(scenario == s)

  ## max(all_scenarios_immunity$immunity_all)
  ## min(all_scenarios_immunity$immunity_all)
  norm_breaks <- seq(10, 100, by = 2.5)
  ylims <- c(0,25)

###'
###'
###'
###' Plot immunity via infection
###' 
###' 
###' 

immunity_inf_hist <- ggplot(immunity_components, 
                            aes(x = immunity_by_infection)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = ylims,
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Immunity by Infection (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_inf_hist, 
       filename = paste0("hist_imm_inf_delta_start_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
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

immunity_vac_hist <- ggplot(immunity_components, 
                            aes(x = immunity_by_vaccination)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = ylims,
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Immunity by Vaccination (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_vac_hist, 
       filename = paste0("hist_imm_vacc_delta_start_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
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

immunity_all_hist <- ggplot(immunity_components, 
                            aes(x = immunity_all)) + 
  geom_histogram(fill = "gray", 
                 color = "gray50",
                 breaks = norm_breaks) +
  scale_y_continuous(limits = ylims,
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  xlab("Overall Immunity (%)") +
  ylab("Count") +
  theme_pander() +
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))
# immunity_inf_hist

ggsave(plot = immunity_all_hist, 
       filename = paste0("hist_imm_all_delta_start_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
       device = "png",
       width = 1200,
       height = 900,
       units = "px")


###'
###'
###'
###' Scatterplot with histograms
###' 
###' 
###' 

mult_value <- 0.055
text_size <- 3.5

imm <-  
  ggplot(data = immunity_components,
         aes(x = immunity_by_infection,
             y = immunity_by_vaccination)) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = mult_value)) +
  scale_x_continuous(expand = expansion(mult = mult_value)) +
  xlab("Immunity via Infection (%)") +
  ylab("Immunity via Vaccination (%)") +
  # geom_text(x = 210,
  #           y = 8,
  #           hjust = 0,
  #           label = paste0("R = ", round(inf_peak_tot_cor$estimate, 2)),
  #           size = text_size) +
  # geom_text(x = 210,
  #           y = 6.25,
  #           hjust = 0,
  #           label = "p < 0.01",
  #           size = text_size) +
  theme_bw() +  
  theme(axis.text.y = element_text(margin = margin(r = 2)),
        plot.margin = margin(10, 4, 7, 4))

imm_h <- ggMarginal(imm, 
                    type = "histogram",
                    fill = "grey",
                    color = "grey50")
ggsave(plot = imm, 
       filename = paste0("scatter_imminf_immvacc_delta_", s, ".png"), 
       path = paste0("sensitivity analysis/maps/", s), 
       device = "png",
       width = 1300,
       height = 900,
       units = "px")

}

  