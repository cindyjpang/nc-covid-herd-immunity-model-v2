##
#### Create violin plots for COVID-19 paper
##
#### Built from Cindy's code, map_county_delta.R
##

library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(ggpubr)
library(ggthemes)

## Assumes working directory is "code_vc" (top level!)

## Read data
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
dat <- read_excel("exported data/delta_county_summary.xlsx")
immunity_est <- read_excel("exported data/immunity_est.xlsx")
immunity_dat <- read_excel("exported data/immunity_vc.xlsx")


## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'
immunity_dat$COUNTY <- toupper(immunity_dat$COUNTY)
names(immunity_dat)[1] <- 'CO_NAME'
immunity_est$COUNTY <- toupper(immunity_est$COUNTY)
names(immunity_est)[1] <- 'CO_NAME'
shp$CO_NAME <- toupper(shp$NAME)
## merge
nc_dat <- merge(shp,
                dat,
                by = 'CO_NAME',
                all = TRUE) %>% st_drop_geometry()

## Create new object
parms <- tibble(group = rep(c("Beta", "Gamma", "R0"), each = 100),
                value = c(nc_dat$beta.hat,
                          nc_dat$gamma.hat,
                          nc_dat$r0.hat) %>% as.numeric())



###'
###' Plot violins
###' 

## Create fill colors
r0_fill <- brewer.pal(5, "BuPu")[4]
beta_fill <- brewer.pal(8, "Dark2")[2]
gamma_fill <- brewer.pal(8, "Dark2")[7]

r0_violin <- 
  ggplot(parms,
         aes(x = group,
             y = value,
             fill = group)) +
  geom_violin(trim = FALSE,
              width = 1.15,
              alpha = 0.7) +
  scale_fill_manual(values = c(beta_fill, gamma_fill, r0_fill)) +
  geom_boxplot(width = 0.12,
               fill = "white") +
  xlab("") + 
  ylab("Value") +
  scale_x_discrete(labels = c(expression(beta), expression(gamma), expression('R'[0])),
                   expand = c(0,0)) +
  theme_pander() +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.text.y = element_text(margin = margin(r = 2)),
#        axis.title.x = element_text(margin = margin(t = 4)),
        axis.text.x = element_text(margin = margin(t = 2.5)),
        axis.line.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.18, "cm"),
        axis.ticks.x = element_line(color = "black", size = 0.6),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(4, 8, -6, 6))

ggsave(plot = r0_violin, 
       filename = "violin_SIR_parms_delta.png", 
       path = "images", 
       device = "png",
       width = 2000,
       height = 900,
       units = "px")
