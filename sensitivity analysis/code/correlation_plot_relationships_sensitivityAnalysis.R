library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(readxl)
library(writexl)
library(egg)
library(lubridate)
## read files
# get population counts
immunity_est <- read_xlsx("./exported data/immunity_est.xlsx")%>%
  select(COUNTY, Population)%>%
  group_by(COUNTY)%>%
  summarize(Population = max(Population))%>%
  filter(COUNTY != "Missing")

scenario_immunity <- read_xlsx("./sensitivity analysis/outputs/all_scenarios_start_immunity.xlsx")


for(s in c(paste0("S", 1:6))){
  read_summary <- paste0("./sensitivity analysis/outputs/", s, "/delta_county_summary_", s, ".xlsx")
  read_obs_dat <- paste0("./sensitivity analysis/outputs/", s, "/delta_obs_dat_", s, ".xlsx")
  
  summary_file <- read_xlsx(read_summary)
  obs_dat <- read_xlsx(read_obs_dat)
  
  
  
  if (nrow(summary_file) > 100) {
    
    # Get middle date for each
    peaks<- summary_file %>% group_by(COUNTY) %>%
      summarize(peak_date = median(peak_date))
    
    summary <- merge(peaks, 
                     summary_file, 
                     by.x = c("COUNTY", "peak_date"),
                     by.y = c("COUNTY", "peak_date"), 
                     all = FALSE)
  }else{
    summary <- summary_file
  }
  
  pop_infected <- obs_dat %>%
    group_by(COUNTY)%>%
    summarize(delta_total_infected_count = sum(I))%>%
    merge(immunity_est, 
          by = "COUNTY", 
          all = FALSE)%>%
    mutate(pct_total_infected = (delta_total_infected_count/Population)*100)
  
  # relationship vectors
  plot_relationships <- merge(summary[, c("COUNTY", "peak_date", "I_proj_peak", "start_date")],
                              pop_infected[, c("COUNTY","pct_total_infected", "Population")], 
                              by = "COUNTY", 
                              all = TRUE)%>%
    mutate(peak_inf_per_10K = (I_proj_peak/Population)*10000)%>%
    merge(filter(scenario_immunity[, c("COUNTY", "immunity_all", "immunity_by_infection", "immunity_by_vaccination", "scenario")], scenario == "S4"), 
          by = "COUNTY", 
          all = TRUE)%>%
    mutate(start_date_proxy = as.numeric(difftime(start_date, as.Date("2021-01-01", units = c("days")))),
           start_date = as.Date(start_date), 
           start_date_snap = floor_date(start_date, "weeks"))
  
  ## GET CORRELATIONS for all relationships
  R_mtx <- matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c("immunity_all", "immunity_by_infection", "immunity_by_vaccination"), c("pct_total_infected", "peak_inf_per_10K", "start_date_proxy")))
  pval_mtx <- matrix(data = NA, nrow = 3, ncol = 3, dimnames = list(c("immunity_all", "immunity_by_infection", "immunity_by_vaccination"), c("pct_total_infected", "peak_inf_per_10K", "start_date_proxy")))
  rows <- c("immunity_all", "immunity_by_infection", "immunity_by_vaccination")
  cols <- c("pct_total_infected", "peak_inf_per_10K", "start_date_proxy")
  
  for(i in rows){
    for(j in cols){
      corTest <- cor.test(plot_relationships[,i ], plot_relationships[,j ])
      R_mtx[i,j]<- round(corTest$estimate, digits = 2)
      pval_mtx[i,j]<- corTest$p.value
    }
  }
  
  ##
  ## Create plots
  ##
  #               Imm Inf    Imm Vacc    Imm Overall
  #  Start Date
  #  Peak Inf R
  #  Delta Inf P
  
  mult_value <- 0.055
  text_size <- 3.5
  axis_text_size <- 2
  
  if(s == "S1" | s == "S2" | s == "S3"){
    st_date_R_y = as.Date("2021-05-30")
    st_date_p_y = as.Date("2021-05-18")
  }else{
    st_date_R_y = as.Date("2021-08-22")
    st_date_p_y = as.Date("2021-08-10")
  }
  inf_stdate <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_infection,
               y = start_date_snap)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_date(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Infection (%)") +
    ylab("Start date") +
    geom_text(x = 40,
              y = st_date_R_y,
              hjust = 0,
              label = paste0("R = ", R_mtx[2,3]),
              size = text_size) +
    geom_text(x = 40,
              y = st_date_p_y,
              hjust = 0,
              label = ifelse(pval_mtx[2,3] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[2,3], digits =2 ))),
              size = text_size) +
    theme_bw()
  
  inf_stdate
  
  vac_stdate <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_vaccination,
               y = start_date_snap)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_date(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Vaccination (%)") +
    ylab("Start date") +
    geom_text(x = 50,
              y = st_date_R_y,
              hjust = 0,
              label = paste0("R = ", R_mtx[3,3]),
              size = text_size) +
    geom_text(x = 50,
              y = st_date_p_y,
              hjust = 0,
              label = ifelse(pval_mtx[3,3] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[3,3], digits =2 ))),
              size = text_size) +
    theme_bw()
  vac_stdate
  ovr_stdate <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_all,
               y = start_date_snap)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_date(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Total Immunity (%)") +
    ylab("Start date") +
    geom_text(x = 30,
              y = st_date_R_y,
              hjust = 0,
              label = paste0("R = ", R_mtx[1,3]),
              size = text_size) +
    geom_text(x = 30,
              y = st_date_p_y,
              hjust = 0,
              label = ifelse(pval_mtx[1,3] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[1,3], digits =2 ))),
              size = text_size) +
    theme_bw() 
  
  
  inf_peakinf <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_infection,
               y = peak_inf_per_10K)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Infection (%)") +
    ylab("Peak infection rate \n (per 10,000)") +
    geom_text(x = 40,
              y = 350,
              hjust = 0,
              label = paste0("R = ", R_mtx[2,2]),
              size = text_size) +
    geom_text(x = 40,
              y = 300,
              hjust = 0,
              label = ifelse(pval_mtx[2,2] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[2,2], digits =2 ))),
              size = text_size) +
    theme_bw()
  inf_peakinf
  
  
  vac_peakinf <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_vaccination,
               y = peak_inf_per_10K)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Vaccination (%)") +
    ylab("Peak infection rate \n (per 10,000)") +
    geom_text(x = 50,
              y = 350,
              hjust = 0,
              label = paste0("R = ", R_mtx[3,2]),
              size = text_size) +
    geom_text(x = 50,
              y = 300,
              hjust = 0,
              label = ifelse(pval_mtx[3,2] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[2,2], digits =2 ))),
              size = text_size) +
    theme_bw()
  
  ovr_peakinf <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_all,
               y = peak_inf_per_10K)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Total Immunity (%)") +
    ylab("Peak infection rate \n (per 10,000)") +
    geom_text(x = 30,
              y = 350,
              hjust = 0,
              label = paste0("R = ", R_mtx[1,2]),
              size = text_size) +
    geom_text(x = 30,
              y = 300,
              hjust = 0,
              label = ifelse(pval_mtx[1,2] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[1,2], digits =2 ))),
              size = text_size)  +
    theme_bw()
  
  
  inf_prop <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_infection,
               y = pct_total_infected)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Infection (%)") +
    ylab("Population infected \n (%)") +
    geom_text(x = 40,
              y = 25,
              hjust = 0,
              label = paste0("R = ", R_mtx[2,1]),
              size = text_size) +
    geom_text(x = 40,
              y = 22,
              hjust = 0,
              label = ifelse(pval_mtx[2,1] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[2,1], digits =2 ))),
              size = text_size) +
    theme_bw()
  inf_prop
  vac_prop <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_by_vaccination,
               y = pct_total_infected)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Immunity by Vaccination (%)") +
    ylab("Population infected \n (%)") +
    geom_text(x = 50,
              y = 25,
              hjust = 0,
              label = paste0("R = ", R_mtx[3,1]),
              size = text_size) +
    geom_text(x = 50,
              y = 22,
              hjust = 0,
              label = ifelse(pval_mtx[3,1] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[3,1], digits =2 ))),
              size = text_size)  +
    theme_bw()
  
  ovr_prop <-  
    ggplot(data = plot_relationships,
           aes(x = immunity_all,
               y = pct_total_infected)) +
    geom_smooth(formula = y ~ x,
                method = "loess",
                size = 0,
                alpha = 0.2) +
    geom_line(stat = "smooth", method = "loess",
              color = "black",
              linetype = "longdash",
              size = 0.5,
              alpha = 0.2) +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = mult_value)) +
    scale_x_continuous(expand = expansion(mult = mult_value)) +
    xlab("Total Immunity (%)") +
    ylab("Population infected \n (%)") +
    geom_text(x = 30,
              y = 25,
              hjust = 0,
              label = paste0("R = ", R_mtx[1,1]),
              size = text_size) +
    geom_text(x = 30,
              y = 22,
              hjust = 0,
              label = ifelse(pval_mtx[1,1] < 0.001, "p < 0.001", paste0("p = ", round(pval_mtx[1,1], digits =2 ))),
              size = text_size) +
    theme_bw()
  
  
  grl1 <- ggarrange(inf_stdate + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    vac_stdate + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank()) + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    ovr_stdate + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank()) + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    inf_peakinf + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    vac_peakinf + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank()) + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    ovr_peakinf + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank()) + 
                      theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.x = element_blank()),
                    inf_prop,
                    vac_prop + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank() ),
                    ovr_prop + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank() ),
                    nrow = 3,
                    ncol = 3)
  
  plot_filename <- paste0("./sensitivity analysis/maps/", s, "/scatterplots_immunity_REV1_", s, ".png")
   ggsave(plot = grl1, 
         filename = plot_filename,
         device = "png",
         width = 2000,
         height = 1300,
         units = "px")
  
  
}




