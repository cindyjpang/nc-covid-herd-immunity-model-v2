### Analysis: Mapping the Immunity Mean Values over time in Shiny.App
###
### Description: see above, serves as a sanity check 
###
###
### Written by: Cindy Pang

library(shiny)
library(tmap)
library(sf)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
shp <- st_read("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\data\\nc shapefile\\counties.shp")
dat <- read_excel("C:\\Users\\Cindy Pang\\nc-covid-herd-immunity-model-v2\\exported data\\immunity_est.xlsx")


## change and merge data
dat$COUNTY <- toupper(dat$COUNTY)
names(dat)[1] <- 'CO_NAME'
nc_imm_dat <- dat[, c('CO_NAME', 'DATE', 'immunity_mean') ]




# counties <- unique(nc_imm_dat$CO_NAME)
# 
# min_dates <- c()
# for(county in counties){
#     county_filter = filter(nc_imm_dat, CO_NAME == county)
#     min_date <- min(county_filter$WeekDate)
#     min_dates <- append(min_dates, min_date)
# }

nc_dat <- merge(
  shp, 
  nc_imm_dat, 
  by.x = 'CO_NAME', 
  all = TRUE
)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NC County Immunity Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = 'user_val',
                  label = "date",
                  min = min(nc_dat$DATE),
                  max = max(nc_dat$DATE),
                  value = max(nc_dat$DATE), 
                  timeFormat = "%F",
                  timezone = "America/New_York")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tmapOutput("my_tmap")
    )
  )
)

# Define server logic required to draw a map
server <- function(input, output) {
  
  output$my_tmap = renderTmap({
    nc_dat_user = filter(nc_dat, DATE == input$user_val)
    
    tm_shape(nc_dat_user) + tm_polygons("immunity_mean",
                                        style = 'fixed',
                                        breaks=c(0,20,30,40,50,60, 70, 100),
                                        palette = 'BuGn')
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
