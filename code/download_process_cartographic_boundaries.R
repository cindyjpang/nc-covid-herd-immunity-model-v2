###
### Code to get cartographic boundary file for NC counties
###    For making nice small maps (for figures)
###

library(sf)
library(tidyverse)
library(magrittr)

## Assumes working directory is "code_vc" (top level!)

### Make directory to store
dir.create("data/cartographic_boundaries")

### Download file
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip",
              destfile = "data/cartographic_boundaries/cb_2018_us_county_5m.zip")

### Unzip
unzip(zipfile = "data/cartographic_boundaries/cb_2018_us_county_5m.zip", 
      exdir = "data/cartographic_boundaries")

### Read file
county <- st_read("data/cartographic_boundaries/cb_2018_us_county_5m.shp")

### Subset to NC
county %<>% filter(STATEFP == 37)

### Reproject
county %<>% st_transform(crs = 32119)

### Write out
st_write(county, "data/cartographic_boundaries/cb_2018_nc_county_5m.shp")
