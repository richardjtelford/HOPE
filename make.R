#load packages
library("tidyverse")
library("neotoma")

#render wrapper
produce_regional_pollen_diagrams <- function(region){
  rmarkdown::render(
    input = "pollen_diagrams.Rmd", 
    params = list(region = region, region_name = region_name)
  )
}

scandinavia <-  data_frame(name = "Scandinavia", lonW = 4, latS = 55, lonE = 37, latN = 81)
produce_regional_pollen_diagrams(scandinavia)
