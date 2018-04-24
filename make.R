#load packages
library("tidyverse")
library("neotoma")

#render wrapper
produce_regional_pollen_diagrams <- function(region, ..., open_pdf = FALSE){
  rmarkdown::render(
    input = "pollen_diagrams.Rmd", 
    params = list(region = region), 
    ...
  )
  if(isTRUE(open_pdf)){
    system("evince pollen_diagrams.pdf")
  }
}

scandinavia <-  data_frame(name = "Scandinavia", lonW = 4, latS = 55, lonE = 37, latN = 81)
produce_regional_pollen_diagrams(scandinavia, open_pdf = TRUE)
