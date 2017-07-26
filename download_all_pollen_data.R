## ---- download

if(!file.exists("allPollenData.RData")){ #SLOW
  allPollenSites <- get_dataset(datasettype = "pollen")
  allPollenIDs <- sapply(allPollenSites, FUN = function(x) x$dataset$dataset.id)
  allPollenData <- get_download(allPollenIDs)
  save(allPollenSites, allPollenData, file = "allPollenData.RData")
}

