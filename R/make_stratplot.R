make_stratplot <- function (x, yaxis = "age", method = "none", group = NULL, maxtaxa = 50, ...) 
{
  counts <- counts(x)
  if (!yaxis %in% c("depth", "age.older", "age.younger", "age")) {
    stop("You must provide a suitable variable name for the y axis.")
  }
  if (!"sample.meta" %in% names(x) | all(is.na(x$sample.meta[, 
                                                             yaxis]))) {
    stop("This download has no suitable chronological or depth information.")
  }
  if (!is.null(group)) {
    x$taxon.list <- (x$taxon.list) %>% 
      filter(ecological.group %in% group) %>% 
      arrange(ecological.group)
    
    if(any(colnames(x$taxon.list) == "alias")){#use alias if any
      taxa <- x$taxon.list$alias
    }else {
      taxa <- x$taxon.list$taxon.name
    }
  } else {
    taxa <- x$taxon.list$taxon.name[!x$taxon.list$ecological.group %in% "LABO"]
  }
  y <- x$sample.meta[, yaxis]

  #make names unique
  names(counts) <- make.names(names(counts), unique = TRUE)
  
  counts <- analogue::tran(counts[, make.names(taxa, unique = TRUE)], method = method)
  
  #make names unique
  names(counts) <- make.names(names(counts), unique = TRUE)
  
  #deal with large number of taxa
  nc <- ncol(counts)
  if(nc > maxtaxa){
    need_plots <- ceiling(nc/maxtaxa)
    for(i in 1:need_plots){
      keep <- 1:nc > (i - 1)/need_plots * nc & 1:nc <= i/need_plots * nc
      rioja::strat.plot(counts[, keep], yvar = y, ...) 
      if(i < need_plots){cat("\n\n\\pagebreak\n\n")}
    }
    
  }else{
    rioja::strat.plot(counts, yvar = y, ...)
  }
}
