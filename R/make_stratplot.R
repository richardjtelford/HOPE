make_stratplot <- function (x, yaxis = "age", method = "none", group = NULL, ...) 
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
    
    taxa <- x$taxon.list$taxon.name
  
  }
  else {
    taxa <- x$taxon.list$taxon.name[!x$taxon.list$ecological.group %in% 
                                      "LABO"]
  }
  y <- x$sample.meta[, yaxis]
  counts <- analogue::tran(counts[, taxa], method = method)
  rioja::strat.plot(counts, yvar = y, ...)
}
