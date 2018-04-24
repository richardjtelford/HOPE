make_summary_plot <- function (x, yaxis = "age", group = NULL, ...) 
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
     taxa <- (x$taxon.list) %>% 
      filter(ecological.group %in% group) %>% 
      arrange(ecological.group)
  } else {
    taxa <- x$taxon.list[!x$taxon.list$ecological.group %in% "LABO", ]
  }
  
  if(any(colnames(taxa) == "alias")){#use alias if any
      taxa <- taxa %>% select(taxon.name = alias, ecological.group)
  }
  
  #force to have only wanted levels in factor
  taxa <- taxa %>% 
    mutate(ecological.group = factor(ecological.group, levels = wanted))  
  
  summary_percent <- counts %>% 
    mutate(y = x$sample.meta[, yaxis]) %>% 
    gather(key = taxon.name, value = count, -y) %>% 
    inner_join(taxa) %>% 
    group_by(y) %>% 
    mutate(percent = count/sum(count) * 100) %>% 
    group_by(y, ecological.group) %>% 
    summarise(sum = sum(percent))
  
  summary_percent %>% ggplot(aes(x = y, y = sum, fill = ecological.group)) +
    geom_col(position = "stack") +
    scale_x_reverse() +
    scale_fill_brewer(type = "qual", palette = "Set1", drop = FALSE) +
    scale_y_continuous(expand = c(0.01, 0)) +
    coord_flip() +
    labs(x = yaxis, y = "%", fill = "Group")
}
