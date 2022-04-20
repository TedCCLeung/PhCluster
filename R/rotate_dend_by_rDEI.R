rotate_dend_by_rDEI <- function(
  dendrogram,
  clusters,
  rDEI_summary
){

  ## Get the correct order by mean rDEI
  cluster_order <- clusters %>%
    sapply(function(x){base::mean(rDEI_summary[rDEI_summary$geneID %in% x, "log2_rDEI_SDLD"], na.rm = TRUE)}) %>%
    base::order()

  ## Get the ordered clusters
  ordered_leaves <- clusters[cluster_order] %>% unlist()

  ## Get the rotated dendrogram
  new_dendrogram <- suppressWarnings(dendrogram %>% dendextend::rotate(ordered_leaves))

  return(new_dendrogram)
}


