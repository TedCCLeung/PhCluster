#' Function to get the merge height of a vector of labels in a dendrogram. These are helper functions for dynamic tree cut
#'
#' @importFrom magrittr %>%
#'
#' @param dendrogram Dendrogram object
#' @param dend_labels Labels of the sub-dendrogram
#'
#' @return Numeric.

dend_get_merge_height <- function(
  dendrogram,
  dend_labels
){

  subtree <- get_subdendrogram_from_labels(dendrogram = dendrogram, dend_labels = dend_labels)

  max_height <- subtree %>% dendextend::get_nodes_attr("height") %>% max()

  return(max_height)
}

get_subdendrogram_from_labels <- function(
  dendrogram,
  dend_labels
){

  order_of_labels <- stats::order.dendrogram(dendrogram)
  names(order_of_labels) <- base::labels(dendrogram)

  subtree <- get_subdendrogram(dend = dendrogram, cluster_order = order_of_labels[dend_labels] %>% as.numeric())

  return(subtree)
}


get_subdendrogram <- function(dend, cluster_order){

  if(all(unlist(dend) %in% cluster_order)){
    return(dend)
  }

  if(any(unlist(dend[[1]]) %in% cluster_order)){
    return(get_subdendrogram(dend[[1]], cluster_order))
  } else{
    return(get_subdendrogram(dend[[2]], cluster_order))
  }
}

get_label_exemplar <- function(
  dendrogram,
  dend_labels,
  aggcluster_out
){





}


