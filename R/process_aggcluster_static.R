#' Function to
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param aggcluster_out output object from AggExCluster
#' @param identifiers Character. Name of genes
#' @param k number of clusters
#'
#' @return list
#' @export

process_aggcluster_static <- function(
  aggcluster_out,
  identifiers,
  k
){

  ## The apcluster package function sometimes orders the merging height at the wrong order
  ## This is due to rounding error in large datasets
  ## It must be fixed before proceeding
  aggcluster_out@height <- sort(aggcluster_out@height, decreasing = TRUE)

  ## Make a tree
  tree <- apcluster::as.dendrogram(aggcluster_out, base = 0.1)
  tree <- dendextend::`order.dendrogram<-`(object = tree, value = stats::order.dendrogram(tree) %>% as.integer())
  tree <- dendextend::`labels<-`(object = tree, value = identifiers[stats::order.dendrogram(tree)])
  cutree_out <- dendextend::cutree(tree, k = k)
  clusters <- lapply(1:max(cutree_out), function(x){names(cutree_out)[cutree_out == x]})

  ## Rotate the dendrogram
  new_tree <- rotate_dend_by_rDEI(
    dendrogram = tree,
    clusters = clusters,
    rDEI_summary = rDEI_summary
  )
  new_cutree <- dendextend::cutree(new_tree, k = k)
  new_cutree <- new_cutree[base::labels(new_tree)]
  re_numbered_cutree <- new_cutree %>% renumber_vector()
  new_clusters <- lapply(unique(re_numbered_cutree), function(x){names(re_numbered_cutree)[re_numbered_cutree == x]})

  exemplars <- identifiers[aggcluster_out@exemplars[[k]]]

  ordered_exemplars <- lapply(new_clusters, function(x){exemplars[exemplars %in% x]})

  exemplar_result <- lengths(new_clusters)
  names(exemplar_result) <- ordered_exemplars

  return(list(dendrogram = new_tree, labels = re_numbered_cutree, clusters = new_clusters, exemplars = exemplar_result))
}
