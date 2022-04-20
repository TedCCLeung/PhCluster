#' Function to
#'
#' @importFrom magrittr %>%
#'
#' @param aggcluster_out output object from AggExCluster
#' @param original_matrix matrix
#' @param identifiers identifiers
#' @param deepsplit_degree 1, 2, 3 or 4
#'
#' @return list
#' @export

process_aggcluster_dynamic <- function(
  aggcluster_out,
  original_matrix,
  identifiers,
  deepsplit_degree = 1
){

  ## GET DENDROGRAM --------------------

  ## The apcluster package function sometimes orders the merging height at the wrong order
  ## This is due to rounding error in large datasets
  ## It must be fixed before proceeding
  aggcluster_out@height <- sort(aggcluster_out@height, decreasing = TRUE)

  ## Make a dendrogram
  ## This is done in redundant conversations to make sure it is stable
  dend <- aggcluster_out %>% apcluster::as.hclust() %>% stats::as.dendrogram() %>%
    stats::as.hclust() %>% stats::as.dendrogram()

  ## DYNAMIC TREE CUT --------------------

  ## Get the exemplars of the original clusters from apcluster
  raw_exemplars <- identifiers[aggcluster_out@exemplars[[length(aggcluster_out@exemplars)]]]
  exemplar_exprs <- original_matrix[aggcluster_out@exemplars[[length(aggcluster_out@exemplars)]],]
  distance_matrix <- 1 - apcluster::corSimMat(exemplar_exprs)

  dynamic_out <- dynamicTreeCut::cutreeHybrid(
    dendro = dend %>% stats::as.hclust(),
    distM = distance_matrix,
    minClusterSize = 0,
    deepSplit = deepsplit_degree,
    verbose = 0,
    pamStage = FALSE
  )

  dynamic_assignment <- dynamic_out$labels
  names(dynamic_assignment) <- NULL
  unassigned <- which(dynamic_assignment == 0)
  extra_cluster_numbers <- (max(dynamic_assignment)+1):(max(dynamic_assignment)+length(unassigned))
  dynamic_assignment[unassigned] <- extra_cluster_numbers

  dynamic_merge <- dynamic_assignment[stats::order.dendrogram(dend)]; names(dynamic_merge) <- base::labels(dend)

  ## RETRIEVE DYNAMIC CLUSTERS --------------------

  clusters <- lapply(1:max(dynamic_merge), function(x){names(dynamic_merge)[dynamic_merge == x]})
  cluster_merge_heights <- lapply(clusters, function(y){dend_get_merge_height(dendrogram = dend, dend_labels = y)})

  ## Helper function that returns the actual genes in each original cluster from affinity propagation
  cluster_to_genes <- function(x){
    no <- x %>% strsplit(split = " ") %>% magrittr::extract2(1) %>% magrittr::extract2(2) %>% as.numeric()
    return(identifiers[aggcluster_out@clusters[[length(aggcluster_out@clusters)]][[no]]])
  }
  dynamic_clusters <- lapply(clusters, function(x){lapply(x, cluster_to_genes) %>% unlist()})

  dynamic_labels <- rep(1:length(dynamic_clusters), times = lengths(dynamic_clusters))
  names(dynamic_labels) <- unlist(dynamic_clusters)

  ## GET EXEMPLAR FOR EACH CLUSTER --------------------

  merged_exemplars <- list()

  for (n in 1:length(clusters)){

    ## For each cluster, get the height at which the subtree is merged
    ## And find the number of clusters at that height
    n_exemplars_at_merge_height <- dendextend::cutree(dend, h = cluster_merge_heights[[n]]) %>% max()

    ## Using the number of clusters, re-cut the dendrogram to find the exemplars at that height
    recut_tree <- apcluster::cutree(aggcluster_out, k = n_exemplars_at_merge_height)
    exemplars_at_height <- identifiers[recut_tree@exemplars]

    ## Intersect between the exemplars of that height and genes in that cluster
    exemplar_of_cluster <- intersect(exemplars_at_height, clusters[[n]] %>% lapply(cluster_to_genes) %>% unlist())
    merged_exemplars[[length(merged_exemplars)+1]] <- exemplar_of_cluster
  }
  dynamic_exemplars <- unlist(merged_exemplars, recursive = FALSE)


  ## MAKE THE FINAL DENDROGRAM --------------------
  tree <- apcluster::as.dendrogram(aggcluster_out, base = 0.1)
  tree <- dendextend::`order.dendrogram<-`(object = tree, value = stats::order.dendrogram(tree) %>% as.integer())
  tree <- dendextend::`labels<-`(object = tree, value = identifiers[stats::order.dendrogram(tree)])

  new_tree <- rotate_dend_by_rDEI(
    dendrogram = tree,
    rDEI_summary = rDEI_summary,
    clusters = dynamic_clusters
  )

  ## Order the clusters by the topology of the tree
  new_cluster_labels <- rep(1:length(dynamic_clusters), times = lengths(dynamic_clusters))
  names(new_cluster_labels) <- dynamic_clusters %>% unlist()

  reordered_labels <- new_cluster_labels[new_tree %>% labels()]
  reordered_clusters <- lapply(unique(reordered_labels), function(x){names(reordered_labels)[reordered_labels==x]})

  ## Get the ordered examplars
  ordered_exemplars <- lapply(reordered_clusters, function(x){dynamic_exemplars[dynamic_exemplars %in% x]})

  exemplar_result <- lengths(reordered_clusters)
  names(exemplar_result) <- ordered_exemplars


  ## OUTPUT --------------------
  return(list(dendrogram = new_tree, labels = reordered_labels, clusters = reordered_clusters, exemplars = exemplar_result))
}
