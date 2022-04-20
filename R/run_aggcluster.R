run_aggcluster <- function(
  input_matrix
){

  identifiers <- rownames(input_matrix)
  rownames(input_matrix) <- NULL

  apcluster_res <- cluster_with_apcluster(matrix = input_matrix)
  aggcluster_res <- apcluster::aggExCluster(x = apcluster_res)

  return(list("apcluster" = apcluster_res, "aggcluster" = aggcluster_res, "identifiers" = identifiers))
}

# aggcluster_photoperiodic_genes <- run_aggcluster()
# aggcluster_photoperiodic_genes <- aggcluster_photoepriodic_genes
# usethis::use_data(aggcluster_photoperiodic_genes)
