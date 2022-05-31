run_master <- function(
  input_matrix = cbind(
    expression_data(0, photoperiodic_only = FALSE)$LD,
    expression_data(0, photoperiodic_only = FALSE)$SD,
    expression_data(0, photoperiodic_only = FALSE)$EQ
  ),
  deepSplit_degree = 3,
  k = 14,
  output_dir
){

  ## Run the apclustering that defines the finest clusters first
  res <- run_aggcluster(input_matrix = input_matrix)
  aggcluster_result <- res$aggcluster
  gene_IDs <- res$identifiers
  expression_matrix <- input_matrix

  run_cluster(
    aggcluster_object = aggcluster_result,
    gene_IDs = gene_IDs,
    expression_matrix = expression_matrix,
    dir = output_dir,
    deepsplit_degree = deepSplit_degree,
    k = k,
    skip_additional_heatmaps = TRUE
  )

}

