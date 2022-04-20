cluster_with_apcluster <- function(
  matrix,
  sim_quantile = 0.5,
  expression_filter = 0
){

  ## Generate a similarity matrix between the patterns --------------------
  filter_exprs <- function(mat, value = 0){return(mat[rowSums(mat) > value, ])}
  final_matrix <- matrix %>% filter_exprs(expression_filter)

  ## Clustering --------------------
  apcluster_out <- apcluster::apcluster(s = apcluster::corSimMat(), x = final_matrix, q = sim_quantile)

  return(apcluster_out)
}
