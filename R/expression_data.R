#' Retrieve TMM expression data of genes
#'
#' @importFrom rlang .data
#'
#' @param threshold Filter by expression level. (Sum of TMM reads in each of the three photoperiods)
#' @param photoperiodic_only Whether to include only genes that show differential expression between photoperiods
#' @param condition_count 1, 2 or 3, the number of photoperiods that needs to be detected above threshold
#'
#' @return List of matrices
#' @export

expression_data <- function(
  threshold = 1,
  photoperiodic_only = FALSE,
  condition_count = 3
){

  EQ_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 1:6]
  LD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 7:12]
  SD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 13:18]

  EQ_genes <- rownames(EQ_mat)[rowSums(EQ_mat) > threshold]
  LD_genes <- rownames(LD_mat)[rowSums(LD_mat) > threshold]
  SD_genes <- rownames(SD_mat)[rowSums(SD_mat) > threshold]

  df_freq <- as.data.frame(table(c(EQ_genes, LD_genes, SD_genes)))
  common <- as.character(df_freq[df_freq$Freq >= condition_count, "Var1"])

  if (photoperiodic_only){common <- common[common %in% photoperiodic_genes]}

  return(list("EQ" = EQ_mat[common, ], "LD" = LD_mat[common, ], "SD" = SD_mat[common, ]))
}



