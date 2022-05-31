#' Retrieve TMM expression data of genes
#'
#' @importFrom rlang .data
#'
#' @param threshold Filter by expression level. (Sum of TMM reads in each of the three photoperiods)
#' @param photoperiodic_only Whether to include only genes that show differential expression between photoperiods
#' @param single_photoperiod_allowed Whether genes that are only present in one single photoperiod should be included
#'
#' @return List of matrices
#' @export

expression_data <- function(
  threshold = 0,
  photoperiodic_only = TRUE,
  single_photoperiod_allowed = FALSE
){

  EQ_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 1:6]
  LD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 7:12]
  SD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 13:18]

  EQ_mat <- EQ_mat[rowSums(EQ_mat) > threshold, ]
  LD_mat <- LD_mat[rowSums(LD_mat) > threshold, ]
  SD_mat <- SD_mat[rowSums(SD_mat) > threshold, ]

  if (single_photoperiod_allowed){
    common <- unique(c(rownames(EQ_mat), rownames(LD_mat), rownames(SD_mat)))
  } else {
    common <- intersect(rownames(EQ_mat), rownames(LD_mat)) %>% intersect(rownames(SD_mat))
  }

  if (photoperiodic_only){common <- common[common %in% photoperiodic_genes]}

  return(list("EQ" = EQ_mat[common, ], "LD" = LD_mat[common, ], "SD" = SD_mat[common, ]))
}



