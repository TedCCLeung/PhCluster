
expression_data <- function(
  threshold = 0,
  photoperiodic_only = TRUE
){
  EQ_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 1:6]
  LD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 7:12]
  SD_mat <- TMM_mean_matrix[rownames(TMM_mean_matrix) %>% startsWith("AT"), 13:18]

  EQ_mat <- EQ_mat[rowSums(EQ_mat) > threshold, ]
  LD_mat <- LD_mat[rowSums(LD_mat) > threshold, ]
  SD_mat <- SD_mat[rowSums(SD_mat) > threshold, ]

  common <- intersect(rownames(EQ_mat), rownames(LD_mat)) %>% intersect(rownames(SD_mat))

  if (photoperiodic_only){common <- common[common %in% photoperiodic_genes]}

  return(list("EQ" = EQ_mat[common, ], "LD" = LD_mat[common, ], "SD" = SD_mat[common, ]))
}



