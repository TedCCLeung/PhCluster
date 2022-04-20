# file <- "/Users/TedCCLeung/Library/CloudStorage/Box-Box/Paper_Photoperiod/Ted/anthogenes.csv"
# df_genes <- read.csv(file, header = FALSE)
# df_genes <- df_genes[df_genes[, 1] %in% rownames(TMM_mean_matrix), ]
#
# cluster_file <- "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/4_clustering/hybrid/clusters.tsv"
#
#
# plot_genes_to_pdf(
#   genes = df_genes[, 1] %>% as.character(),
#   tags = df_genes[, 2] %>% as.character(),
#   tags2 = rep("1", nrow(df_genes)),
#   filename = "./antho_genes.pdf",
#   dense = TRUE
# )
