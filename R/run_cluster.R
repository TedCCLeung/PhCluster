run_cluster <- function(
  dir = "/Users/TedCCLeung/Documents/Projects/Photoperiod/4_clustering/hybrid/",
  deepsplit_degree = 3,
  k = 14,
  skip_additional_heatmaps = FALSE,
  aggcluster_object,
  gene_IDs,
  expression_matrix
){

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  #### PROCESS THE CLUSTERING INFORMATION ---------------------

  ## STATIC
  # Extract the clusters, exemplars and dendrogram from the aggcluster result
  aggcluster_static <- process_aggcluster_static(
    aggcluster_out = aggcluster_object,
    identifiers = gene_IDs,
    k = k
  )

  static_exemplars <- aggcluster_static$exemplars
  static_dend <- aggcluster_static$dendrogram
  static_labels <- aggcluster_static$labels
  static_clusters <- aggcluster_static$clusters

  ## DYNAMIC
  ## Get cluster from the tree
  aggcluster_dynamic <- process_aggcluster_dynamic(
    aggcluster_out = aggcluster_object,
    original_matrix = expression_matrix,
    identifiers = gene_IDs,
    deepsplit_degree = deepsplit_degree
  )

  dynamic_exemplars <- aggcluster_dynamic$exemplars
  dynamic_dend <- aggcluster_dynamic$dendrogram
  dynamic_labels <- aggcluster_dynamic$labels
  dynamic_clusters <- aggcluster_dynamic$clusters

  #### MAKE HEATMAP ---------------------

  ## Make heatmap
  heatmap_input_matrix <- TMM_mean_matrix[gene_IDs, ] %>% t() %>% scale() %>% t()

  if (!skip_additional_heatmaps){

    ## These are the more basic heatmaps with only one level of clustering
    static_heatmap <- clustering_heatmap_static(
      input_matrix =  heatmap_input_matrix,
      dendrogram = static_dend,
      cluster_no = k
    )
    grDevices::pdf(paste0(dir, "/heatmap_static.pdf"), width = 9, height = 4)
    static_heatmap %>% ComplexHeatmap::draw()
    grDevices::dev.off()


    dynamic_heatmap <- clustering_heatmap_dynamic(
      input_matrix =  heatmap_input_matrix,
      clusters = dynamic_clusters
    )
    grDevices::pdf(paste0(dir, "/heatmap_dynamic.pdf"), width = 9, height = 4)
    dynamic_heatmap %>% ComplexHeatmap::draw()
    grDevices::dev.off()
  }

  ## Hybrid heatmap
  hybrid_heatmap <- clustering_heatmap_hybrid(
    input_matrix = heatmap_input_matrix,
    dendrogram = static_dend,
    cluster_no = k,
    dynamic_labels = dynamic_labels
  )
  grDevices::pdf(paste0(dir, "/heatmap_hybrid.pdf"), width = 9, height = 4)
  hybrid_heatmap %>% ComplexHeatmap::draw()
  grDevices::dev.off()

  #### OUTPUT OTHER INFORMATION ---------------------
  df_static <- static_labels %>% as.data.frame() %>% tibble::rownames_to_column(var = "geneID"); colnames(df_static) <- c("geneID", "static")
  df_dynamic <- dynamic_labels %>% as.data.frame() %>% tibble::rownames_to_column(var = "geneID"); colnames(df_dynamic) <- c("geneID", "dynamic")
  df_hybrid <- dplyr::full_join(df_static, df_dynamic, by = "geneID")

  df_hybrid$sub <- lapply(unique(df_hybrid$static), function(x){
    sub_clusters <- df_hybrid[df_hybrid$static == x, "dynamic"]
    sub_cluster_factor <- factor(sub_clusters, levels = unique(sub_clusters)) %>% as.numeric()
    return(paste0(x %>% as.character(), c(LETTERS, paste0("A", LETTERS))[sub_cluster_factor]))
  }) %>% unlist()

  df_hybrid <- df_hybrid[match(labels(static_dend), df_hybrid$geneID), ]

  df_hybrid$static_exemplar <- rep(static_exemplars %>% names(), times = lengths(static_clusters))
  df_hybrid$dynamic_exemplar <- rep(dynamic_exemplars %>% names(), times = lengths(dynamic_clusters))

  utils::write.table(df_hybrid, file = paste0(dir, "/clusters.tsv"), quote = FALSE, row.names = FALSE, sep = "\t")

  hybrid_clusters <- lapply(unique(df_hybrid$sub), function(x){df_hybrid[df_hybrid$sub == x, "geneID"]})
  names(hybrid_clusters) <- unique(df_hybrid$sub)

  hybrid_exemplars <- lapply(hybrid_clusters, function(x){
    res <- names(dynamic_exemplars)[names(dynamic_exemplars) %in% x]
    if (length(res) == 0){
      res <- names(static_exemplars)[names(static_exemplars) %in% x]
    }
    return(res)
  })
  hybrid_exemplars_vector <- hybrid_exemplars %>% unlist()
  df_hybrid$dynamic_exemplar <- hybrid_exemplars_vector[df_hybrid$sub]

  ## Plot out exemplars
  plot_genes_to_pdf(genes = static_exemplars %>% names(), tags = paste0(addLeadingZeros(1:length(static_clusters))), tags2 = as.character(static_exemplars), filename = paste0(dir, "/static_exemplars.pdf"), dense = TRUE)
  plot_genes_to_pdf(genes = hybrid_exemplars %>% unlist(), tags = paste0(df_hybrid$sub %>% unique()), tags2 = as.character(lengths(hybrid_clusters) %>% as.character()), filename = paste0(dir, "/dynamic_exemplars.pdf"), dense = TRUE)
}

