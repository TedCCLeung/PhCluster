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

## Check number of clusters
#aggcluster_photoperiodic %>% apcluster::cutree(h = 0.80) %>% length() ## 41

# plot_genes_to_pdf(
#   genes = c("AT1G80340", "AT3G25590", "AT4G37310",
#             "AT3G05220", "AT4G19900", "AT4G28652",
#             "AT5G40570", "AT5G10750", "AT5G51410",
#             "AT5G03280", "AT2G43160", "AT2G08025",
#             "AT3G47100", "AT1G05347"
#             ),
#   tags = addLeadingZeros(1:14),
#   tags2 = as.character(c(114, 250, 3157, 982, 61, 519, 20, 33, 56, 43, 2883, 99, 34, 42)),
#   ymax = 4,
#   ymin = -3,
#   filename = "/Users/TedCCLeung/Documents/Projects/Photoperiod/2_analysis/2_pipeline/PhotoperiodClusters/hybrid/static_exemplars.pdf",
#   dense = TRUE)

