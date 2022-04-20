# run_cluster_old <- function(
#   dir,
#   k,
#   eigen_vector_table,
#   hclust_method = "ward.D"
# ){
#
#   ## STEP 0: SET UP DIRECTORIES -----------------------------
#   if (!dir.exists(paste0(dir, "/results"))){dir.create(paste0(dir, "/results"), recursive = TRUE)}
#   if (!dir.exists(paste0(dir, "/figures"))){dir.create(paste0(dir, "/figures"), recursive = TRUE)}
#
#
#   ## STEP 1: GET CLUSTERS FROM EIGENVECTORS -----------------------------
#
#   ## Load the Eigen vectors
#   eigen_vecs <- eigen_vector_table[,1:k]
#
#   hclust_out <- stats::hclust(stats::as.dist(1 - apcluster::corSimMat(eigen_vecs)), method = hclust_method)
#   cutree_out <- dendextend::cutree(stats::as.dendrogram(hclust_out), k = k, order_clusters_as_data = FALSE)
#   clusters <- lapply(1:k, function(x){return(names(cutree_out[cutree_out == x]))})
#
#   ## Output the clusters
#   utils::write.table(cluster_to_dataframe(clusters), file = paste0(dir, "results/cluster_table.tsv"),
#                      sep = "\t", row.names = FALSE, quote = FALSE)
#
#
#   ## STEP 2: PLOT HEAT MAPS -----------------------------
#
#   heatmap_density <- clustering_heatmap(
#     input_matrix = TMM_mean_matrix %>% calculate_z_row(),
#     hclust_out = hclust_out,
#     cluster_no = k,
#     rDEI_table = rDEI_summary,
#     dendrogram = FALSE
#   )
#
#   grDevices::pdf(paste0(dir, "figures/heatmap_", hclust_method, ".pdf"), height = 4, width = 8)
#   ComplexHeatmap::draw(heatmap_density)
#   grDevices::dev.off()
#
# }



# get_exemplar_from_label <- function(
#   dend,
#   labels,
#   merge_height
# ){
#
#   ## For each cluster, get the height at which the subtree is merged
#   ## And find the number of clusters at that height
#   n_exemplars_at_merge_height <- dendextend::cutree(dend, h = merge_height) %>% max()
#   ## Get the exemplars at that height
#   exemplars_at_height <- identifiers[aggcluster_out@exemplars[[n_exemplars_at_merge_height]]]
#   ## Get the intersection between those exemplars and the cluster labels
#   (intersect(exemplars_at_height, C1_labels))
#
# }

# cluster_sort_genes_by_rDEI <- function(
#   hclust_out,
#   k,
#   rDEI_summary
# ){
#
#   ## Get cutree result
#   cutree_out <- hclust_out %>% stats::cutree(k = k)
#
#   ## Get the clusters of gene list
#   clusters <- lapply(1:k, function(x){return(names(cutree_out[cutree_out == x]))})
#
#   ## Get the correct order by mean rDEI
#   cluster_order <- clusters %>%
#     sapply(function(x){base::mean(rDEI_summary[rDEI_summary$geneID %in% x, "log2_rDEI_SDLD"], na.rm = TRUE)}) %>%
#     base::order()
#
#   ## Get the ordered clusters
#   ordered_clusters <- clusters[cluster_order]
#
#   ## Now that the big clusters are ordered, we have to make sure the genes within each cluster is ordered too
#   twice_ordered <- ordered_clusters %>% lapply(function(x){x[order(match(x, labels(hclust_out)))]})
#
#   return(twice_ordered)
# }


# clustering_heatmap_plain <- function(
#   input_matrix,
#   hclust_out,
#   cluster_no,
#   rDEI_table = rDEI_summary
# ){
#
#   ## APPERANCE --------------------------------
#
#   ## Options for the heat map appearance
#   ComplexHeatmap::ht_opt(
#     legend_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     legend_labels_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_column_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_column_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_row_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_row_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     message = FALSE
#   )
#
#   ## PROCESS INPUT DATA --------------------------------
#   row_order <- c("LD_00", "LD_04", "LD_08", "LD_12", "LD_16", "LD_20", "EQ_00", "EQ_04", "EQ_08", "EQ_12", "EQ_16", "EQ_20", "SD_00", "SD_04", "SD_08", "SD_12", "SD_16", "SD_20")
#
#   ## Rotate the dendrogram
#   ordered_clusters <- hclust_out %>% cluster_sort_genes_by_rDEI(k = cluster_no, rDEI_summary = rDEI_summary)
#   column_order <- ordered_clusters %>% unlist()
#   mat_expression <- t(input_matrix[column_order, row_order])
#
#   column_split_order <- rep(1:k, times = lengths(ordered_clusters)) %>% as.factor()
#   anno_column_order <- 1:ncol(mat_expression) %>% split(column_split_order)
#
#   ## SET PANELS --------------------------------
#
#   ## This is the function for getting the density plot
#   panel_fun <- function(index){
#
#     ## the input is index, that is the row number of a gene in the mat_expression input matrix for the heatmap
#     ## since the row numbers of the mat_expression matrix and those of the rDEI_table matrix are not the same
#     ## we need to convert it to the actual genes
#     genes <- colnames(mat_expression)[index]
#
#     SDLD <- rDEI_table %>%
#       dplyr::filter(.data$geneID %in% genes) %>%
#       dplyr::select("log2_rDEI_SDLD") %>%
#       dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#       tidyr::drop_na() %>%
#       unlist() %>% as.numeric()
#
#     SDEQ <- rDEI_table %>%
#       dplyr::filter(.data$geneID %in% genes) %>%
#       dplyr::select("log2_rDEI_SDEQ") %>%
#       dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#       tidyr::drop_na() %>%
#       unlist() %>% as.numeric()
#
#     EQLD <- rDEI_table %>%
#       dplyr::filter(.data$geneID %in% genes) %>%
#       dplyr::select("log2_rDEI_LDEQ") %>%
#       dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#       tidyr::drop_na() %>%
#       unlist() %>% as.numeric() %>% magrittr::multiply_by(-1)
#
#     df_values <- data.frame(
#       value = c(SDLD, SDEQ, EQLD),
#       color = c(rep("SDLD", length(SDLD)), rep("SDEQ", length(SDEQ)), rep("EQLD", length(EQLD)))
#     )
#
#     g <- ggplot2::ggplot(df_values, ggplot2::aes_string(x = "value", y = "color", fill = "color")) +
#       ggridges::geom_density_ridges(alpha = 0.4, size = 0.335, scale = 5) +
#       ggplot2::geom_vline(xintercept = 0, size = 0.335) +
#       ggplot2::xlim(-1.5, 1.5) +
#       ggplot2::scale_y_discrete(expand = ggplot2::expand_scale(mult = c(0, 3))) +
#       #ggplot2::coord_cartesian(clip = "off") +
#       ggplot2::coord_flip() +
#       ggplot2::scale_fill_manual(values=c("#F39B7F", "#4DBBD5", "#8491B4")) +
#       #theme_Prism() +
#       ggmap::theme_nothing() +
#       ggplot2::theme(
#         line = ggplot2::element_line(colour = "black", size = 0.67/2, linetype = 1, lineend = "square"),
#         panel.grid.major = ggplot2::element_blank(),
#         panel.grid.minor = ggplot2::element_blank()
#       )
#
#     g = grid::grid.grabExpr(print(g))
#
#     grid::pushViewport(grid::viewport())
#     grid::grid.rect()
#     grid::grid.draw(g)
#     grid::popViewport()
#   }
#
#   anno = ComplexHeatmap::anno_zoom(
#     align_to = anno_column_order,
#     which = "column",
#     side = "bottom",
#     panel_fun = panel_fun,
#     size = grid::unit(0.8, "cm"),
#     gap = grid::unit(0.5, "cm"),
#     width = grid::unit(2.2, "cm"),
#     link_width = grid::unit(0.2, "cm")
#   )
#
#   ## LIGHT DARK BAR --------------------------------
#
#   LD_bar <- ComplexHeatmap::rowAnnotation(
#     "photoperiod" = c(c("Dark", "Light", "Light", "Light", "Light", "Dark"), c("Dark", "Light", "Light", "Light", "Dark", "Dark"), c("Dark", "Light", "Light", "Dark", "Dark", "Dark")),
#     border = TRUE,
#     col = list("photoperiod" = c("Light" = "#EEEEEE", "Dark" = "#000000")),
#     simple_anno_size = grid::unit(2, "mm"),
#     annotation_name_side = "top",
#     annotation_name_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial")
#   )
#
#   ## MAIN HEATMAP ACTUALLY OUTPUTTED --------------------------------
#
#   main <- ComplexHeatmap::Heatmap(
#
#     mat_expression,
#
#     ## Rows
#     row_order = row_order,
#     row_labels = rep(c("24", "04", "08", "12", "16", "20"), 3),
#     row_title_side = c("left"),
#     cluster_row_slices = FALSE,
#     row_split = factor(c(rep("LD", 6), rep("EQ", 6), rep("SD", 6)), levels = c("LD", "EQ", "SD")),
#     row_names_rot = 0,
#     row_names_centered = TRUE,
#
#     ## Columns
#     column_order = column_order,
#     show_column_names = FALSE,
#     column_title = paste0(" ", as.character(1:cluster_no)),
#     column_split = column_split_order,
#
#     ## Others
#     name = "expression (Z-score)",
#     heatmap_legend_param = list(direction = "horizontal"),
#     left_annotation = LD_bar,
#     bottom_annotation = ComplexHeatmap::columnAnnotation(foo = anno),
#
#     ## Graphics
#     use_raster = TRUE,
#     raster_quality = 5,
#     col = circlize::colorRamp2(c(-2, 0, 2), c("#3B4992", "#FFFFFF", "#EE0000"))
#   )
#
#   a <- ComplexHeatmap::column_order(main)
#
#   return(main)
# }


# clustering_heatmap_dend <- function(
#   input_matrix,
#   dendrogram,
#   cluster_no,
#   rDEI_table
# ){
#
#   ## APPERANCE --------------------------------
#
#   ## Options for the heat map appearance
#   ComplexHeatmap::ht_opt(
#     legend_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     legend_labels_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_column_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_column_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_row_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     heatmap_row_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
#     message = FALSE
#   )
#
#   ## PROCESS INPUT DATA --------------------------------
#   row_order <- c("LD_00", "LD_04", "LD_08", "LD_12", "LD_16", "LD_20", "EQ_00", "EQ_04", "EQ_08", "EQ_12", "EQ_16", "EQ_20", "SD_00", "SD_04", "SD_08", "SD_12", "SD_16", "SD_20")
#
#   ## Rotate the dendrogram
#   #hclust_out <- hclust_out %>% cluster_rotate_dend_by_rDEI(k = cluster_no, rDEI_summary = rDEI_summary)
#
#   mat_expression <- t(input_matrix[, row_order])
#
#   ## SET PANELS --------------------------------
#
#   ## This is the function for getting the density plot
#   panel_fun <- function(index){
#
#     ## the input is index, that is the row number of a gene in the mat_expression input matrix for the heatmap
#     ## since the row numbers of the mat_expression matrix and those of the rDEI_table matrix are not the same
#     ## we need to convert it to the actual genes
#
#     if (length(index) >= 50){
#
#       genes <- colnames(mat_expression)[index]
#
#       SDLD <- rDEI_table %>%
#         dplyr::filter(.data$geneID %in% genes) %>%
#         dplyr::select("log2_rDEI_SDLD") %>%
#         dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#         tidyr::drop_na() %>%
#         unlist() %>% as.numeric()
#
#       SDEQ <- rDEI_table %>%
#         dplyr::filter(.data$geneID %in% genes) %>%
#         dplyr::select("log2_rDEI_SDEQ") %>%
#         dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#         tidyr::drop_na() %>%
#         unlist() %>% as.numeric()
#
#
#       EQLD <- rDEI_table %>%
#         dplyr::filter(.data$geneID %in% genes) %>%
#         dplyr::select("log2_rDEI_LDEQ") %>%
#         dplyr::mutate_all(.funs = function(x){ifelse(is.infinite(x),NA,x)}) %>%
#         tidyr::drop_na() %>%
#         unlist() %>% as.numeric() %>% magrittr::multiply_by(-1)
#
#       df_values <- data.frame(
#         value = c(SDLD, SDEQ, EQLD),
#         color = c(rep("SDLD", length(SDLD)), rep("SDEQ", length(SDEQ)), rep("EQLD", length(EQLD)))
#       )
#
#       g <- ggplot2::ggplot(df_values, ggplot2::aes_string(x = "value", y = "color", fill = "color")) +
#         ggridges::geom_density_ridges(alpha = 0.4, size = 0.335, scale = 5) +
#         ggplot2::geom_vline(xintercept = 0, size = 0.335) +
#         ggplot2::xlim(-1.5, 1.5) +
#         ggplot2::scale_y_discrete(expand = c(0, 1.1)) +
#         ggplot2::coord_flip() +
#         ggplot2::scale_fill_manual(values=c("#F39B7F", "#4DBBD5", "#8491B4")) +
#         #theme_Prism() +
#         ggmap::theme_nothing() +
#         ggplot2::theme(line = ggplot2::element_line(colour = "black", size = 0.67/2, linetype = 1, lineend = "square"))
#
#       g = grid::grid.grabExpr(print(g))
#
#       grid::pushViewport(grid::viewport())
#       grid::grid.rect()
#       grid::grid.draw(g)
#       grid::popViewport()
#
#     }
#
#   }
#
#   panel_order <- ComplexHeatmap::Heatmap(
#     mat_expression,
#     cluster_columns = dendrogram,
#     column_split = cluster_no
#   ) %>%
#     #ComplexHeatmap::draw() %>%
#     ComplexHeatmap::column_order() %>%
#     suppressWarnings()
#
#   anno = ComplexHeatmap::anno_zoom(
#     align_to = panel_order,
#     which = "column",
#     side = "bottom",
#     panel_fun = panel_fun,
#     size = grid::unit(1, "cm"),
#     gap = grid::unit(0.5, "cm"),
#     width = grid::unit(2.2, "cm"),
#     link_width = grid::unit(0.2, "cm")
#   )
#
#   ## LIGHT DARK BAR --------------------------------
#
#   LD_bar <- ComplexHeatmap::rowAnnotation(
#     "photoperiod" = c(c("Dark", "Light", "Light", "Light", "Light", "Dark"), c("Dark", "Light", "Light", "Light", "Dark", "Dark"), c("Dark", "Light", "Light", "Dark", "Dark", "Dark")),
#     border = TRUE,
#     col = list("photoperiod" = c("Light" = "#EEEEEE", "Dark" = "#000000")),
#     simple_anno_size = grid::unit(2, "mm"),
#     annotation_name_side = "top",
#     annotation_name_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial")
#   )
#
#   ## MAIN HEATMAP ACTUALLY OUTPUTTED --------------------------------
#
#   main <- ComplexHeatmap::Heatmap(
#
#     mat_expression,
#
#     ## Rows
#     row_order = row_order,
#     row_labels = rep(c("24", "04", "08", "12", "16", "20"), 3),
#     row_title_side = c("left"),
#     cluster_row_slices = FALSE,
#     row_split = factor(c(rep("LD", 6), rep("EQ", 6), rep("SD", 6)), levels = c("LD", "EQ", "SD")),
#     row_names_rot = 0,
#     row_names_centered = TRUE,
#
#     ## Columns
#     column_title = paste0(" ", as.character(1:cluster_no)),
#     show_column_names = FALSE,
#     cluster_columns = dendextend::color_branches(dendrogram, k = cluster_no),
#     column_split = cluster_no,
#     column_dend_height = grid::unit(1, "cm"),
#     column_gap = grid::unit(0.5, "mm"),
#     border = FALSE,
#
#     ## Others
#     name = "expression (Z-score)",
#     heatmap_legend_param = list(direction = "horizontal"),
#     left_annotation = LD_bar,
#     bottom_annotation = ComplexHeatmap::columnAnnotation(foo = anno),
#
#     ## Graphics
#     use_raster = TRUE,
#     raster_quality = 2,
#     col = circlize::colorRamp2(c(-2, 0, 2), c("#3B4992", "#FFFFFF", "#EE0000"))
#   ) %>% suppressMessages() %>% suppressWarnings()
#
#   return(main)
# }

