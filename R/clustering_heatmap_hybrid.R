#' Plot heat map of clustering of expression value
#'
#' @importFrom magrittr %>%
#'
#' @param input_matrix Matrix. Expression values.
#' @param dendrogram Dendrogram object.
#' @param cluster_no Numeric. Number of clusters.
#' @param dynamic_labels Dynamic clustering labels
#'
#' @return ComplexHeatmap

clustering_heatmap_hybrid <- function(
  input_matrix,
  dendrogram,
  cluster_no,
  dynamic_labels
){

  ## APPERANCE --------------------------------

  ## Options for the heat map appearance
  ComplexHeatmap::ht_opt(
    legend_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    legend_labels_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    heatmap_column_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    heatmap_column_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    heatmap_row_title_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    heatmap_row_names_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    message = FALSE
  )

  ## PROCESS INPUT DATA --------------------------------
  row_order <- c("LD_00", "LD_04", "LD_08", "LD_12", "LD_16", "LD_20", "EQ_00", "EQ_04", "EQ_08", "EQ_12", "EQ_16", "EQ_20", "SD_00", "SD_04", "SD_08", "SD_12", "SD_16", "SD_20")

  mat_expression <- t(input_matrix[, row_order])

  ## LIGHT DARK BAR --------------------------------

  LD_bar <- ComplexHeatmap::rowAnnotation(
    "photoperiod" = c(c("Dark", "Light", "Light", "Light", "Light", "Dark"), c("Dark", "Light", "Light", "Light", "Dark", "Dark"), c("Dark", "Light", "Light", "Dark", "Dark", "Dark")),
    border = TRUE,
    col = list("photoperiod" = c("Light" = "#EEEEEE", "Dark" = "#000000")),
    simple_anno_size = grid::unit(2, "mm"),
    annotation_name_side = "top",
    annotation_name_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial")
  )

  ## DYNAMIC BAR --------------------------------

  membership <- dynamic_labels[labels(dendrogram)]
  coloring_scheme <- rep(c("#EEEEEE", "#000000"), floor((length(unique(membership)))/2))
  if (length(coloring_scheme) < length(unique(membership))){coloring_scheme <- c(coloring_scheme, "#EEEEEE")}
  names(coloring_scheme) <- unique(membership) %>% as.character()

  dynamic_bar <- ComplexHeatmap::columnAnnotation(
    "subgroups" = membership[rownames(input_matrix)] %>% as.character(),
    border = TRUE,
    col = list("subgroups" = coloring_scheme),
    simple_anno_size = grid::unit(2, "mm"),
    #annotation_name_side = "top",
    annotation_name_gp = grid::gpar(fontsize = 7, fontface = "bold", base_family="Arial"),
    show_legend = FALSE
  )

  ## rDEI bar --------------------------------

  power <- 4
  total_DEI <- (rDEI_summary$EQ_DEI)^power + (rDEI_summary$LD_DEI)^power + (rDEI_summary$SD_DEI)^power

  DEI_ratio_matrix <- data.frame(
    LD = (rDEI_summary$LD_DEI)^power/total_DEI,
    EQ = (rDEI_summary$EQ_DEI)^power/total_DEI,
    SD = (rDEI_summary$SD_DEI)^power/total_DEI
  ) %>% as.matrix()
  rownames(DEI_ratio_matrix) <- rDEI_summary$geneID
  DEI_ratio_matrix <- DEI_ratio_matrix[rownames(input_matrix), ]
  rownames(DEI_ratio_matrix) <- NULL

  rDEI_bar <- ComplexHeatmap::HeatmapAnnotation(
    DEI = ComplexHeatmap::anno_barplot(
      DEI_ratio_matrix,
      gp = grid::gpar(fill = c("LD" = "#EE0000", "EQ" = "#00EE00", "SD" = "#0000EE"), lty = 0, alpha = 0.25),
      bar_width = 1,
      height = grid::unit(1, "cm")
    ),
    gp = grid::gpar(lty = 0, col = "#FFFFFF"),
    border = FALSE
  )

  ## MAIN HEATMAP ACTUALLY OUTPUTTED --------------------------------

  main <- ComplexHeatmap::Heatmap(

    mat_expression,

    ## Rows
    row_order = row_order,
    row_labels = rep(c("24", "04", "08", "12", "16", "20"), 3),
    row_title_side = c("left"),
    cluster_row_slices = FALSE,
    row_split = factor(c(rep("LD", 6), rep("EQ", 6), rep("SD", 6)), levels = c("LD", "EQ", "SD")),
    row_names_rot = 0,
    row_names_centered = TRUE,

    ## Columns
    column_title = NULL,
    column_labels = FALSE,
    show_column_names = FALSE,
    cluster_columns = dendextend::color_branches(dendrogram, k = cluster_no),
    column_split = cluster_no,
    column_dend_height = grid::unit(1, "cm"),
    column_gap = grid::unit(0.3, "mm"),
    border = FALSE,

    ## Others
    name = "expression (Z-score)",
    heatmap_legend_param = list(direction = "horizontal"),
    left_annotation = LD_bar,
    bottom_annotation = dynamic_bar,
    top_annotation = rDEI_bar,


    ## Graphics
    use_raster = TRUE,
    raster_quality = 2,
    col = circlize::colorRamp2(c(-2, 0, 2), c("#3B4992", "#FFFFFF", "#EE0000"))
  ) %>% suppressMessages() %>% suppressWarnings()

  return(main)
}
