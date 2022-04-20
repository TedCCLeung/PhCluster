#' Plot heat map of clustering of expression value
#'
#' @importFrom magrittr %>%
#'
#' @param input_matrix Matrix. Expression values.
#' @param dendrogram Dendrogram object.
#' @param cluster_no Numeric. Number of clusters.
#'
#' @return ComplexHeatmap

clustering_heatmap_static <- function(
  input_matrix,
  dendrogram,
  cluster_no
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
    show_column_names = FALSE,
    cluster_columns = dendextend::color_branches(dendrogram, k = cluster_no),
    column_split = cluster_no,
    column_dend_height = grid::unit(1, "cm"),
    column_gap = grid::unit(0.5, "mm"),
    border = FALSE,

    ## Others
    name = "expression (Z-score)",
    heatmap_legend_param = list(direction = "horizontal"),
    left_annotation = LD_bar,

    ## Graphics
    use_raster = TRUE,
    raster_quality = 2,
    col = circlize::colorRamp2(c(-2, 0, 2), c("#3B4992", "#FFFFFF", "#EE0000"))
  ) %>% suppressMessages() %>% suppressWarnings()

  return(main)
}
