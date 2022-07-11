#' Extract significantly up- and down-reguluted genes from edgeR results
#'
#' @importFrom rlang .data
#'
#' @param gene Character
#' @param tag Character
#' @param ymax Numeric
#' @param ymin Numeric
#' @param transparency Numeric
#'
#' @return None

plot_gene <- function(
  gene,
  tag = "",
  ymax = 4,
  ymin = -2,
  transparency = 1,
  font_size = 7
){

  ## Data manipulation ---------------------
  plot_table <- TMM_mean_matrix[gene, ] %>%
    scale() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::mutate(color = .data$ID %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 1)}) %>% unlist()) %>%
    dplyr::mutate(time = .data$ID %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 2) %>% as.numeric()}) %>% unlist())

  if (nchar(tag) > 0){
    name_label <- paste0('bold("', gene, "-", tag, '")')
  } else {
    name_label <- paste0('bold("', gene, '")')
  }


  ## Plot ---------------------
  plot <- ggplot2::ggplot(data = plot_table, ggplot2::aes_string(x = "time", y = "V1", group = "color")) +
    ggplot2::geom_line(alpha = transparency, size = 1, ggplot2::aes_string(color = "color")) +
    ggplot2::scale_color_manual(values=c("#008B45", "#EE0000", "#3B4992")) +
    ggplot2::scale_x_continuous(limits = c(-0.2, 25), breaks = seq(0, 24, 4), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), breaks = c(ymin, 0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = "Time (h)", y = "Relative expression (Z score)", title = "") +
    ggplot2::coord_fixed(ratio = 3.2) +
    ggplot2::annotate("text", x = 12, y = 2, label= name_label, size = font_size, parse = TRUE) +
    ggplot2::theme(
      legend.position="none",
      strip.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.67, linetype = 1, lineend = "square"),
      axis.text = ggplot2::element_text(size = font_size, colour = "black", face = "bold"),
      axis.title = ggplot2::element_text(size = font_size, face = "bold")
    )

  return(plot)
}
