plot_gene_dense <- function(
  gene,
  tag = "",
  tag2 = "",
  ymax = 4,
  ymin = -4,
  transparency = 1
){

  ## Data manipulation ---------------------
  plot_table <- TMM_mean_matrix[gene, ] %>%
    scale() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ID") %>%
    dplyr::mutate(color = .data$ID %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 1)}) %>% unlist()) %>%
    dplyr::mutate(time = .data$ID %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 2) %>% as.numeric()}) %>% unlist())

  name_label <- paste0('bold("', tag, ":", gene, '")')
  number_label <- paste0('bold("n = ', tag2, '")')

  ## Plot ---------------------
  plot <- ggplot2::ggplot(data = plot_table, ggplot2::aes_string(x = "time", y = "V1", group = "color")) +
    ggplot2::geom_line(alpha = transparency, size = 1, ggplot2::aes_string(color = "color")) +
    ggplot2::scale_color_manual(values=c("#008B45", "#EE0000", "#3B4992")) +
    ggplot2::scale_x_continuous(limits = c(-0.2, 25), breaks = seq(0, 24, 4), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), breaks = c(ymin, 0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = "", y = "", title = "") +
    ggplot2::coord_fixed(ratio = 3.2) +
    ggplot2::annotate("text", x = 2, y = 3, label= name_label, size = 2.1, parse = TRUE, hjust = 0) +
    ggplot2::annotate("text", x = 12, y = 2, label= number_label, size = 2.1, parse = TRUE, hjust = 0) +
    ggplot2::theme(
      legend.position="none",
      strip.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.67, linetype = 1, lineend = "square"),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )

  return(plot)
}
