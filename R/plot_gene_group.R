plot_gene_group <- function(
  genes = c("AT3G61060", "AT3G47340"),
  ymax = 2.5,
  ymin = -2.5,
  transparency = 1
){

  ## Data manipulation ---------------------

  if (length(genes) > 1){
    plot_table <- TMM_mean_matrix[genes, ] %>%
      t() %>%
      scale() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "ID") %>%
      tidyr::pivot_longer(cols = colnames(TMM_mean_matrix)) %>%
      dplyr::mutate(color = .data$name %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 1)}) %>% unlist()) %>%
      dplyr::mutate(time = .data$name %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 2) %>% as.numeric()}) %>% unlist())
  } else {
    plot_table <- TMM_mean_matrix[genes, ] %>%
      scale() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name") %>%
      dplyr::mutate(ID = rep(genes, 18)) %>%
      dplyr::mutate(color = .data$name %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 1)}) %>% unlist()) %>%
      dplyr::mutate(time = .data$name %>% strsplit(split = "_") %>% lapply(function(x){magrittr::extract2(x, 2) %>% as.numeric()}) %>% unlist())
    colnames(plot_table) <- c("name", "value", "ID", "color", "time")
    plot_table <- plot_table[, c("ID", "name", "value", "color", "time")]
  }

  ## Plot ---------------------
  plot_LD <- ggplot2::ggplot(data = plot_table[plot_table$color == "LD",], ggplot2::aes_string(x = "time", y = "value", group = "ID")) +
    ggplot2::geom_line(alpha = transparency, size = 1, col = "#EE0000") +
    ggplot2::scale_x_continuous(limits = c(-0.2, 25), breaks = seq(0, 24, 4), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), breaks = c(ymin, 0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = "Time (h)", y = "Relative expression (Z score)", title = "") +
    ggplot2::coord_fixed(ratio = 3.2) +
    ggplot2::annotate("text", x = 5, y = 2, label= "bold(LD)", size = 2.1, parse = TRUE) +
    ggplot2::theme(
      legend.position="none",
      strip.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.67, linetype = 1, lineend = "square"),
      axis.text = ggplot2::element_text(size = 7, colour = "black", face = "bold"),
      axis.title = ggplot2::element_text(size = 7, face = "bold")
    )

  plot_EQ <- ggplot2::ggplot(data = plot_table[plot_table$color == "EQ",], ggplot2::aes_string(x = "time", y = "value", group = "ID")) +
    ggplot2::geom_line(alpha = transparency, size = 1, col = "#008B45") +
    ggplot2::scale_x_continuous(limits = c(-0.2, 25), breaks = seq(0, 24, 4), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), breaks = c(ymin, 0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = "Time (h)", y = "Relative expression (Z score)", title = "") +
    ggplot2::coord_fixed(ratio = 3.2) +
    ggplot2::annotate("text", x = 5, y = 2, label= "bold(EQ)", size = 2.1, parse = TRUE) +
    ggplot2::theme(
      legend.position="none",
      strip.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.67, linetype = 1, lineend = "square"),
      axis.text = ggplot2::element_text(size = 7, colour = "black", face = "bold"),
      axis.title = ggplot2::element_text(size = 7, face = "bold")
    )

  plot_SD <- ggplot2::ggplot(data = plot_table[plot_table$color == "SD",], ggplot2::aes_string(x = "time", y = "value", group = "ID")) +
    ggplot2::geom_line(alpha = transparency, size = 1, col = "#3B4992") +
    ggplot2::scale_x_continuous(limits = c(-0.2, 25), breaks = seq(0, 24, 4), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax), breaks = c(ymin, 0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = "Time (h)", y = "Relative expression (Z score)", title = "") +
    ggplot2::coord_fixed(ratio = 3.2) +
    ggplot2::annotate("text", x = 5, y = 2, label= "bold(SD)", size = 2.1, parse = TRUE) +
    ggplot2::theme(
      legend.position="none",
      strip.text = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.67, linetype = 1, lineend = "square"),
      axis.text = ggplot2::element_text(size = 7, colour = "black", face = "bold"),
      axis.title = ggplot2::element_text(size = 7, face = "bold")
    )

  plot <- ggpubr::ggarrange(plotlist = list(plot_LD, plot_EQ, plot_SD), nrow = 1)

  return(plot)
}
