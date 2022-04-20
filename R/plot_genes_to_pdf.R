plot_genes_to_pdf <- function(
  genes = "AT3G61060",
  tags = "",
  tags2 = "",
  filename = "./genes.pdf",
  dense = FALSE,
  ymax = 4,
  ymin = -2
){

  if (dense){

    ggpubr::ggarrange(plotlist = Map(plot_gene_dense, genes, tags, tags2,
                                     rep(ymax, length(genes)), rep(ymin, length(genes))),
                      nrow = length(genes)) %>%
      ggplot2::ggsave(
        filename = filename,
        height = length(genes)*1.1,
        width = 2.0,
        limitsize = FALSE
      )

  } else {

    ggpubr::ggarrange(plotlist = Map(plot_gene, genes, tags, rep(ymax, length(genes)), rep(ymin, length(genes))), nrow = length(genes)) %>%
      ggplot2::ggsave(
        filename = filename,
        height = length(genes)*1.5,
        width = 2.5,
        limitsize = FALSE
      )

  }
}