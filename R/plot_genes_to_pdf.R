plot_genes_to_pdf <- function(
  genes = "AT3G61060",
  tags = "",
  tags2 = "",
  filename = "./genes.pdf",
  dense = FALSE,
  ymax = 4,
  ymin = -2,
  return_directly = FALSE,
  font_size = 7,
  label_size = 3.5
){

  if (!return_directly){

    if (dense){
      ggpubr::ggarrange(plotlist = Map(plot_gene_dense, genes, tags, tags2,
                                       rep(ymax, length(genes)), rep(ymin, length(genes)),
                                       rep(1, length(genes)), rep(font_size, length(genes)),
                                       rep(label_size, length(genes))
                                       ),
                        nrow = length(genes)) %>%
        ggplot2::ggsave(
          filename = filename,
          height = length(genes)*1.1,
          width = 2.0,
          limitsize = FALSE
        )
    } else {
      ggpubr::ggarrange(plotlist = Map(plot_gene, genes, tags,
                                       rep(ymax, length(genes)), rep(ymin, length(genes)),
                                       rep(1, length(genes)), rep(font_size, length(genes)),
                                       rep(label_size, length(genes))
                                       ),
                        nrow = length(genes)) %>%
        ggplot2::ggsave(
          filename = filename,
          height = length(genes)*1.5,
          width = 2.5,
          limitsize = FALSE
        )
    }

  } else {

    if (dense){
      ggpubr::ggarrange(plotlist = Map(plot_gene_dense, genes, tags, tags2,
                                       rep(ymax, length(genes)), rep(ymin, length(genes)),
                                       rep(1, length(genes)), rep(font_size, length(genes)),
                                       rep(label_size, length(genes))
                                       ),
                        nrow = length(genes))
    } else {
      ggpubr::ggarrange(plotlist = Map(plot_gene, genes, tags,
                                       rep(ymax, length(genes)), rep(ymin, length(genes)),
                                       rep(1, length(genes)), rep(font_size, length(genes)),
                                       rep(label_size, length(genes))
                                       ),
                        nrow = length(genes))
    }
  }

}
