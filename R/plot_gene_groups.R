plot_gene_groups <- function(
  gene_groups = list(c("AT3G61060", "AT3G47340"), c("AT3G61060"), c("AT3G47340")),
  ymax = 2.5,
  ymin = -2.5
){

  plotlist <- Map(plot_gene_group, gene_groups, rep(ymax, length(gene_groups)), rep(ymin, length(gene_groups)))
  return(ggpubr::ggarrange(plotlist = plotlist, nrow = length(plotlist)))

}
