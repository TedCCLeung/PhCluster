#' Information on daily expression integral.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{geneID}{}
#'   \item{EQ_DEI}{daily expression integral at equinox photoperiod}
#'   \item{LD_DEI}{daily expression integral at long day photoperiod}
#'   \item{SD_DEI}{daily expression integral at short day photoperiod}
#'   \item{log2_rDEI_LDEQ}{}
#'   \item{log2_rDEI_SDEQ}{}
#'   \item{log2_rDEI_SDLD}{}
#' }
"rDEI_summary"


#' Expression of genes in mean TMM calculated by edgeR
#'
#' @format A data frame with 24845 rows and 18 variables:
#' \describe{
#'   \item{EQ_00}{Sample}
#'   \item{EQ_04}{Sample}
#'   \item{EQ_08}{Sample}
#'   \item{EQ_12}{Sample}
#'   \item{EQ_16}{Sample}
#'   \item{EQ_20}{Sample}
#'   \item{LD_00}{Sample}
#'   \item{LD_04}{Sample}
#'   \item{LD_08}{Sample}
#'   \item{LD_12}{Sample}
#'   \item{LD_16}{Sample}
#'   \item{LD_20}{Sample}
#'   \item{SD_00}{Sample}
#'   \item{SD_04}{Sample}
#'   \item{SD_08}{Sample}
#'   \item{SD_12}{Sample}
#'   \item{SD_16}{Sample}
#'   \item{SD_20}{Sample}
#' }
"TMM_mean_matrix"

#' Photoperiodic genes in this dataset
#'
#' @format A vector of 8302 photoperiodic genes
"photoperiodic_genes"

#' Gene names for the matrix expression_matrix_photoperiodic
#'
#' @format Character vector
"gene_identifiers"

