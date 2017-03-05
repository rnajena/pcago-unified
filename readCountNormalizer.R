#
# Contains routines to normalize readcounts
#

supportedReadcountNormalizationTypes <- c("None" = "none", "DeSeq2" = "deseq", "TPM" = "tpm")

#' Applies normalization to a read count table. The normalization algorithm is determined by normalizationtype
#'
#' @param rawdata Read count table
#' @param normalizationtype One of supportedReadcountNormalizationTypes
#'
#' @return Normalized read count table
#' @export
#'
#' @examples
applyReadcountNormalization <- function(rawdata, normalizationtype) {
  return(rawdata) # todo
}
