#'
#' Methods for read count processing
#' 

library(shiny)

#' Removes constant read count genes from the table.
#' As they result in variance = 0, scaling in the PCA step won't work
#'
#' @param readcounts 
#'
#' @return list of readcounts without constant entries (readcounts) and list of removed genes (genes.removed)
#' @export
#'
#' @examples
removeConstantReads <- function(readcounts) {
  
  invalid <- (do.call(pmin, readcounts) == do.call(pmax, readcounts))
  readcounts.removed <- readcounts[which(!invalid),]
  genes.removed <- rownames(readcounts)[invalid]
  
  return(list(readcounts = readcounts.removed, genes.removed = genes.removed))
  
}