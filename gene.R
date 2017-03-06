#
# Contains methods that return information about each gene
#

library(matrixStats)

#' Creates the final annotation for all genes
#'
#' @param inputdata 
#'
#' @return Data frame with gene id ('id'), variance ('var') <todo>
#' @export
#'
#' @examples
annotateGenes <- function(readcounts) {
  
  if(is.null(readcounts) || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  # We always have the variance of each gene
  genetable <- data.frame("id" = readcounts$id,
                          "var" = rowVars(data.matrix(readcounts[,-1])))
  # Sort by variance
  genetable <- genetable[order(genetable$var, decreasing = T),]
  
  return(genetable)
}

#' Selects only the top n most variant genes from the read count table.
#'
#' @param readcounts The readcount table to be filtered
#' @param annotation Annotation table
#' @param top The top n of genes 1 <= n <= nrow(readcounts)
#'
#' @return Filtered readcount table
#' @export
#'
#' @examples
selectTopVariantGenes <- function(readcounts, annotation, top) {
  
  if(is.null(readcounts) || is.null(annotation) || top <= 0 || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  topgeneids <- annotation[1:top,1]
  indices <- na.omit(match(topgeneids, readcounts$id))
  
  result <- readcounts[indices,]
  
  return(result)
}