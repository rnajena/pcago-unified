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
  geneids <- rownames(readcounts)
  gene.var <- rowVars(data.matrix(readcounts))
  gene.var.percentage <- gene.var / sum(gene.var)
  
  genetable <- data.frame(var = gene.var,
                          var.percentage = gene.var.percentage,
                          row.names = geneids)
  
  # Sort by variance
  genetable <- genetable[order(genetable$var, decreasing = T), , drop = F]
  
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
  
  topgeneids <- rownames(annotation)[1:top]
  readcounts.geneids <- rownames(readcounts)
  
  indices <- na.omit(match(topgeneids, readcounts.geneids))
  
  result <- readcounts[indices,]
  
  return(result)
}