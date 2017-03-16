#'
#' Contains methods that return information about each gene
#' For example: variance of each gene
#' 
#' The final output is the gene annotation with different kinds of information
#' Those will be used for selecting a set of genes to apply PCA for
#'

library(shiny)
library(matrixStats)

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer
supportedAnnotationImporters <- c("GFF v3" = "gff3")
supportedAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
availableAnnotationSamples <- c("Vitamins" = "vitamins.gff3")

#' Imports annotation from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedAnnotationDataTypes
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importAnnotation <- function(filehandle, datatype) {
  
}

#' Creates the final annotation for all genes
#'
#' @param readcounts Read counts
#'
#' @return Data frame with gene id ('id'), variance ('var') and relative variance ('var.relative') of each gene. Sorted by variance (decreasing)
#' @export
#'
#' @examples
geneVarianceTable <- function(readcounts) {
  
  if(is.null(readcounts) || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  geneids <- rownames(readcounts)
  gene.var <- rowVars(data.matrix(readcounts))
  
  variances.table <- data.frame(var = gene.var,
                          var.relative = gene.var / sum(gene.var),
                          row.names = geneids)
  
  # Sort by variance
  variances.table <- variances.table[order(variances.table$var, decreasing = T), , drop = F]
  
  return(variances.table)
}

#' Selects only the top n most variant genes from the read count table.
#'
#' @param readcounts The readcount table to be filtered
#' @param annotation Gene annotation object
#' @param top The top n of genes 1 <= n <= nrow(readcounts)
#'
#' @return Filtered readcount table
#' @export
#'
#' @examples
selectTopVariantGeneReadcounts <- function(readcounts, annotation, top) {
  
  if(is.null(readcounts) || is.null(annotation) || top <= 0 || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  # Fetch the variances for all available genes
  # But only select those which are in the read count table
  variances <- annotation$variances[rownames(readcounts),]
  
  topgeneids <- rownames(variances)[1:min(top, nrow(variances))]
  readcounts.geneids <- rownames(readcounts)
  
  indices <- na.omit(match(topgeneids, readcounts.geneids))
  
  result <- readcounts[indices,]
  
  return(result)
}

