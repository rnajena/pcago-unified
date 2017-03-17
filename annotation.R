#'
#' Contains methods that return information about each gene
#' For example: variance of each gene
#' 
#' The final output is the gene annotation with different kinds of information
#' Those will be used for selecting a set of genes to apply PCA for
#'

library(shiny)
library(matrixStats)
library(rtracklayer)

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer
supportedAnnotationImporters <- c("GFF" = "gff")
supportedAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
availableAnnotationSamples <- c("Vitamins" = "vitamins.gff3")

#' Imports an annotation and returns a data frame with several information.
#' This annotation will be later used to extract the gene information.
#' 
#' For all annotated genes the table will at least contain following entries:
#' 
#' seq.start: Start position of the feature
#' seq.end: End position of the feature
#' seq.length: Length of the feature sequence
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedAnnotationDataTypes
#'
#' @return Updated annotation
#' @export
#'
#' @examples
importAnnotation <- function(filehandle, datatype) {
  
  if(datatype == "gff") {
    gr <- import.gff(filehandle)
    
    seq.start <- start(gr)
    seq.end <- end(gr)
    seq.length <- width(gr)
    
    table <- mcols(gr)
    table$seq.start <- seq.start
    table$seq.end <- seq.end
    table$seq.length <- seq.length
    
    return(table)
  }
  else {
    return(NULL)
  }
  
}

#' Condenses multiple information sources about genes into one data source about the genes in the read counts
#' This will be later used to filter genes
#' 
#' Access the information about a gene by x$gene.id
#' 
#' A gene has following information entries:
#' 
#' variance: The variance of this gene
#' variance.relative: The relative variance of this gene
#' sequence: The sequence ID the gene is located in
#' sequence.start: The start position of this gene
#' sequence.stop: The stop position of the gene
#' sequence.length: The length of the gene
#' features: Vector of features that are associated with the gene
#' go.terms: Vector of GO terms associated with the gene
#'
#' @param readcounts The read count table
#' @param variances The gene variance table
#' @param annotation The gene annotation table
#'
#' @return List indexed by gene ID
#' @export
#'
#' @examples
buildGeneInformation <- function(readcounts, variances, annotation) {
  
}


#' Creates a table with gene ID and its variance and relative variance.
#'
#' @param readcounts Read counts
#'
#' @return Data frame with gene id ('id'), variance ('var') and relative variance ('var.relative') of each gene. Sorted by variance (decreasing)
#' @export
#'
#' @examples
buildGeneVarianceTable <- function(readcounts) {
  
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
#' @param gene.variances Gene variances table
#' @param top The top n of genes 1 <= n <= nrow(readcounts)
#'
#' @return Filtered readcount table
#' @export
#'
#' @examples
selectTopVariantGeneReadcounts <- function(readcounts, gene.variances, top) {
  
  if(!is.data.frame(readcounts) || !is.data.frame(gene.variances) || top <= 0) {
    return(NULL)
  }
  
  # Fetch the variances for all available genes
  # But only select those which are in the read count table
  variances <- gene.variances[rownames(readcounts),]
  
  topgeneids <- rownames(variances)[1:min(top, nrow(variances))]
  readcounts.geneids <- rownames(readcounts)
  
  indices <- na.omit(match(topgeneids, readcounts.geneids))
  
  result <- readcounts[indices,]
  
  return(result)
}

