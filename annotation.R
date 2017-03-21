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
supportedAnnotationImporters <- c("Ensembl GFF" = "gff_ensembl")
supportedAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
availableAnnotationSamples <- c("Vitamins" = "vitamins.gff3")

#' Extracts gene information from an Ensembl GFF file
#' See importGeneInformationFromAnnotation for more info
#'
#' @param filehandle 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
importGeneInformationFromAnnotation.EnsemblGFF <- function(filehandle, readcounts) {
  
  # Load the data inside the GFF file
  gr <- suppressWarnings(import.gff(filehandle)) # Supppress warning here: Will warn that connection is rewound. Didn't find a way to make it like the connection
  gff <- mcols(gr)
  
  # Build table containing the sequence, start, stop & length
  # Will fail if not every gene is explained
  meta.indices <- match(rownames(readcounts), gff$gene_id)
  
  if(any(is.na(meta.indices))) {
    stop("Annotation does not contain all genes!")
  }
  
  gene.meta <- gff[meta.indices,]
  sequence.info <- data.frame(row.names = rownames(readcounts),
                              sequence = as.vector(seqnames(gr)[meta.indices]),
                              start = start(gr)[meta.indices],
                              end = end(gr)[meta.indices],
                              length = width(gr)[meta.indices])
  
  # Extract features
  features <- list()
  
  for(feature_type in unique(gff$type)) {
    
    ids <- unlist(gff[gff$type == feature_type,"Parent"])
    gene_ids <- gff[match(ids, gff$ID),"gene_id"]
   
    if(length(gene_ids) > 0) {
      
      # Reduce to genes that are actually present in read counts
      gene_ids <- intersect(gene_ids, rownames(readcounts))
      
      features[[feature_type]] <- gene_ids 
    }
  }
  
  return(list(sequence.info = sequence.info, features = features))
}

#' Extracts gene information from an annotation
#'
#' @param filehandle 
#' @param datatype 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
importGeneInformationFromAnnotation <- function(filehandle, datatype, readcounts) {
  
  if(datatype == "gff_ensembl") {
    return(importGeneInformationFromAnnotation.EnsemblGFF(filehandle, readcounts))
  }
  else {
    return(NULL)
  }
  
}

importSampleGeneInformationFromAnnotation <- function(sample, readcounts) {
  
  if(sample == "vitamins.gff3") {
    
    con <- file("sampledata/vitamins.gff3", "r")
    data <- importGeneInformationFromAnnotation(con, "gff_ensembl", readcounts)
    close(con)
    return(data)
    
  }
  else {
    return(NULL)
  }
  
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

