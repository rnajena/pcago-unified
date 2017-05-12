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
library(biomaRt)
library(AnnotationHub)
source("classAnnotation.R")
source("classImporterEntry.R")
source("annotationGRanges.R")
source("annotationBioMart.R")
source("annotationHub.R")

supportedAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
supportedAnnotationImporters <- list(ImporterEntry(name = "gff_ensembl",
                                                   label = "Ensembl GFF (*.gff3)"),
                                     ImporterEntry(name = "pcago_csv",
                                                   label = "Tabular (*.csv)"))
supportedAnnotationGenerators <- list(annotationHub.importerEntry,
                                      bioMart.importerEntry)
                                      
availableAnnotationSamples <- list(ImporterEntry(name = "vitamins.gff3",
                                                 label = "Vitamins"))

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
  
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  # Load the data inside the GFF file
  # Suppress warning here: Will warn that connection is rewound. Didn't find a way to make it like the connection
  gr <- suppressWarnings(import.gff(filehandle)) 
  genes <- rownames(readcounts)
 
  annot.sequence.info <- GRanges.extractSequenceInfoAnnotation(gr, genes)
  annot.biotype <- GRanges.extractBiotypeAnnotation(gr, genes)
  
  annot <- mergeAnnotation(annot.sequence.info, annot.biotype)
  
  return(annot)
}

#' Imports the annotation from a table built by the annotation class
#'
#' @param filehandle 
#' @param readcounts 
#' @param sep 
#'
#' @return
#' @export
#'
#' @examples
importGeneInformationFromAnnotation.PCAGOTabular <- function(filehandle, readcounts, sep = ",") {
  
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
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
  
  if(missing(filehandle) || !is.character(datatype) || !is.SummarizedExperiment(readcounts)) {
    stop("Invalid arguments!")
  }
  
  if(datatype == "gff_ensembl") {
    return(importGeneInformationFromAnnotation.EnsemblGFF(filehandle, readcounts))
  }
  else {
    stop(paste("Unknown datatype", datatype))
  }
}

#' Generates an annotation
#'
#' @param generator 
#' @param parameters 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
generateGeneInformation <- function(generator, parameters, readcounts) {
  
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  
  if(generator == "ensembl_biomart") {
    return(generateGeneInformation.EnsemblBioMart(parameters$datatype, parameters$database, parameters$species, readcounts))
  }
  else if(generator == "annotation_hub") {
    return(generateGeneInformation.AnnotationHub(parameters$datatype, parameters$database, parameters$species, parameters$dataset, readcounts))
  }
  else {
    stop(paste("Unkown generator", generator))
  }
  
}

#' Imports a sample annotation file
#'
#' @param sample 
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
importSampleGeneInformation <- function(sample, readcounts) {
  
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  if(sample == "vitamins.gff3") {
    
    con <- file("sampledata/vitamins.gff3", "r")
    data <- importGeneInformationFromAnnotation(con, "gff_ensembl", readcounts)
    close(con)
    return(data)
    
  }
  else {
    stop(paste("Unknown sample", sample))
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
  
  if(!is.SummarizedExperiment(readcounts) || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  geneids <- rownames(readcounts)
  gene.var <- rowVars(assay(readcounts))
  
  variances.table <- data.frame(var = gene.var,
                          var.relative = gene.var / sum(gene.var),
                          row.names = geneids)
  
  # Sort by variance
  variances.table <- variances.table[order(variances.table$var, decreasing = T), ,drop = F]
  
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
  
  if(!is.SummarizedExperiment(readcounts) || !is.data.frame(gene.variances) || top <= 0) {
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

