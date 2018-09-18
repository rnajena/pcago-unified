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
source("classGeneAnnotation.R")
source("classImporterEntry.R")
source("geneAnnotationGRanges.R")
source("geneAnnotationBioMart.R")
source("geneAnnotationHub.R")

geneAnnotationImporterParameter.imported_data.pcago_csv <- ImporterParameter(name = "imported_data",
                                                                             label = "Imported data",
                                                                             type = "checkboxes",
                                                                             checkboxes.options = GeneAnnotationEntryNames,
                                                                             checkboxes.selected = GeneAnnotationEntryNames)

GeneAnnotationEntryNames.gff_ensembl <- c("Scaffold" = "scaffold",
                                          "Start position" = "start_position",
                                          "End position" = "end_position",
                                          "Length" =  "length",
                                          "Exon length" =  "exon_length",
                                          "Biotype" =  "biotype")

geneAnnotationImporterParameter.imported_data.gff_ensembl <- ImporterParameter(name = "imported_data",
                                                                             label = "Imported data",
                                                                             type = "checkboxes",
                                                                             checkboxes.options = GeneAnnotationEntryNames.gff_ensembl,
                                                                             checkboxes.selected = GeneAnnotationEntryNames.gff_ensembl)

supportedGeneAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
supportedGeneAnnotationImporters <- list(ImporterEntry(name = "gff_ensembl",
                                                   label = "Ensembl GFF (*.gff3)",
                                                   parameters = list(geneAnnotationImporterParameter.imported_data.gff_ensembl)),
                                     ImporterEntry(name = "pcago_csv",
                                                   label = "PCAGO gene annotation table (*.csv)",
                                                   parameters = list(ImporterParameter.csv, 
                                                                     geneAnnotationImporterParameter.imported_data.pcago_csv)))
supportedGeneAnnotationGenerators <- list(annotationHub.importerEntry,
                                      bioMart.importerEntry)
                                      
availableGeneAnnotationSamples <- list(ImporterEntry(name = "Monocytes/geneannotation.csv",
                                                 label = "Monocytes (PCAGO annotation table CSV)",
                                                 parameters = list(geneAnnotationImporterParameter.imported_data.pcago_csv)),
                                   ImporterEntry(name = "Monocytes/geneannotation.gff3",
                                                 label = "Monocytes (Ensembl GFF)",
                                                 parameters = list(geneAnnotationImporterParameter.imported_data.gff_ensembl)))

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
importGeneInformationFromAnnotation.EnsemblGFF <- function(filehandle, dataset, parameters) {
  
  readcounts <- dataset$readcounts.preprocessed
  
  if(is.null(readcounts)) {
    stop("No read counts loaded!")
  }
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  imported_data <- parameters$imported_data
  
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  # Load the data inside the GFF file
  # Suppress warning here: Will warn that connection is rewound. Didn't find a way to make it like the connection
  gr <- suppressWarnings(import.gff(filehandle)) 
  genes <- rownames(readcounts)
  
  # Remove "Name" annotation as this disturbs getting the exon length (Name is preferred over gene_id)
  mcols(gr)$Name <- NULL
 
  annot.sequence.info <- GRanges.extractSequenceInfoAnnotation(gr, genes, imported_data)
  annot.biotype <- if("biotype" %in% imported_data) GRanges.extractBiotypeAnnotation(gr, genes) else GeneAnnotation()
  annot.scaffold <- if("scaffold" %in% imported_data) GRanges.extractScaffoldAnnotation(gr, genes) else GeneAnnotation()
  
  annot <- mergeGeneAnnotation(annot.sequence.info, annot.biotype)
  annot <- mergeGeneAnnotation(annot, annot.scaffold)
  
  annot <- geneAnnotationRestrictContentTypes(annot, imported_data)
  
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
importGeneInformationFromAnnotation.PCAGOTabular <- function(filehandle, dataset, parameters) {
  
  readcounts <- dataset$readcounts.preprocessed
  
  if(is.null(readcounts)) {
    stop("No read counts loaded!")
  }
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  imported_data <- parameters$imported_data
  
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  sep <- parameters$separator
  
  data <- read.csv(filehandle, header = T, row.names = 1, sep = sep, stringsAsFactors = F)
  
  # Restrict to genes and columns we want
  data <- data[intersect(rownames(data), rownames(readcounts)), imported_data, drop = F]
  
  annot <- geneAnnotationFromTable(data)
  
  return(annot)
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
importGeneInformationFromAnnotation <- function(filehandle, datatype, dataset, parameters) {
  
  readcounts <- dataset$readcounts.preprocessed
  
  if(is.null(readcounts)) {
    stop("No read counts loaded!")
  }
  if(missing(filehandle) || !is.character(datatype) || !is.SummarizedExperiment(readcounts)) {
    stop("Invalid arguments!")
  }
  
  if(datatype == "gff_ensembl") {
    return(importGeneInformationFromAnnotation.EnsemblGFF(filehandle, dataset, parameters))
  }
  if(datatype == "pcago_csv") {
    return(importGeneInformationFromAnnotation.PCAGOTabular(filehandle, dataset, parameters))
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
generateGeneInformation <- function(generator, dataset, parameters) {
  
  readcounts <- dataset$readcounts.preprocessed
  
  if(is.null(readcounts)) {
    stop("No read counts loaded!")
  }
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  
  if(generator == "ensembl_biomart") {
    return(generateGeneInformation.EnsemblBioMart(parameters$database, parameters$species, parameters$imported_data, dataset))
  }
  else if(generator == "annotation_hub") {
    return(generateGeneInformation.AnnotationHub(parameters$database, parameters$species, parameters$dataset, parameters$imported_data, dataset))
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
importSampleGeneInformation <- function(sample, dataset, parameters) {
  
  readcounts <- dataset$readcounts.preprocessed
  
  if(is.null(readcounts)) {
    stop("No read counts loaded!")
  }
  if(!is.SummarizedExperiment(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({
    close(con)
  })
  
  if(sample == "Monocytes/geneannotation.gff3") {
    data <- importGeneInformationFromAnnotation(con, "gff_ensembl", dataset, parameters)
    return(data)
  }
  else if(sample == "Monocytes/geneannotation.csv") {
    parameters$separator <- ","
    data <- importGeneInformationFromAnnotation(con, "pcago_csv", dataset, parameters)
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
#' @param gene.variances Gene variances table (this table is ordered by gene variance decreasing)
#' @param top The top n of genes 1 <= n <= nrow(readcounts)
#'
#' @return Filtered readcount table
#' @export
#'
#' @examples
selectTopVariantGeneReadcounts <- function(readcounts, gene.variances, top) {
  
  if(!is.SummarizedExperiment(readcounts) || !is.data.frame(gene.variances) || nrow(readcounts) == 0 || top <= 0) {
    return(NULL)
  }
  
  available.rows <- rownames(readcounts)
  requested.rows <- rownames(gene.variances)[1:min(top, nrow(gene.variances))]
  requested.rows <- intersect(available.rows, requested.rows)
  
  if(length(requested.rows) == 0) {
    return(NULL)
  }
 
  return( readcounts[requested.rows,] ) 
 
}

