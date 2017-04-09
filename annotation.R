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
source("classAnnotation.R")
source("classImporterEntry.R")
source("biomart.R")

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer

bioMart.data.sets <- reactive(getBioMartDatsets()) # Make reactive to increase load performance

test.select2.values <- function(first.data) {
  return(sapply(1:5, function(x) { paste0(first.data, x) }))
}

supportedAnnotationFileTypes <- c("text/plain", ".gff", ".gff3")
supportedAnnotationImporters <- list(ImporterEntry(name = "gff_ensembl",
                                                   label = "Ensembl GFF"))
supportedAnnotationGenerators <- list(
                                      ImporterEntry(name = "test",
                                                    label = "Test",
                                                    parameters = list(
                                                      ImporterParameter(name = "param1",
                                                                        label = "First parameter",
                                                                        type = "select",
                                                                        select.values = c("A", "B", "C")),
                                                      ImporterParameter(name = "param2",
                                                                        label = "Second parameter",
                                                                        type = "select",
                                                                        select.values = test.select2.values)
                                                    )),
                                      ImporterEntry(name = "ensembl_go",
                                                    label = "Ensembl BioMart GO terms",
                                                    parameters = list(
                                                      ImporterParameter(name = "dataset", 
                                                                        label = "Dataset", 
                                                                        type = "select", 
                                                                        select.values = bioMart.data.sets) 
                                                    )),
                                      ImporterEntry(name = "ensembl_sequence_info",
                                                    label = "Ensembl BioMart sequence info",
                                                    parameters = list(
                                                      ImporterParameter(name = "dataset", 
                                                                        label = "Dataset", 
                                                                        type = "select", 
                                                                        select.values = bioMart.data.sets) 
                                                    ))
                                      )
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
  
  if(!is.data.frame(readcounts)) {
    stop("No readcounts to annotate!")
  }
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  # Load the data inside the GFF file
  gr <- suppressWarnings(import.gff(filehandle)) # Suppress warning here: Will warn that connection is rewound. Didn't find a way to make it like the connection
  gff <- mcols(gr)
  
  # Build table containing the sequence, start, stop & length
  
  genes <- rownames(readcounts)
  meta.indices <- match(genes, gff$gene_id) # F: Index of gene in readcounts -> Index of gene in gff annotation
  
  if(any(is.na(meta.indices))) {
    
    # Remove genes that are NA. They will not appear in the annotation
    na.genes <- genes[is.na(meta.indices)]
    genes <- genes[!is.na(meta.indices)]
    meta.indices <- na.omit(meta.indices)
    
    showNotification(type = "warning", 
                     duration = NULL,
                     paste("Could not find sequence information for all genes. Following genes are affected:", 
                           strJoinLimited(na.genes, limit = 10)))
    
  }
  
  sequence.info <- data.frame(row.names = genes,
                              scaffold = as.vector(seqnames(gr)[meta.indices]),
                              start_position = start(gr)[meta.indices],
                              end_position = end(gr)[meta.indices],
                              length = width(gr)[meta.indices],
                              stringsAsFactors = F)
  
  # Extract features
  features <- list()
  
  for(feature_type in unique(gff$type)) {
    
    ids <- unlist(gff[gff$type == feature_type,"Parent"])
    gene_ids <- gff[match(ids, gff$ID),"gene_id"]
    gene_ids <- intersect(gene_ids, rownames(readcounts)) # Reduce to genes that are actually present in read counts
   
    if(length(gene_ids) > 0) {
      features[[feature_type]] <- gene_ids 
    }
  }
  
  # As we have sequence info, extract scaffold filter
  scaffolds <- list()
  for(scaffold in unique(sequence.info$scaffold)) {
    scaffolds[[scaffold]] <- rownames(sequence.info)[sequence.info$scaffold == scaffold]
  }
  
  return(Annotation(sequence.info = sequence.info, 
                    gene.features = GeneFilter(data = features),
                    gene.scaffold = GeneFilter(data = scaffolds)))
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
  
  if(missing(filehandle) || !is.character(datatype) || !is.data.frame(readcounts)) {
    stop("Invalid arguments!")
  }
  
  if(datatype == "gff_ensembl") {
    return(importGeneInformationFromAnnotation.EnsemblGFF(filehandle, readcounts))
  }
  else {
    stop(paste("Unknown datatype", datatype))
  }
}

#' Builds an annotation of GO terms by searching the genes in Ensembl dataset
#'
#' @param dataset.string <BIOMART>@<DATASET> string
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
generateGeneInformation.EnsemblGO <- function(dataset.string, readcounts) {
  
  genes <- rownames(readcounts)
  bio.mart <- getBioMartDataset(dataset.string)
  go.terms.table <- getBioMartGOTerms(bio.mart, genes)
  
  if(is.null(go.terms.table)) {
    stop("Data set does not contain even one of the requested genes! Did you choose a wrong data set?")
  }
  
  # We have to transform the table with columns gene_id, term to term -> list of gene ids
  go.terms.filter <- list()
  
  for(term in unique(go.terms.table$go_term)) {
    
    if(term == "") {
      next()
    }
    
    gene.indices <- term == go.terms.table$go_term
    genes <- go.terms.table$ensembl_gene_id[gene.indices]
    
    go.terms.filter[[term]] <- genes
    
  }
  
  return(Annotation(gene.go.terms = GeneFilter(data = go.terms.filter)))
  
}

#' Builds an annotation of sequence info by searching the genes in Ensembl dataset
#'
#' @param dataset.string <BIOMART>@<DATASET> string
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
generateGeneInformation.EnsemblSequenceInfo <- function(dataset.string, readcounts) {
  
  genes <- rownames(readcounts)
  bio.mart <- getBioMartDataset(dataset.string)
  sequence.info <- getBioMartSequenceInfo(bio.mart, genes)
  
  if(is.null(sequence.info)) {
    stop("Data set does not contain even one of the requested genes! Did you choose a wrong data set?")
  }
  
  # Extract scaffold filter
  scaffolds <- list()
  for(scaffold in unique(sequence.info$scaffold)) {
    scaffolds[[scaffold]] <- rownames(sequence.info)[sequence.info$scaffold == scaffold]
  }
  
  return(Annotation(sequence.info = sequence.info,
                    gene.scaffold = GeneFilter(data = scaffolds)))
  
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
  
  if(!is.data.frame(readcounts)) {
    stop("No readcounts to annotate!")
  }
  
  if(generator == "ensembl_go") {
    return(generateGeneInformation.EnsemblGO(parameters$dataset, readcounts))
  }
  else if(generator == "ensembl_sequence_info") {
    return(generateGeneInformation.EnsemblSequenceInfo(parameters$dataset, readcounts))
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
  
  if(!is.data.frame(readcounts)) {
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
  
  if(is.null(readcounts) || ncol(readcounts) < 2 || nrow(readcounts) == 0) {
    return(NULL)
  }
  
  geneids <- rownames(readcounts)
  gene.var <- rowVars(data.matrix(readcounts))
  
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

