#'
#' Contains methods that are related to read counts
#' 

library(shiny)
library(SummarizedExperiment)
library(DESeq2)
source("classImporterEntry.R")

#' A list of all read count data types that will be supported
#' The user selects one of those types, which will then invoke the corresponding importer

supportedReadcountImporters <- list(
  ImporterEntry(name = "csv", label = "CSV"),
  ImporterEntry(name = "tsv", label = "TSV")
)

supportedReadcountGenerators <- list()

availableReadcountSamples <- list(
  ImporterEntry(name = "vitamins.debug.csv", label = "Vitamins (debug)"),
  ImporterEntry(name = "vitamins.small.csv", label = "Vitamins (small)"),
  ImporterEntry(name = "vitamins.csv", label = "Vitamins")
)

#' Supported read count normalization types
supportedReadcountNormalizationTypes <- c("None" = "none", "DeSeq2" = "deseq2", "TPM" = "tpm")


#' Imports readcount from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedReadcountDataTypes
#' @param parameters Additional parameters from the importer. Not used.
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importReadcount <- function(filehandle, datatype, parameters) {
  
  if(missing(filehandle) || !is.character(datatype)) {
    stop("Invalid arguments!")
  }
  
  sep <- ","
  
  if(datatype == "tsv") {
    sep <- ""
  }
  else if(datatype == "csv") {
    sep <- ","
  }
  else {
    stop(paste("Unsupported format", datatype))
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F, check.names = F)

  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Read count table is empty!")
  }
  
  # if(!all(apply(data, 1, function(x) { is.numeric(x) }))) {
  #   stop("Read count table is not entirely numeric!")
  # }
  
  counts <- as.matrix(data)
  experiment <- SummarizedExperiment(assays = list(counts = counts))
  
  return(experiment)
}

#' Imports sample with given sample id
#'
#' @param sample 
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importReadcountSample <- function(sample, parameters) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  data <- importReadcount(con, "csv")
  
  return(data)
  
}

#' Applies read count normalization (DeSeq2) to readcounts
#'
#' @param readcounts 
#' @param condition.table Conditions table that associates each cell to the conditions it has
#' @param selectedn.conditions Vector of conditions that should be used for normalization
#'
#' @return
#' @export
#'
#' @examples
applyReadcountNormalization.DESeq2 <- function(readcounts, cell.annotation, selected.conditions) {
  
  if(!is.SummarizedExperiment(readcounts) || !is(cell.annotation, "CellAnnotation") || !is.character(selected.conditions)) {
    stop("Invalid arguments!")
  }
  if(!cellAnnotationHasConditions(cell.annotation)) {
    stop("Cell annotation has no condition table!")
  }
  if(!is.integer(assay(readcounts))) {
    stop("Read counts need to be integers!")
  }
  
  progress <- progressNotification("Building DESeq2 data. This will take some time ...")
  on.exit({
    removeNotification(progress)
  })
  
  condition.table <- cell.annotation@conditions
  
  # Deseq expects that we assign a condition to each cell
  # But we store a boolean condition array. Collapse it into strings.
  collapsed.conditions <- collapseConditions(condition.table, selected.conditions)
  
  deseq.coldata <- data.frame(row.names = names(collapsed.conditions), condition = collapsed.conditions)
  
  deseq.dataset <- DESeqDataSetFromMatrix(countData = assay(readcounts),
                                          colData = deseq.coldata,
                                          design = ~ condition)
  
  deseq.obj <- DESeq(deseq.dataset)
  normalized.counts <- counts(deseq.obj, normalized = T)
  
  assay(readcounts) <- normalized.counts
  
  # Return the readcounts and the conditions used for normalization
  return(list(readcounts = readcounts, conditions = deseq.coldata))
}

#' Applies read count normalization (TPM) to readcounts
#'
#' @param readcounts Read count data
#' @param use.fragment.effectivelength Calculate the effective length instead of using the feature length (preferred)
#' @param use.feature.exonlength Use the exon length of a feature instead of the feature length (preferred)
#' @param cell.annotation Annotation of cells
#' @param gene.annotation Annotation of genes
#'
#' @return
#' @export
#'
#' @examples
applyReadcountNormalization.TPM <- function(readcounts,
                                            cell.annotation, 
                                            gene.annotation, 
                                            use.feature.exonlength = T, 
                                            use.fragment.effectivelength = T) {
  
  if(!is.SummarizedExperiment(readcounts) ||
     !is.logical(use.fragment.effectivelength) ||
     !is.logical(use.feature.exonlength) || 
     !is(gene.annotation, "GeneAnnotation")) {
    stop("Invalid arguments!")
  }
  if(nrow(readcounts) == 0 || ncol(readcounts) == 0) {
    stop("No read counts to process!")
  }
  # if(!is.integer(assay(readcounts))) {
  #   stop("Read counts need to be integers!")
  # }
  if(!geneAnnotationHasSequenceInfo(gene.annotation)) {
    stop("No sequence info available!")
  }
  if(use.fragment.effectivelength && !cellAnnotationHasCellInfo(cell.annotation)) {
    stop("No cell info available!")
  }
  
  counts <- assay(readcounts)
  
  # Fetch feature information from annotation
  genes <- rownames(readcounts)
  feature.lengths <- if(use.feature.exonlength) gene.annotation@sequence.info[genes, "exonlength"] else gene.annotation@sequence.info[genes, "length"]
  
  if(any(is.na(feature.lengths)) || any(!is.numeric(feature.lengths))) {
    stop("Not all features have an annotation!")
  }
  
  # Go through each sample
  for(i in seq_len(ncol(counts))) {
    
    cell <- colnames(readcounts)[i]
    
    # First, we need to calculate the effective length of each feature
    # It depends on the mean fragment length of that sample and the feature length.
    # The feature length on the other hand cannot be used directly as our reads are only generated by exons
    # thus we use the exon length for this calculation (which can be disabled)
    # The user can disable calculating the effective length. Then it will just use the feature length
    
    feature.effectivelength <- NA
    
    if(use.fragment.effectivelength) {
      
      mean.fragmentlength <- cell.annotation@cell.info[cell, "meanfragmentlength"]
      
      if(is.na(mean.fragmentlength) || !is.numeric(mean.fragmentlength)) {
        stop("All cells need a mean fragment length annotation!")
      }
      
      feature.effectivelength <- feature.lengths - mean.fragmentlength + 1
    } else {
      feature.effectivelength <- feature.lengths
    }
    
    # Calculate TPM
    
    sample.counts <- counts[,i]
    
    # First step: divide by effective length (= transcript length normalization) and summarize it (for the second step)
    sample.counts.rpk <- sample.counts / feature.effectivelength
    sum.sample.counts.rpk <- sum(sample.counts.rpk)
    
    # Second step: divide RPK by sum of RPK () and multiply with factor 10e6
    sample.counts.tpm <- sample.counts.rpk * (10e6 / sum.sample.counts.rpk)
    
    counts[,i] <- sample.counts.tpm
  }
  
  assay(readcounts) <- counts
  
  # Output the assay and parameters
  return(list(readcounts = readcounts))
  
}

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
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  counts <- assay(readcounts)
  invalid <- (apply(counts, 1, min) == apply(counts, 1, max))
  
  genes.removed <- rownames(readcounts)[invalid]
  readcounts <- readcounts[which(!invalid),]
  
  return(list(readcounts = readcounts, genes.removed = genes.removed))
  
}

#' Transposes the read count table
#'
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
transposeReadCounts <- function(readcounts) {
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  counts <- t(assay(readcounts))
  return(SummarizedExperiment(assays = list(counts = counts)))
  
}