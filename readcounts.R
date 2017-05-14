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
applyReadcountNormalization.DESeq2 <- function(readcounts, condition.table, selected.conditions) {
  
  if(!is.SummarizedExperiment(readcounts)) {
    stop("Invalid arguments!")
  }
  
  progress <- progressNotification("Building DESeq2 data. This will take some time ...")
  on.exit({
    removeNotification(progress)
  })
  
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
applyReadcountNormalization.TPM <- function(readcounts,cell.annotation, gene.annotation, use.feature.exonlength = T, use.fragment.effectivelength, T) {
  
  if(!is.SummarizedExperiment(readcounts) || !is.logical(use.effective.length) || !is.logical(use.feature.exonlength) || !is.data.frame(sequence.info)) {
    stop("Invalid arguments!")
  }
  
  validate(need(geneAnnotationHasSequenceInfo(gene.annotation), "No sequence info available!"),
           need(readcounts, "No read counts to process!"),
           need(all(rownames(readcounts) %in% rownames(sequence.info)), "Not all genes annotated!"))
  
  # First, we need to calculate the effective length of each feature
  # It depends on the mean fragment length of that sample and the feature length.
  # The feature length on the other hand cannot be used directly as our reads are only generated by exons
  # thus we use the exon length for this calculation (which can be disabled)
  # The user can disable calcululating the effective length. Then it will just use the feature length
  
  sequence.info <- gene.annotation@sequence.info[rownames(readcounts),]
  sequence.info$effective.length <- sequence.info$length - mu.fld + 1
  
  counts <- assay(readcounts)
  
  
  # Normalize sequence length
  counts.rpk <- sweep(counts, 1, sequence.info$effective.length, "/")
  
  # Normalize by sequence depth
  counts.tpm <- sweep(counts.rpk, 2, 10e6 / colSums(counts.rpk), "*")
  
  assay(readcounts) <- counts.tpm
  
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