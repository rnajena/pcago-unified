#'
#' Contains methods that are related to read counts
#' 

library(shiny)
library(SummarizedExperiment)
library(DESeq2)
source("classImporterEntry.R")
source("classDataSet.R")
source("helpers.R")

#' A list of all read count data types that will be supported
#' The user selects one of those types, which will then invoke the corresponding importer

supportedReadcountImporters <- list(
  ImporterEntry(name = "csv", label = "Read count table (*.csv)", parameters = list(ImporterParameter.csv,
                                                                                    ImporterParameter.csv.comment,
                                                                                    ImporterParameter.csv.selectrows,
                                                                                    ImporterParameter.csv.selectcolumns))
)

supportedReadcountGenerators <- list()

availableReadcountSamples <- list(
  ImporterEntry(name = "Monocytes/readcounts_rna.csv", label = "Monocytes (Raw)"),
  ImporterEntry(name = "Monocytes/readcounts_normalized.csv", label = "Monocytes (Normalized)"),
  ImporterEntry(name = "Monocytes/readcounts_normalized_diffexpressed.csv", label = "Monocytes (DEG, Normalized)"),
  ImporterEntry(name = "Mouse/readcounts_rna.csv", label = "Mouse (Raw)"),
  ImporterEntry(name = "Mouse/readcounts_normalized.csv", label = "Mouse (Normalized)"),
  ImporterEntry(name = "Myotis RNA/readcounts_rna.csv", label = "Myotis RNA (Raw)"),
  ImporterEntry(name = "Myotis RNA/readcounts_normalized_rna.csv", label = "Myotis RNA (Normalized)"),
  ImporterEntry(name = "Myotis smallRNA/readcounts_smallrna.csv", label = "Myotis smallRNA (Raw)"),
  ImporterEntry(name = "Myotis smallRNA/readcounts_normalized_smallrna.csv", label = "Myotis smallRNA (Normalized)")
)

#' Supported read count normalization types
supportedReadcountNormalizationTypes <- c("None" = "none", "DESeq2" = "deseq2", "TPM" = "tpm")


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
importReadcount <- function(filehandle, importer, parameters) {
  
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  sep <- parameters$separator
  selected.rows <- parse.selectIntegers(parameters$selected.rows)
  selected.columns <- parse.selectIntegers(parameters$selected.columns)
  comment.char <- if(parameters$comment.char == "none") "" else parameters$comment.char
 
  if(!is.null(selected.rows) && is.na(selected.rows)) {
    stop("Invalid row selection parameter!")
  }
  if(!is.null(selected.columns) && is.na(selected.columns)) {
    stop("Invalid column selection parameter!")
  }
  
  data <- read.delim(filehandle, sep = sep, row.names = 1, stringsAsFactors = F, check.names = F, comment.char = comment.char)

  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Read count table is empty!")
  }
  
  # Restrict
  if(!is.null(selected.rows)) {
    selected.rows <- selected.rows[selected.rows > 0 & selected.rows <= nrow(data)]
    data <- data[selected.rows, ]
  }
  if(!is.null(selected.columns)) {
    selected.columns <- selected.columns[selected.columns > 0 & selected.columns <= ncol(data)]
    data <- data[, selected.columns]
  }
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Read count table is empty!")
  }
  
  # Create matrix
  counts <- as.matrix(data)
  
  if(!all(is.numeric(counts))) {
    stop("Read counts must be numeric!")
  }
  
  experiment <- SummarizedExperiment(assays = list(counts = counts))
  
  dataset <- PCAGODataSet$new()
  dataset$readcounts.raw <- experiment
  
  return(dataset)
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
  
  parameters$separator <- ","
  parameters$comment.char <- "none"
  parameters$selected.rows <- ""
  parameters$selected.columns <- ""
  data <- importReadcount(con, "csv", parameters)
  
  return(data)
  
}

#' Applies read count normalization (DeSeq2) to readcounts
#'
#' @param readcounts 
#' @param normalize Enable normalization (default true)
#' @param transform Can be 'none' or 'rlog' (default none)
#' @param betaPrior Set betaPrior in DESeq2 params (default false)
#' @param condition.table Conditions table that associates each sample to the conditions it has
#' @param selectedn.conditions Vector of conditions that should be used for normalization
#'
#' @return
#' @export
#'
#' @examples
applyReadcountNormalization.DESeq2 <- function(readcounts, transform = "none", betaPrior = F, sample.annotation, selected.conditions) {
  
  if(!is.SummarizedExperiment(readcounts) || !is(sample.annotation, "SampleAnnotation") || !is.character(selected.conditions)) {
    stop("Invalid arguments!")
  }
  if(!(transform %in% c("none", "rlog"))) {
    stop("Unsupported transformation!")
  }
  
  validate(need(nrow(readcounts) > 0 && ncol(readcounts) > 0, "No read counts to process!"),
           need(sampleAnnotationHasConditions(sample.annotation), "Sample annotation has no condition table!"),
           need(is.integer(assay(readcounts)), "Read counts need to be integers!"))
  
  # progress <- progressNotification("Building DESeq2 data. This will take some time ...")
  # on.exit({
  #   removeNotification(progress)
  # })
  
  condition.table <- sample.annotation@conditions
  
  # Deseq expects that we assign a condition to each sample
  # But we store a boolean condition array. Collapse it into strings.
  collapsed.conditions <- collapseConditions(condition.table, selected.conditions)
  
  deseq.coldata <- data.frame(row.names = names(collapsed.conditions), condition = collapsed.conditions)
  
  deseq.dataset <- DESeqDataSetFromMatrix(countData = assay(readcounts),
                                          colData = deseq.coldata,
                                          design = ~ condition)
  
  deseq.obj <- DESeq(deseq.dataset, betaPrior = betaPrior)
  
  if(transform == "none") {
    assay(readcounts) <- counts(deseq.obj, normalized = T)
  }
  else if(transform == "rlog") {
    assay(readcounts) <- assay(rlog(deseq.obj))
  }
  else {
    stop("Invalid transformation!")
  }
  
  # Return the readcounts and the conditions used for normalization
  return(list(readcounts = readcounts, 
              conditions = deseq.coldata, 
              design = "~condition", 
              operation.normalization = "deseq2", 
              deseq2.transformation = transform))
}

#' Applies read count normalization (TPM) to readcounts
#'
#' @param readcounts Read count data
#' @param use.feature.effectivelength Calculate the effective length instead of using the feature length (preferred)
#' @param use.feature.exonlength Use the exon length of a feature instead of the feature length (preferred)
#' @param sample.annotation Annotation of samples
#' @param gene.annotation Annotation of genes
#'
#' @return
#' @export
#'
#' @examples
applyReadcountNormalization.TPM <- function(readcounts,
                                            sample.annotation, 
                                            gene.annotation, 
                                            use.feature.exonlength = T, 
                                            use.feature.effectivelength = T) {
  
  if(!is.SummarizedExperiment(readcounts) ||
     !is.logical(use.feature.effectivelength) ||
     !is.logical(use.feature.exonlength) || 
     !is(gene.annotation, "GeneAnnotation")) {
    stop("Invalid arguments!")
  }
  
  validate(need(nrow(readcounts) > 0 && ncol(readcounts) > 0, "[TPM] No read counts to process!"),
           need(geneAnnotationHasSequenceInfo(gene.annotation), "[TPM] No sequence info available!"),
           need(!use.feature.effectivelength || sampleAnnotationHasSampleInfo(sample.annotation), "[TPM] No sample info available!"),
           need(is.integer(assay(readcounts)), "[TPM] Read counts need to be integers!"))
  
   counts <- assay(readcounts)
  
  # Fetch feature information from annotation
  genes <- rownames(readcounts)
  feature.lengths <- if(use.feature.exonlength) gene.annotation@sequence.info[genes, "exon_length"] else gene.annotation@sequence.info[genes, "length"]
  
  validate(need(all(!is.na(feature.lengths)), paste("[TPM] Missing feature length annotations: ", paste(genes[is.na(feature.lengths)], collapse = ", "))),
           need(!use.feature.effectivelength || all(!is.na(sample.annotation@sample.info$meanfragmentlength)), "[TPM] All samples need a mean fragment length annotation!"))
  
  # Go through each sample
  for(i in seq_len(ncol(counts))) {
    
    sample <- colnames(readcounts)[i]
    
    # First, we need to calculate the effective length of each feature
    # It depends on the mean fragment length of that sample and the feature length.
    # The feature length on the other hand cannot be used directly as our reads are only generated by exons
    # thus we use the exon length for this calculation (which can be disabled)
    # The user can disable calculating the effective length. Then it will just use the feature length
    
    feature.effectivelength <- NA
    
    if(use.feature.effectivelength) {
      
      mean.fragmentlength <- sample.annotation@sample.info[sample, "meanfragmentlength"]
      
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
  
  # Make some additional statistics
  sample.sum <- colSums(assay(readcounts))
  names(sample.sum) <- colnames(readcounts)
  
  # Output the assay and parameters
  return(list(readcounts = readcounts, 
              sample.sum = sample.sum, 
              use.feature.effectivelength = use.feature.effectivelength, 
              use.feature.exonlength = use.feature.exonlength, 
              operation.normalization = "tpm"))
  
}

#' Removes zero read count genes from the table.
#' #'
#' @param readcounts 
#'
#' @return list of readcounts without constant entries (readcounts) and list of removed genes (genes.removed)
#' @export
#'
#' @examples
removeZeroReads <- function(readcounts) {
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  counts <- assay(readcounts)
  invalid <- rowMins(counts) == 0 & rowMaxs(counts) == rowMins(counts)
  
  genes.removed <- rownames(readcounts)[invalid]
  readcounts <- readcounts[which(!invalid),]
  
  return(list(readcounts = readcounts, genes.removed = genes.removed))
  
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
  invalid <- rowVars(counts) == 0
  
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