#'
#' Contains methods that are related to read counts
#' 

library(shiny)
library(SummarizedExperiment)
library(DESeq2)
source("classImporterEntry.R")
source("helpers.R")

#' A list of all read count data types that will be supported
#' The user selects one of those types, which will then invoke the corresponding importer

supportedReadcountImporters <- list(
  ImporterEntry(name = "csv", label = "CSV read count table (*.csv)", parameters = list(ImporterParameter.csv))
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
importReadcount <- function(filehandle, parameters) {
  
  if(missing(filehandle)) {
    stop("Invalid arguments!")
  }
  
  sep <- parameters$separator
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F, check.names = F)

  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Read count table is empty!")
  }
  
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
  
  parameters$separator <- ","
  data <- importReadcount(con, parameters)
  
  return(data)
  
}

#' Applies read count normalization (DeSeq2) to readcounts
#'
#' @param readcounts 
#' @param condition.table Conditions table that associates each sample to the conditions it has
#' @param selectedn.conditions Vector of conditions that should be used for normalization
#'
#' @return
#' @export
#'
#' @examples
applyReadcountNormalization.DESeq2 <- function(readcounts, sample.annotation, selected.conditions) {
  
  if(!is.SummarizedExperiment(readcounts) || !is(sample.annotation, "SampleAnnotation") || !is.character(selected.conditions)) {
    stop("Invalid arguments!")
  }
  
  validate(need(nrow(readcounts) > 0 && ncol(readcounts) > 0, "No read counts to process!"),
           need(sampleAnnotationHasConditions(sample.annotation), "Sample annotation has no condition table!"),
           need(is.integer(assay(readcounts)), "Read counts need to be integers!"))
  
  progress <- progressNotification("Building DESeq2 data. This will take some time ...")
  on.exit({
    removeNotification(progress)
  })
  
  condition.table <- sample.annotation@conditions
  
  # Deseq expects that we assign a condition to each sample
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
  return(list(readcounts = readcounts, conditions = deseq.coldata, design = "~condition"))
}

#' Applies read count normalization (TPM) to readcounts
#'
#' @param readcounts Read count data
#' @param use.fragment.effectivelength Calculate the effective length instead of using the feature length (preferred)
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
                                            use.fragment.effectivelength = T) {
  
  if(!is.SummarizedExperiment(readcounts) ||
     !is.logical(use.fragment.effectivelength) ||
     !is.logical(use.feature.exonlength) || 
     !is(gene.annotation, "GeneAnnotation")) {
    stop("Invalid arguments!")
  }
  
  validate(need(nrow(readcounts) > 0 && ncol(readcounts) > 0, "No read counts to process!"),
           need(geneAnnotationHasSequenceInfo(gene.annotation), "No sequence info available!"),
           need(!use.fragment.effectivelength || sampleAnnotationHasSampleInfo(sample.annotation), "No sample info available!"),
           need(is.integer(assay(readcounts)), "Read counts need to be integers!"))
  
   counts <- assay(readcounts)
  
  # Fetch feature information from annotation
  genes <- rownames(readcounts)
  feature.lengths <- if(use.feature.exonlength) gene.annotation@sequence.info[genes, "exon_length"] else gene.annotation@sequence.info[genes, "length"]
  
  validate(need(all(!is.na(feature.lengths)), paste("Missing feature length annotations: ", paste(genes[is.na(feature.lengths)], collapse = ", "))),
           need(!use.fragment.effectivelength || all(!is.na(sample.annotation@sample.info$meanfragmentlength)), "All samples need a mean fragment length annotation!"))
  
  # Go through each sample
  for(i in seq_len(ncol(counts))) {
    
    sample <- colnames(readcounts)[i]
    
    # First, we need to calculate the effective length of each feature
    # It depends on the mean fragment length of that sample and the feature length.
    # The feature length on the other hand cannot be used directly as our reads are only generated by exons
    # thus we use the exon length for this calculation (which can be disabled)
    # The user can disable calculating the effective length. Then it will just use the feature length
    
    feature.effectivelength <- NA
    
    if(use.fragment.effectivelength) {
      
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
              use.fragment.effectivelength = use.fragment.effectivelength, 
              use.feature.exonlength = use.feature.exonlength))
  
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