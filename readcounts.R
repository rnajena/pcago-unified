#'
#' Contains methods that are related to read counts
#' 

library(shiny)
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
supportedReadcountNormalizationTypes <- c("None" = "none", "DeSeq2" = "deseq", "TPM" = "tpm")


#' Imports readcount from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedReadcountDataTypes
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
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)

  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Read count table is empty!")
  }
  if(!all(apply(data, 1, function(x) { is.numeric(x) }))) {
    stop("Read count table is not entirely numeric!")
  }
  
  return(data)
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
  data <- importReadcount(con, "csv")
  close(con)
  return(data)
  
}

#' Applies normalization to a read count table. The normalization algorithm is determined by normalizationtype
#'
#' @param rawdata Read count table
#' @param normalizationtype One of supportedReadcountNormalizationTypes
#'
#' @return Normalized read count table
#' @export
#'
#' @examples
applyReadcountNormalization <- function(rawdata, normalizationtype) {
  
  validate(need(rawdata, "No data to normalize!"))
  
  return(rawdata) # todo
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
  
  invalid <- (do.call(pmin, readcounts) == do.call(pmax, readcounts))
  readcounts.removed <- readcounts[which(!invalid),]
  genes.removed <- rownames(readcounts)[invalid]
  
  return(list(readcounts = readcounts.removed, genes.removed = genes.removed))
  
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
  
  return(data.frame(t(readcounts)))
  
}