#'
#' Class that holds the whole data set
#' 

library(SummarizedExperiment)
source("classGeneAnnotation.R")

PCAGODataSet <- setClass(
  "PCAGODataSet",
  slots = signature(
    readcounts.raw = "SummarizedExperiment",
    readcounts.preprocessed = "SummarizedExperiment",
    readcounts.preprocessing.parameters = "list",
    readcounts.normalized = "SummarizedExperiment",
    readcounts.normalization.parameters = "list",
    readcounts.postprocessed = "SummarizedExperiment",
    readcounts.postprocessing.parameters = "list",
    gene.annotation = "GeneAnnotation"
  ),
  prototype = list(
    readcounts.raw = NULL,
    readcounts.preprocessed = NULL,
    readcounts.preprocessing.parameters = list(),
    readcounts.normalized = NULL,
    readcounts.normalization.parameters = list(),
    readcounts.postprocessed = NULL,
    readcounts.postprocessing.parameters = list(),
    gene.annotation = NULL
  ),
  validity = function(object) {
    return(T)
  }
)
