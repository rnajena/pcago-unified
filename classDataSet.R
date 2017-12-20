#'
#' Class that holds the whole data set
#' 

library(R6)
library(SummarizedExperiment)
source("classGeneAnnotation.R")

PCAGODataSet <- R6Class(
  "PCAGODataSet",
  public = list(
    readcounts.raw = NULL,
    readcounts.preprocessed = NULL,
    readcounts.preprocessing.parameters = list(removed.genes = c(), operation.transpose = F, operation.remove.zero = F),
    readcounts.normalized = NULL,
    readcounts.normalization.parameters = NULL,
    readcounts.processed = NULL,
    readcounts.postprocessing.parameters = list(removed.genes = c(), operation.remove.constant = F),
    readcounts.filtered.keywords = NULL,
    readcounts.filtered.keywords.parameters.genes = NULL,
    readcounts.filtered = NULL,
    readcounts.top.variant = NULL,
    readcounts.top.variant.parameters.count = NULL,
    gene.annotation = NULL,
    sample.annotation = NULL,
    variances.processed = NULL,
    variances.filtered = NULL,
    variances.top.variant = NULL,
    pca.top.variant = NULL
  )
)
