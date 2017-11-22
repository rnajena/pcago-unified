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
    readcounts.preprocessing.parameters = NULL,
    readcounts.normalized = NULL,
    readcounts.normalization.parameters = NULL,
    readcounts.processed = NULL,
    readcounts.postprocessing.parameters = NULL,
    readcounts.filtered = NULL,
    readcounts.filtered.parameters.genes = NULL,
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
