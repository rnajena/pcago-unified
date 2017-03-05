#
# This file contains all PCA calculations
#

#' Applies PCA to input data
#'
#' @param inputdata 
#'
#' @return List of transformed values (transformed), variances (var), principal components (pc)
#' @export
#'
#' @examples
applyPCA <- function(inputdata) {
  
  if(is.null(inputdata) || ncol(inputdata) < 1 || nrow(inputdata) < 1) {
    return(NULL)
  }
  
  # Extract the actual data (as the first column are the gene ids)
  X <- inputdata[,-1]
  X <- t(X) # We want to do PCA for our conditions
  
  # Extract the genes for later use
  conditions <- colnames(inputdata)[-1]
  
  # PCA works by diagonalizing the covariance matrix by transforming
  # the data with a transform matrix consisting of the eigenvectors (as rows)
  # This transformation also maximizes the distance between the data points
  # and minimizes the error that happens if PCx dimensions are removed.
  # The eigenvalues are then the variances of the data in PCx direction -> we can rank them
  # We know how much a gene contributes to PCx just by looking at the values of the corresponding
  # eigenvector at the index of the gene.
  
  # Using R's internal function for improved speed (and accuracy as they use SDV)
  result <- prcomp(X)
  transformed <- data.frame("condition" = conditions, result$x)
  rownames(transformed) <- c() # We want an actual column
  
  variances <- data.frame(var = (result$sdev)^2)
  rownames(variances) <- names(result$rotation)
  
  return(list("transformed" = transformed,
              "var" = variances,
              "pc" = result$rotation))
}