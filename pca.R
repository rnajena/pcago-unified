#'
#' This file contains all PCA calculations
#'

#' Applies PCA to input data
#'
#' @param inputdata 
#'
#' @return List of transformed values (transformed), variances (var), principal components (pc)
#' @export
#'
#' @examples
applyPCA <- function(inputdata, center, scale) {
  
  if(is.null(inputdata) || ncol(inputdata) == 0 || nrow(inputdata) == 0) {
    return(NULL)
  }
  
  # Extract the data 
  X <- inputdata
  X <- t(X) # We want to do PCA for our cells
  
  # Extract the cells for later use
  cells <- colnames(inputdata)
  
  # PCA works by diagonalizing the covariance matrix by transforming
  # the data with a transform matrix consisting of the eigenvectors (as rows)
  # This transformation also maximizes the distance between the data points
  # and minimizes the error that happens if PCx dimensions are removed.
  # The eigenvalues are then the variances of the data in PCx direction -> we can rank them
  # We know how much a gene contributes to PCx just by looking at the values of the corresponding
  # eigenvector at the index of the gene.
  
  # Using R's internal function for improved speed (and accuracy as they use SDV)
  result <- prcomp(X, center = center, scale = scale)
  transformed <- data.frame(result$x, row.names = cells)
  pc.names <- colnames(result$rotation)
  
  # Build the variance table
  variances <- (result$sdev)^2
  variances.percent <- variances / sum(variances)
  variances.table <- data.frame(var = variances, 
                                percentage = variances.percent, 
                                row.names = pc.names)
  
  return(list("transformed" = transformed,
              "var" = variances.table,
              "pc" = result$rotation))
}