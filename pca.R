#'
#' This file contains all PCA calculations
#'

#' Applies PCA to input data
#'
#' @param readcounts Read counts
#'
#' @return List of transformed values (transformed), variances (var), principal components (pc)
#' @export
#'
#' @examples
applyPCA <- function(readcounts, center, scale) {
  
  # Soft and hard parameter checking
  if(is.null(readcounts) || ncol(readcounts) == 0 || nrow(readcounts) == 0) {
    return(NULL)
  }
  if(!is.logical(center) || !is.logical(scale)) {
    stop("Invalid arguments!")
  }
  
  # Extract the data 
  X <- readcounts
  X <- t(X) # We want to do PCA for our cells
  
  # Extract the cells for later use
  cells <- colnames(readcounts)
  
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
  variances.table <- data.frame(var = variances, 
                                var.relative = variances / sum(variances), 
                                row.names = pc.names)
  
  return(list("transformed" = transformed,
              "var" = variances.table,
              "pc" = result$rotation))
}

