#'
#' This file contains all PCA calculations
#'

library(SummarizedExperiment)

#' Applies PCA to input data
#'
#' @param readcounts Read counts
#' @param center Center data
#' @param scale Apply variance scaling (no 0-variance genes allowed!)
#' @param relative Accepts none (Not relative), dimension (Per dimension) and global (Use global min and max)
#'
#' @return List of transformed values (transformed), variances (var), principal components (pc)
#' @export
#'
#' @examples
applyPCA <- function(readcounts, center, scale, relative) {
  
  # Soft and hard parameter checking
  if(is.null(readcounts)) {
    return(NULL)
  }
  if(!is.logical(center) || !is.logical(scale)) {
    stop("Invalid arguments!")
  }
  if(!is.character(relative) || !enum.contains(relative, c("none", "dimension", "global"))) {
    stop("Invalid arguments!")
  }
  
  # Extract the data 
  X <- assay(readcounts)
  X <- t(X) # Transpose this as our read count matrix as R has dimensions as columns and not as rows (thanks, R!!!)
  
  # Extract the cells for later use
  cells <- colnames(readcounts)
  
  # Using R's internal function for improved speed (and accuracy as they use SDV)
  # Caution: R will not consider all eigenvectors (there are thousands of genes)
  # Theoretically, we need to calculate ALL of them (we then obtain PC1, PC2, ... PCm with m dimensions = genes)
  # But R will truncate it to PC1, PC2, ... PCn with n data points (if n < m), which is fast.
  # Don't let this confuse you
  result <- prcomp(X, center = center, scale = scale)
  transformed <- data.frame(result$x, row.names = cells)
  pc.names <- colnames(result$rotation)
  
  # Optionally make the transformed coordinates relative.
  # This makes them scale-invariant, but keeps the distance relation (which is the important part)
  # Either per dimension (might skew stuff) or global (should be always fine)
  if(relative == "dimension") {
    for(pc in colnames(transformed)) {
      pc.min <- min(transformed[[pc]])
      pc.max <- max(transformed[[pc]])
      transformed[[pc]] <- sapply(transformed[[pc]], function(x) { (x - pc.min) / (pc.max - pc.min) })
    }
  }
  else if(relative == "global") {
    pc.min <- min(result$x)
    pc.max <- max(result$x)
    
    for(pc in colnames(transformed)) {
      transformed[[pc]] <- sapply(transformed[[pc]], function(x) { (x - pc.min) / (pc.max - pc.min) })
    }
  }
  
  # Build the variance table
  variances <- (result$sdev)^2
  variances.table <- data.frame(var = variances, 
                                var.relative = variances / sum(variances), 
                                row.names = pc.names)
  
  
  return(list("transformed" = transformed,
              "var" = variances.table,
              "pc" = result$rotation,
              "params" = list(
                "center" = center,
                "scale" = scale,
                "relative" = relative
              )))
}

