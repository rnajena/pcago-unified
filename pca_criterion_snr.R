library(matrixStats)

#' Returns true if two hierarchical clusterings have the same structure. Tree height is ignored.
#'
#' @param cluster1 
#' @param cluster2 
#'
#' @return
#' @export
#'
#' @examples
clustering.equal <- function(cluster1, cluster2) {
  
  # cluster1$height <- rep(0, length(cluster1$height))
  # cluster2$height <- rep(0, length(cluster2$height))
  # 
  # return(identical(as.dendrogram(cluster1), as.dendrogram(cluster2)))
  
  return(identical(cluster1$order, cluster2$order) && identical(cluster1$merge, cluster2$merge))
  
}

#' Finds minimal n top variant genes that have the same clustering as all genes
#'
#' @param data 
#' @param method.dist 
#' @param method.link 
#'
#' @return
#' @export
#'
#' @examples
find.minimal.clustering.genes.simple.index <- function(data, method.dist, method.link, pca.center, pca.scale) {
  
  reference.data.pca <- prcomp(t(data), center = pca.center, scale. = pca.scale)$x
  reference.clustering <- hclust(dist(reference.data.pca, method = method.dist), method = method.link)
  
  data <- data[order(rowVars(data), decreasing = T),]
  
  for(i in 2:nrow(data)) {
    
    test.data <- data[1:i,] # Select the top i most variant genes
    
    test.data.pca <- prcomp(t(test.data), center = pca.center, scale. = pca.scale)$x
    test.clustering <-hclust(dist(test.data.pca, method = method.dist), method = method.link)
    
    if(clustering.equal(reference.clustering, test.clustering)) {
      return(i)
    }
    
  }
  
  return(nrow(data))
}

#' Finds minimal n top variant genes, where n + i; i>= 0 top variant genes have the same clustering as all genes
#'
#' @param data 
#' @param method.dist 
#' @param method.link 
#'
#' @return
#' @export
#'
#' @examples
find.minimal.clustering.genes.improved.index <- function(data, method.dist, method.link, pca.center, pca.scale) {
  
  reference.data.pca <- prcomp(t(data), center = pca.center, scale. = pca.scale)$x
  reference.clustering <- hclust(dist(reference.data.pca, method = method.dist), method = method.link)
  
  data <- data[order(rowVars(data), decreasing = T),]
  top <- NA
  
  for(i in 2:nrow(data)) {
    
    test.data <- data[1:i,] # Select the top i most variant genes
    
    test.data.pca <- prcomp(t(test.data), center = pca.center, scale. = pca.scale)$x
    test.clustering <-hclust(dist(test.data.pca, method = method.dist), method = method.link)
    
    if(clustering.equal(reference.clustering, test.clustering)) {
      if(is.na(top)) {
        top <- i
      }
    }
    else {
      top <- NA
    }
    
  }
  
  if(is.na(top)) {
    top <- nrow(data)
  }
  
  return(top)
}