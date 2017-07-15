library(matrixStats)
library(fastcluster)

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
  return(identical(cluster1$order, cluster2$order) && identical(cluster1$merge, cluster2$merge))
}

#' Applies clustering
#'
#' @param data 
#' @param method.dist 
#' @param method.link 
#' @param pca.enable 
#' @param pca.center 
#' @param pca.scale 
#'
#' @return
#' @export
#'
#' @examples
clustering <- function(data, method.dist, method.link, pca.enable, pca.center, pca.scale) {
  
  if(pca.enable) {
    data <- prcomp(t(data), center = pca.center, scale. = pca.scale)$x
  }
  else {
    data <- t(data)
  }
  
  if(ncol(data) < 2) {
    return(NULL)
  }
  
  clustering <- hclust(dist(data, method = method.dist), method = method.link)
  
  return(clustering)
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
find.minimal.clustering.genes.index <- function(data, reference.clustering, method.dist, method.link, pca.enable, pca.center, pca.scale) {
  
  data <- data[order(rowVars(data), decreasing = T),]
  top <- NA
  
  for(i in 2:nrow(data)) {
    
    if(i %% 1000 == 0) {
      print(i)
    }
    
    test.data <- data[1:i,] # Select the top i most variant genes
    test.clustering <- clustering(test.data, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(clustering.equal(reference.clustering, test.clustering)) {
      if(is.na(top)) {
        top <- i
      }
    }
    else {
      #print(paste(top, "deleted by", top))
      top <- NA
    }
    
  }
  
  if(is.na(top)) {
    top <- nrow(data)
  }
  
  return(top)
}

#' Estimates if all supersets of candidate relevant genes produce the same clustering.
#' This function outputs the number of fails and the genes involved.
#'
#' @param data 
#' @param data.candidate.relevant 
#' @param reference.clustering 
#' @param method.dist 
#' @param method.link 
#' @param pca.enable 
#' @param pca.center 
#' @param pca.scale 
#' @param times.superset 
#'
#' @return
#' @export
#'
#' @examples
estimate.superset.clusterequal.debug <- function(data,
                                                 data.candidate.relevant,
                                                 reference.clustering,
                                                 method.dist, 
                                                 method.link, 
                                                 pca.enable,
                                                 pca.center, 
                                                 pca.scale,
                                                 times.superset) {
  
  # To make the algorithm robust against the set of candidate relevant genes
  data.noncandidates <- data[setdiff(rownames(data), rownames(data.candidate.relevant)),]
  
  fails.count <- 0
  fails <- data.frame(row.names = rownames(data), fails = rep(0, nrow(data)))
  
  for(i in seq_len(times.superset)) {
    
    if(i %% 1000 == 0) {
      print(i)
    }
    
    sample.size <- sample.int(nrow(data.noncandidates), 1)
    
    data.superset.rownames <- union(rownames(data.noncandidates)[sample.int(nrow(data.noncandidates), sample.size)], rownames(data.candidate.relevant))
    data.superset <- data[data.superset.rownames,]
    
    superset.clustering <- clustering(data.superset, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(!clustering.equal(superset.clustering, reference.clustering)) {
      # get the new genes
      genes.superset.exclusive <- setdiff(data.superset.rownames, rownames(data.candidate.relevant))
      fails[genes.superset.exclusive,] <- fails[genes.superset.exclusive,] + 1
      fails.count <- fails.count + 1
    }
  }
  
  return(list(fails.count = fails.count, fails.table = fails))
  
}

#' Estimates if all supersets of candidate relevant genes produce the same clustering.
#'
#' @param data 
#' @param data.candidate.relevant 
#' @param reference.clustering 
#' @param method.dist 
#' @param method.link 
#' @param pca.enable 
#' @param pca.center 
#' @param pca.scale 
#' @param times.superset 
#'
#' @return
#' @export
#'
#' @examples
estimate.superset.clusterequal <- function(data,
                                           data.candidate.relevant,
                                           reference.clustering,
                                           method.dist, 
                                           method.link, 
                                           pca.enable,
                                           pca.center, 
                                           pca.scale,
                                           times.superset) {
  
  # To make the algorithm robust against the set of candidate relevant genes
  data.noncandidates <- data[setdiff(rownames(data), rownames(data.candidate.relevant)),]
  
  for(i in seq_len(times.superset)) {
    sample.size <- sample.int(nrow(data.noncandidates), 1)
    
    data.superset.rownames <- union(rownames(data.noncandidates)[sample.int(nrow(data.noncandidates), sample.size)], rownames(data.candidate.relevant))
    data.superset <- data[data.superset.rownames,]
    
    superset.clustering <- clustering(data.superset, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(!clustering.equal(superset.clustering, reference.clustering)) {
      return(F)
    }
  }
  
  return(T)
  
}


#' Estimates which genes are not important in the set of clustering relevant genes
#' and produces a smaller set
#'
#' @param data 
#' @param data.candidate.relevant 
#' @param reference.clustering 
#' @param method.dist 
#' @param method.link 
#' @param pca.enable 
#' @param pca.center 
#' @param pca.scale 
#' @param times.subset 
#' @param times.superset 
#' @param subset.min 
#'
#' @return
#' @export
#'
#' @examples
estimate.remove.nonrelevant.genes <- function(data, 
                                              data.candidate.relevant,
                                              reference.clustering,
                                              method.dist, 
                                              method.link, 
                                              pca.enable,
                                              pca.center, 
                                              pca.scale,
                                              times.subset = 1000, 
                                              times.superset = 1000,
                                              subset.min = 2) {
  
  # The current minimal kown set is the set of candidate relevant genes
  data.min <- data.candidate.relevant 
  
  for(i in seq_len(times.subset)) {
    
    sample.size <- sample.int(nrow(data.min) - 1, 1) # determine the sample size. should be 1 smaller than the current minimum
    test.data <- data.candidate.relevant[sample.int(nrow(data.candidate.relevant), max(subset.min, sample.size)),] # fetch <sample size> items from candidate
    
    test.clustering <- clustering(test.data, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(is.null(test.clustering)) {
      next()
    }
    
    if(clustering.equal(reference.clustering, test.clustering)) {
      print(paste("Same clustering found", nrow(test.data), "testing supersets."))
      
      # Now testing the superset (criterion that all supersets should have the same clustering)
      superset.ok <- estimate.superset.clusterequal(data = data,
                                                    data.candidate.relevant = test.data,
                                                    reference.clustering = reference.clustering,
                                                    method.dist = method.dist,
                                                    method.link = method.link,
                                                    pca.enable = pca.enable,
                                                    pca.center = pca.center,
                                                    pca.scale = pca.scale,
                                                    times.superset = times.superset)
      
      if(superset.ok && nrow(test.data) < nrow(data.min)) {
        print(paste("Found new minimal subset"))
        data.min <- test.data
      }
      
    }
    
  }
  
  return(data.min)
  
}

#' Estimates a superset of candidate clustering relevant genes that actually satisfies 
#' the criterion that all supersets should produce the reference clustering
#'
#' @param data 
#' @param data.candidate.relevant 
#' @param reference.clustering 
#' @param method.dist 
#' @param method.link 
#' @param pca.enable 
#' @param pca.center 
#' @param pca.scale 
#' @param times.superset 
#' @param times.superset.test 
#'
#' @return
#' @export
#'
#' @examples
estimate.add.relevant.genes <- function(data, 
                                        data.candidate.relevant,
                                        reference.clustering,
                                        method.dist, 
                                        method.link, 
                                        pca.enable,
                                        pca.center, 
                                        pca.scale,
                                        times.superset = 1000, 
                                        times.superset.test = 1000) {
  
  # To make the algorithm robust against the set of candidate relevant genes
  data.noncandidates <- data[setdiff(rownames(data), rownames(data.candidate.relevant)),]
  
  # The current minimal kown set is the set of all genes
  data.min <- data 
  
  for(i in seq_len(times.superset)) {
    
    # determine the sample size. should be 1 smaller than the current minimum
    # subtract the candidate relevant genes from the current minimal superset - 1
    # This is equivalent to length(setdiff(data.min, data.candidate.relevant)) as data.min is a subset of data.candidate.relevant
    sample.size <- sample.int(nrow(data.min) - nrow(data.candidate.relevant) - 1, 1) 
    data.superset.rownames <- union(rownames(data.noncandidates)[sample.int(nrow(data.noncandidates), sample.size)], rownames(data.candidate.relevant))
    test.data <- data[data.superset.rownames,]
    
    test.clustering <- clustering(test.data, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(is.null(test.clustering)) {
      next()
    }
    
    if(clustering.equal(reference.clustering, test.clustering)) {
      print(paste("Same clustering found", nrow(test.data), "testing supersets."))
      
      # Now testing the superset (criterion that all supersets should have the same clustering)
      superset.ok <- estimate.superset.clusterequal(data = data,
                                                    data.candidate.relevant = test.data,
                                                    reference.clustering = reference.clustering,
                                                    method.dist = method.dist,
                                                    method.link = method.link,
                                                    pca.enable = pca.enable,
                                                    pca.center = pca.center,
                                                    pca.scale = pca.scale,
                                                    times.superset = times.superset.test)
      
      if(superset.ok && nrow(test.data) < nrow(data.min)) {
        print(paste("Found new minimal superset."))
        data.min <- test.data
      }
      
    }
    
  }
  
  return(data.min)
  
}