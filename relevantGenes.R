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
clustering.order.equal_ <- function(cluster1, cluster2) {
  return(identical(cluster1$order, cluster2$order) && identical(cluster1$merge, cluster2$merge))
}

clustering.order.equal <- compiler::cmpfun(clustering.order.equal_)
clustering.equal <- clustering.order.equal


#' Extracts all sets defined in a dendrogram
#'
#' @param cluster hclust dendrogram
#'
#' @return
#' @export
#'
#' @examples
clustering.extract.sets_ <- function(cluster) {
  
  sets <- as.list(cluster$labels)
  
  for(height in cluster$height) {
    
    grp <- cutree(cluster, h = height)
    grp <- split(names(grp), grp)
    sets <- append(sets, grp)
    
  }
  
  names(sets) <- NULL
  
  return(unique(sets))
}

clustering.extract.sets <- compiler::cmpfun(clustering.extract.sets_)

#' Returns true if two hierarchical clusterings have the same structure. Tree height is ignored.
#'
#' @param cluster1
#' @param cluster2
#'
#' @return
#' @export
#'
#' @examples
clustering.set.equal_ <- function(cluster1, cluster2) {
  
  return(setequal(clustering.extract.sets(cluster1), clustering.extract.sets(cluster2)))
  
}

clustering.set.equal <- compiler::cmpfun(clustering.set.equal_)

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
clustering_ <- function(data, method.dist, method.link, pca.enable, pca.center, pca.scale) {
  
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

clustering <- compiler::cmpfun(clustering_)

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
find.minimal.clustering.genes.index_ <- function(data, reference.clustering, method.dist, method.link, pca.enable, pca.center, pca.scale, f.clustering.equal = clustering.order.equal, updateProgress = NULL) {
  
  data <- data[order(rowVars(data), decreasing = T),]
  top <- NA
  
  for(i in 2:nrow(data)) {
    
    if(i %% 100 == 0) {
      print(i)
      
      if(is.function(updateProgress)) {
        if(is.na(top)) {
          updateProgress(value = i / nrow(data), detail = paste("Testing gene subsets. No best solution so far"))  
        }
        else {
          updateProgress(value = i / nrow(data), detail = paste("Testing gene subsets. Current best solution is", top))  
        }
      }
      
    }
    
    test.data <- data[1:i,] # Select the top i most variant genes
    test.clustering <- clustering(test.data, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(f.clustering.equal(reference.clustering, test.clustering)) {
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

find.minimal.clustering.genes.index <- compiler::cmpfun(find.minimal.clustering.genes.index_)

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
estimate.superset.clusterequal.debug_ <- function(data,
                                                 data.candidate.relevant,
                                                 reference.clustering,
                                                 method.dist, 
                                                 method.link, 
                                                 pca.enable,
                                                 pca.center, 
                                                 pca.scale,
                                                 times.superset) {
  
  print(paste("Superset test ", method.dist, method.link, pca.enable, pca.center, pca.scale))
  
  fails.count <- 0
  fails <- data.frame(row.names = rownames(data), fails = rep(0, nrow(data)))
  
  if(!setequal(rownames(data), rownames(data.candidate.relevant))) {
    
    # To make the algorithm robust against the set of candidate relevant genes
    data.noncandidates <- data[setdiff(rownames(data), rownames(data.candidate.relevant)),,drop=F]
    
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
    
  }
  else {
    print("Set of relevant genes is equal to set of all genes")
    fails.count <- -1
  }
  
  
  return(list(fails.count = fails.count, fails.table = fails))
  
}

estimate.superset.clusterequal.debug <- compiler::cmpfun(estimate.superset.clusterequal.debug_)

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
estimate.superset.clusterequal_ <- function(data,
                                           data.candidate.relevant,
                                           reference.clustering,
                                           method.dist, 
                                           method.link, 
                                           pca.enable,
                                           pca.center, 
                                           pca.scale,
                                           times.superset) {
  
  if(setequal(rownames(data), rownames(data.candidate.relevant))) {
    return(T)
  }
  
  # To make the algorithm robust against the set of candidate relevant genes
  data.noncandidates <- data[setdiff(rownames(data), rownames(data.candidate.relevant)),]
  
  for(i in seq_len(times.superset)) {
    sample.size <- sample.int(nrow(data.noncandidates), 1)
    
    data.superset.rownames <- union(rownames(data.noncandidates)[sample.int(nrow(data.noncandidates), sample.size)], rownames(data.candidate.relevant))
    data.superset <- data[data.superset.rownames,,drop=F]
    
    superset.clustering <- clustering(data.superset, method.dist, method.link, pca.enable, pca.center, pca.scale)
    
    if(!clustering.equal(superset.clustering, reference.clustering)) {
      return(F)
    }
  }
  
  return(T)
  
}

estimate.superset.clusterequal <- compiler::cmpfun(estimate.superset.clusterequal_)


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
    sample.size <- sample.int(max(1, nrow(data.min) - nrow(data.candidate.relevant) - 1), 1) 
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