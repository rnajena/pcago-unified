#'
#' All Ensembl BioMart functions
#' 

library(biomaRt)

#' Returns all available biomart datasets as list of vectors
#' The data sets are stored in following fashion: <BIOMART>@<DATASET>
#' 
#' This is done as the list can be directly used by selectizeInput
#'
#' @return
#' @export
#'
#' @examples
getBioMartDatsets <- function() {
  
  marts <- biomaRt::listMarts()
  
  output <- list()
  
  for(i in 1:nrow(marts)) {
    
    mart.name <- marts[i, "version"]
    mart.id <- marts[i, "biomart"]
    datasets <- listDatasets(useMart(mart.id))
    
    output[[mart.name]] <- sapply(datasets$dataset, function(x) { paste0(mart.id, "@", x) })
    names(output[[mart.name]]) <- datasets$description
    
  }
  
  return(output)
  
}