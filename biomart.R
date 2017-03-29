#'
#' All Ensembl BioMart functions
#' 

library(biomaRt)
library(GO.db)

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
    datasets <- biomaRt::listDatasets(useMart(mart.id))
    
    output[[mart.name]] <- sapply(datasets$dataset, function(x) { paste0(mart.id, "@", x) })
    names(output[[mart.name]]) <- datasets$description
    
  }
  
  return(output)
  
}

#' Returns BioMart data set from string <BIOMART>@<DATASET>
#'
#' @param dataset.string 
#'
#' @return
#' @export
#'
#' @examples
getBioMartDataset <- function(dataset.string) {
  
  cell <- unlist(strsplit(dataset.string, "@", fixed = T))
  biomart <- cell[1]
  dataset <- cell[2]
  
  return(biomaRt::useMart(biomart, dataset = dataset))
  
}

#' Returns a table with gene id, GO term
#' Note that a gene can have multiple GO terms, so there can be multiple
#'
#' @param biomart.dataset 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
getBioMartGOTerms <- function(biomart.dataset, genes) {
  
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "go_id", "chromosome_name", "start_position", "end_position"), 
              filters = c("ensembl_gene_id"),
              values = genes, 
              mart = biomart.dataset)
  
  if(nrow(bm) == 0) {
    return(NULL)
  }
  
  bm <- bm[bm$go_id != "",] # Seems to sometimes return empty GO-IDs
  go.terms <- AnnotationDbi::select(GO.db, bm$go_id, c("TERM"))
  
  bm$go_term <- go.terms$TERM
  bm <- na.omit(bm) # If the term wasn't found
  
  return(bm)
}

#' Returns a table with sequence info (gene ,scaffold, start_position, end_position, length) for each gene
#'
#' @param biomart.dataset 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
getBioMartSequenceInfo <- function(biomart.dataset, genes) {
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "chromosome_name", "start_position", "end_position"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = biomart.dataset)
  
  if(nrow(bm) == 0) {
    return(NULL)
  }
  
  colnames(bm) <- c("gene","scaffold", "start_position", "end_position")
  rownames(bm) <- bm$gene
  bm <- bm[,c("scaffold", "start_position", "end_position")]
  bm <- na.omit(bm)
  bm$length <- abs(bm$start_position - bm$end_position + 1)
  
  return(bm)
}