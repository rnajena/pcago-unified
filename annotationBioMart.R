#'
#' All Ensembl BioMart functions
#' 

library(shiny)
library(biomaRt)
library(GO.db)

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
getBioMartGOTerms <- function(mart, genes) {
  
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "go_id", "chromosome_name", "start_position", "end_position"), 
              filters = c("ensembl_gene_id"),
              values = genes, 
              mart = mart)
  
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
getBioMartSequenceInfo <- function(mart, genes) {
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "chromosome_name", "start_position", "end_position"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = mart)
  
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

bioMart.databaseChoices <- function(datatype) {
  
  marts <- biomaRt::listMarts()
  choices <- marts$biomart
  names(choices) <- marts$version
  
  return(choices)
  
}

bioMart.speciesChoices <- function(datatype, database) {
  stop("todo")
  return(c())
}

bioMart.importerEntry <- ImporterEntry(name = "ensembl_biomart",
                                       label = "Ensembl BioMart",
                                       parameters = list(
                                         ImporterParameter(name = "datatype",
                                                           label = "Extract information",
                                                           type = "select",
                                                           select.values = c("Sequence info" = "sequence.info",
                                                                             "GO terms" = "go.terms")),
                                         ImporterParameter(name = "database",
                                                           label = "Database",
                                                           type = "select",
                                                           select.values = bioMart.databaseChoices),
                                         ImporterParameter(name = "species", 
                                                           label = "Species", 
                                                           type = "select", 
                                                           select.values = bioMart.speciesChoices) 
                                       ))


generateGeneInformation.EnsemblBioMart <- function(data, database, species, readcounts) {
  
  genes <- rownames(readcounts)
  bio.mart <- biomaRt::useMart(database, species)
  
  if(data == "go.terms") {
    
    go.terms.table <- getBioMartGOTerms(bio.mart, genes)
    
    if(is.null(go.terms.table)) {
      stop("Data set does not contain even one of the requested genes! Did you choose a wrong data set?")
    }
    
    # We have to transform the table with columns gene_id, term to term -> list of gene ids
    go.terms.filter <- list()
    
    for(term in unique(go.terms.table$go_term)) {
      
      if(term == "") {
        next()
      }
      
      gene.indices <- term == go.terms.table$go_term
      genes <- go.terms.table$ensembl_gene_id[gene.indices]
      
      go.terms.filter[[term]] <- genes
      
    }
    
    return(Annotation(gene.go.terms = GeneFilter(data = go.terms.filter)))
  }
  else if(data == "sequence.info") {
    
    sequence.info <- getBioMartSequenceInfo(bio.mart, genes)
    
    if(is.null(sequence.info)) {
      stop("Data set does not contain even one of the requested genes! Did you choose a wrong data set?")
    }
    
    # Extract scaffold filter
    scaffolds <- list()
    for(scaffold in unique(sequence.info$scaffold)) {
      scaffolds[[scaffold]] <- rownames(sequence.info)[sequence.info$scaffold == scaffold]
    }
    
    return(Annotation(sequence.info = sequence.info,
                      gene.scaffold = GeneFilter(data = scaffolds)))
    
  }
  
  
}