#'
#' All Ensembl BioMart functions
#' 

library(shiny)
library(biomaRt)
library(GO.db)
library(GenomicRanges)

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
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", 
                                      "chromosome_name", 
                                      "start_position", 
                                      "end_position",
                                      "ensembl_exon_id",
                                      "exon_chrom_start",
                                      "exon_chrom_end",
                                      "rank",
                                      "external_gene_name"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = mart)
  
  if(nrow(bm) == 0) {
    return(NULL)
  }
  
  # Build GRanges and parse with GRanges sequence info tool
  bm.genes <- bm[match(unique(bm$ensembl_gene_id), bm$ensembl_gene_id),]
  gr.genes <- GRanges(seqnames = bm.genes$chromosome_name, 
                      ranges = IRanges(start = bm.genes$start_position, 
                                       end = bm.genes$end_position))
  mcols(gr.genes) <- data.frame(gene_id = bm.genes$ensembl_gene_id,
                                type = rep("gene", nrow(bm.genes)))
  
  gr.exons <- GRanges(seqnames = bm$chromosome_name, 
                      ranges = IRanges(start = bm$exon_chrom_start, 
                                       end = bm$exon_chrom_end))
  mcols(gr.exons) <- data.frame(gene_id = bm$ensembl_gene_id,
                                type = rep("exon", nrow(bm)))
  
  gr <- gr.genes
  
  stop("Not implemented!")
  
  return(gr)
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
getBioMartGOTerms <- function(mart, genes) {
  
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "go_id"), 
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

#' Returns a table with gene and biotype 
#'
#' @param mart 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
getBioMartBiotype <- function(mart, genes) {
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "gene_biotype"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = mart)
  
  if(nrow(bm) == 0) {
    return(NULL)
  }
  
  colnames(bm) <- c("gene","type")
  bm <- na.omit(bm)
  
  return(bm)
}

#' Returns a table with gene and scaffold 
#'
#' @param mart 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
getBioMartScaffold <- function(mart, genes) {
  bm <- biomaRt::getBM(attributes = c("ensembl_gene_id", "chromosome_name"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = mart)
  
  if(nrow(bm) == 0) {
    return(NULL)
  }
  
  colnames(bm) <- c("gene","scaffold")
  bm <- na.omit(bm)
  
  return(bm)
}

bioMart.databaseChoices <- function(datatype) {
  
  if(!is.character(datatype) || datatype == "") {
    return(c())
  }
  
  marts <- biomaRt::listMarts()
  choices <- marts$biomart
  names(choices) <- marts$version
  
  return(c("", choices))
  
}

bioMart.speciesChoices <- function(datatype, database) {
  
  if(!is.character(datatype) || datatype == "") {
    return(c())
  }
  if(!is.character(database) || database == "") {
    return(c())
  }
  
  mart <- biomaRt::useMart(database)
  mart.datasets <- biomaRt::listDatasets(mart)
  
  choices <- mart.datasets$dataset
  names(choices) <- mart.datasets$description
  
  return(choices)
}

bioMart.importerEntry <- ImporterEntry(name = "ensembl_biomart",
                                       label = "Ensembl BioMart",
                                       parameters = list(
                                         ImporterParameter(name = "database",
                                                           label = "Database",
                                                           type = "select",
                                                           select.values = bioMart.databaseChoices),
                                         ImporterParameter(name = "species", 
                                                           label = "Species", 
                                                           type = "select", 
                                                           select.values = bioMart.speciesChoices),
                                         ImporterParameter(name = "imported_data",
                                                           label = "Imported data",
                                                           type = "checkboxes",
                                                           checkboxes.options = GeneAnnotationEntryNames,
                                                           checkboxes.selected = GeneAnnotationEntryNames)
                                       ))


generateGeneInformation.EnsemblBioMart <- function(database, species, imported_data, readcounts) {
  
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  genes <- rownames(readcounts)
  bio.mart <- biomaRt::useMart(database, species)
  
  output <- GeneAnnotation()
  
  if("go_terms" %in% imported_data) {
    
    go.terms.table <- getBioMartGOTerms(bio.mart, genes)
    
    if(!is.null(go.terms.table)) {
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
      
      output <- mergeGeneAnnotation(output, GeneAnnotation(gene.go.terms = GeneFilter(data = go.terms.filter)))
    }
    
  }
  
  if("biotype" %in% imported_data) {
    
    biotypes.table <- getBioMartBiotype(bio.mart, genes)
    
    if(!is.null(biotypes.table)) {
      # Extract filter
      biotypes <- list()
      for(feature in unique(biotypes.table$type)) {
        biotypes[[feature]] <- unique(biotypes.table$gene[biotypes.table$type == feature])
      }
      
      output <- mergeGeneAnnotation(output, GeneAnnotation(gene.biotype = GeneFilter(data = biotypes)))
    }
    
  }
  
  if("scaffold" %in% imported_data) {
    
    scaffold.table <- getBioMartScaffold(bio.mart, genes)
    
    if(!is.null(scaffold.table)) {
      # Extract filter
      scaffold <- list()
      for(feature in unique(scaffold.table$type)) {
        scaffold[[feature]] <- unique(scaffold.table$gene[scaffold.table$type == feature])
      }
      
      output <- mergeGeneAnnotation(output, GeneAnnotation(gene.scaffold = GeneFilter(data = scaffold)))
    }
    
  }
  
  if(length(intersect(imported_data, c("start_position", "end_position", "length", "exon_length"))) > 0) {
    sequence.info <- getBioMartSequenceInfo(bio.mart, genes)
    
    if(!is.null(sequence.info)) {
      output <- mergeGeneAnnotation(GeneAnnotation(sequence.info = sequence.info))
    }
    
  }
  
  output <- geneAnnotationRestrictContentTypes(output, imported_data)
  
  return(output)
  
}