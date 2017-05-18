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
                                      "ensembl_transcript_id",
                                      "transcript_start",
                                      "transcript_end",
                                      "strand"), 
                       filters = c("ensembl_gene_id"),
                       values = genes, 
                       mart = mart)
  
  if(nrow(bm) == 0) {
    return(GeneAnnotation())
  }
  
  # Build GRanges and parse with GRanges sequence info tool
  bm.genes <- bm[match(unique(bm$ensembl_gene_id), bm$ensembl_gene_id),]
  gr.genes <- GRanges(seqnames = bm.genes$chromosome_name, 
                      strand = bm.genes$strand,
                      ranges = IRanges(start = bm.genes$start_position, 
                                       end = bm.genes$end_position))
  mcols(gr.genes) <- data.frame(gene_id = bm.genes$ensembl_gene_id,
                                transcript_id = rep(NA, nrow(bm.genes)),
                                type = rep("gene", nrow(bm.genes)),
                                ID = sapply(bm.genes$ensembl_gene_id, function(x) { paste0("gene:", x) }),
                                Parent = rep(NA, nrow(bm.genes)),
                                stringsAsFactors = F)
  
  gr.transcripts <- GRanges(seqnames = bm$chromosome_name, 
                            strand = bm$strand,
                            ranges = IRanges(start = bm$transcript_start, 
                                             end = bm$transcript_end))
  mcols(gr.transcripts) <- data.frame(gene_id = bm$ensembl_gene_id,
                                      transcript_id = bm$ensembl_transcript_id,
                                      type = rep("transcript", nrow(bm)),
                                      ID = sapply(bm$ensembl_transcript_id, function(x) { paste0("transcript:", x) }),
                                      Parent = sapply(bm$ensembl_gene_id, function(x) { paste0("gene:", x) }))
  
  gr <- c(gr.genes, gr.transcripts)
  return(GRanges.extractSequenceInfoAnnotation(gr))
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
    return(GeneAnnotation())
  }
  
  bm <- bm[bm$go_id != "",] # Seems to sometimes return empty GO-IDs
  go.terms <- AnnotationDbi::select(GO.db, bm$go_id, c("TERM"))
  
  bm$go_term <- go.terms$TERM
  bm <- na.omit(bm) # If the term wasn't found
  
  # We have to transform the table with columns gene_id, term to term -> list of gene ids
  go.terms.filter <- list()
  
  for(term in unique(bm$go_term)) {
    
    if(term == "") {
      next()
    }
    
    gene.indices <- term == bm$go_term
    genes <- go.terms.table$ensembl_gene_id[gene.indices]
    
    go.terms.filter[[term]] <- genes
    
  }
  
  return(GeneAnnotation(genes.go.terms = GeneFilter(data = go.terms.filter)))
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
  
  # Extract filter
  biotypes <- list()
  for(feature in unique(bm$type)) {
    biotypes[[feature]] <- unique(biotypes.table$gene[bm$type == feature])
  }
  
  return(GeneAnnotation(gene.biotype = GeneFilter(data = biotypes)))
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
  bm <- na.omit(bm)
  
  if(nrow(bm) == 0) {
    return(GeneAnnotation())
  }
  
  colnames(bm) <- c("gene","scaffold")
 
  
  # Extract filter
  scaffold <- list()
  for(feature in unique(bm$type)) {
    scaffold[[feature]] <- unique(bm$gene[scaffold.table$type == feature])
  }
  
  return(GeneAnnotation(gene.scaffold = GeneFilter(data = scaffold)))
}

bioMart.databaseChoices <- function() {
  
  if(!is.character(datatype) || datatype == "") {
    return(c())
  }
  
  marts <- biomaRt::listMarts()
  choices <- marts$biomart
  names(choices) <- marts$version
  
  return(c("", choices))
  
}

bioMart.speciesChoices <- function(database) {
 
  if(!is.character(database) || length(database) != 1 || database == "") {
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
    output <- mergeGeneAnnotation(output, getBioMartGOTerms(bio.mart, genes))
  }
  
  if("biotype" %in% imported_data) {
    output <- mergeGeneAnnotation(output, getBioMartBiotype(bio.mart, genes))
  }
  
  if("scaffold" %in% imported_data) {
    output <- mergeGeneAnnotation(output, getBioMartScaffold(bio.mart, genes))
  }
  
  if(length(intersect(imported_data, GeneAnnotationEntryNames.sequence.info)) > 0) {
      output <- mergeGeneAnnotation(output, getBioMartSequenceInfo(bio.mart, genes))
  }
  
  output <- geneAnnotationRestrictContentTypes(output, imported_data)
  
  return(output)
  
}