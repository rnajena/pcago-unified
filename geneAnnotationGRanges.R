#'
#' Extract annotation from GRanges
#' 

library(AnnotationDbi)
library(GenomicFeatures)

#' Extracts sequence info annotation and scaffold filter from a GRanges object
#'
#' @param gr 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
GRanges.extractSequenceInfoAnnotation <- function(gr, genes, imported_data) {
  
  gff <- mcols(gr)
  
  meta.indices <- match(genes, gff$gene_id) # F: Index of gene in readcounts -> Index of gene in gff annotation
  
  if(any(is.na(meta.indices))) {
    # Remove genes that are NA. They will not appear in the annotation
    na.genes <- genes[is.na(meta.indices)]
    genes <- genes[!is.na(meta.indices)]
    meta.indices <- na.omit(meta.indices)
  }
  
  if(length(meta.indices) == 0) {
    return(GeneAnnotation())
  }
  
  # Extract available info directly from GRanges
  
  sequence.info <- data.frame(row.names = genes,
                              start_position = start(gr)[meta.indices],
                              end_position = end(gr)[meta.indices],
                              length = width(gr)[meta.indices],
                              exon_length = rep(NA, length(genes)),
                              stringsAsFactors = F)
  
  if("exon_length" %in% imported_data) {
    # For normalization we need the exonic length
    # https://www.biostars.org/p/83901/
    txdb <- suppressWarnings(makeTxDbFromGRanges(gr))
    exons.list.per.gene <- exonsBy(txdb,by="gene")
    exonic.gene.sizes <- sum(width(reduce(exons.list.per.gene)))
    
    #found.genes <- mcols(gr)[match(names(exonic.gene.sizes), gr$Name), "gene_id"] # Txdb uses name instead of gene_id
    found.genes <- intersect(names(exonic.gene.sizes), genes)
    
    if(length(found.genes) > 0) {
      sequence.info[found.genes, "exon_length"] <- exonic.gene.sizes[found.genes]
    }
    
  }
  
  return(GeneAnnotation(sequence.info = sequence.info))
  
}

#' Extracts biotype annotation from a GRanges object
#'
#' @param gr 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
GRanges.extractBiotypeAnnotation <- function(gr, genes) {
  
  gff <- mcols(gr)
  
  features <- list()
  
  for(feature_type in unique(gff$biotype)) {
    
    gene_ids <- unlist(gff[gff$biotype == feature_type,"gene_id"])
    gene_ids <- unique(intersect(gene_ids, genes))
    
    if(length(gene_ids) > 0) {
      features[[feature_type]] <- gene_ids 
    }
  }
  
  return(GeneAnnotation(gene.biotype = GeneFilter(data = features)))
  
}

#' Extracts scaffold annotation from a GRanges object
#'
#' @param gr 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
GRanges.extractScaffoldAnnotation <- function(gr, genes) {
  
  gff <- mcols(gr)
  
  scaffold <- list()
  
  for(type in unique(seqnames(gr))) {
    
    gene_ids <- unlist(gff[seqnames(gr) == type,"gene_id"])
    gene_ids <- unique(intersect(gene_ids, genes))
    
    if(length(gene_ids) > 0) {
      scaffold[[type]] <- gene_ids 
    }
  }
  
  return(GeneAnnotation(gene.scaffold = GeneFilter(data = scaffold)))
}