#'
#' Extract annotation from GRanges
#' 

library(AnnotationDbi)

#' Extracts sequence info annotation and scaffold filter from a GRanges object
#'
#' @param gr 
#' @param genes 
#'
#' @return
#' @export
#'
#' @examples
GRanges.extractSequenceInfoAnnotation <- function(gr, genes) {
  
  gff <- mcols(gr)
  
  meta.indices <- match(genes, gff$gene_id) # F: Index of gene in readcounts -> Index of gene in gff annotation
  
  if(any(is.na(meta.indices))) {
    
    # Remove genes that are NA. They will not appear in the annotation
    na.genes <- genes[is.na(meta.indices)]
    genes <- genes[!is.na(meta.indices)]
    meta.indices <- na.omit(meta.indices)
    
    showNotification(type = "warning", 
                     duration = NULL,
                     paste("Could not find sequence information for all genes. Following genes are affected:", 
                           strJoinLimited(na.genes, limit = 10)))
    
  }
  
  if(length(meta.indices) == 0) {
    return(Annotation())
  }
  
  sequence.info <- data.frame(row.names = genes,
                              scaffold = as.vector(seqnames(gr)[meta.indices]),
                              start_position = start(gr)[meta.indices],
                              end_position = end(gr)[meta.indices],
                              length = width(gr)[meta.indices],
                              stringsAsFactors = F)
  
  # As we have sequence info, extract scaffold filter
  scaffolds <- list()
  for(scaffold in unique(sequence.info$scaffold)) {
    scaffolds[[scaffold]] <- rownames(sequence.info)[sequence.info$scaffold == scaffold]
  }
  
  return(Annotation(sequence.info = sequence.info, 
                    gene.scaffold = GeneFilter(data = scaffolds)))
  
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
  
  return(Annotation(gene.features = GeneFilter(data = features)))
  
}