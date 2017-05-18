#'
#' This class contains the annotation results
#' As they have an exactly defined structure, use a class
#' 

library(shiny)
source("classGeneFilter.R")

GeneAnnotationEntryNames <- c("Scaffold" = "scaffold",
                              "Start position" = "start_position",
                              "End position" = "end_position",
                              "Length" =  "length",
                              "Exon length" =  "exon_length",
                              "Biotype" =  "biotype", 
                              "GO terms" =  "go_terms")

#' Contains the annotation of various genes
#'
#' @slot sequence.info data.frame. 
#' @slot gene.biotype GeneFilter. 
#' @slot gene.go.terms GeneFilter. 
#'
#' @return
#' @export
#'
#' @examples
GeneAnnotation <- setClass(
  "GeneAnnotation",
  slots = signature(
    sequence.info = "data.frame",
    gene.biotype = "GeneFilter",
    gene.go.terms = "GeneFilter",
    gene.scaffold = "GeneFilter"
  ),
  prototype = list(
    sequence.info = data.frame(),
    gene.biotype = GeneFilter(),
    gene.go.terms = GeneFilter(),
    gene.scaffold = GeneFilter()
  ),
  validity = function(object) {
    
    if(nrow(object@sequence.info) > 0 && 
       ncol(object@sequence.info) > 0 && 
       !setequal(colnames(object@sequence.info), c("start_position", "end_position", "length", "exon_length"))) {
      return("Invalid sequence info object")
    }
    
    return(T)
  }
)

#' Restricts the information contained within a gene annotation
#'
#' @param object GeneAnnotation object
#' @param content_types character vector of GeneAnnotationEntryNames
#'
#' @return
#' @export
#' @rdname geneAnnotationRestrictContentTypes
#'
#' @examples
setGeneric(name = "geneAnnotationRestrictContentTypes",
           def = function(object, content_types) {
             standardGeneric("geneAnnotationRestrictContentTypes")
           })

#' @rdname geneAnnotationRestrictContentTypes
setMethod(f = "geneAnnotationRestrictContentTypes",
          signature = signature(object = "GeneAnnotation", content_types = "character"),
          definition = function(object, content_types) {
            
            if(!("scaffold" %in% content_types)) {
              object@gene.scaffold <- GeneFilter()
            }
            if(!("biotype" %in% content_types)) {
              object@gene.biotype <- GeneFilter()
            }
            if(!("go_terms" %in% content_types)) {
              object@gene.go.terms <- GeneFilter()
            }
           
            # Restrict sequence info table
            keep.sequence.info <- intersect(content_types, c("start_position", "end_position", "length", "exon_length"))
            
            if(length(keep.sequence.info) == 0) {
              object@sequence.info <- data.frame()
            }
            else {
              to.remove <- setdiff(c("start_position", "end_position", "length", "exon_length"), content_types)
              
              if(length(to.remove) > 0) {
                object@sequence.info[,to.remove] <- NA
              }
              
            }
            
            return(object)
            
          })

#' Checks if a gene annotation has a sequence info table
#'
#' @param object GeneAnnotation object
#'
#' @return
#' @export
#' @rdname geneAnnotationHasSequenceInfo
#'
#' @examples
setGeneric(name = "geneAnnotationHasSequenceInfo",
           def = function(object) {
             standardGeneric("geneAnnotationHasSequenceInfo")
           })

#' @rdname geneAnnotationHasSequenceInfo
setMethod(f = "geneAnnotationHasSequenceInfo",
          signature = signature(object = "GeneAnnotation"),
          definition = function(object) {
            return(nrow(object@sequence.info) > 0 && ncol(object@sequence.info) > 0)
          })

#' Loads an annotation from a data frame
#'
#' @param object GeneAnnotation object
#'
#' @return
#' @export
#' @rdname geneAnnotationFromTable
#'
#' @examples
setGeneric(name = "geneAnnotationFromTable",
           def = function(table) {
             standardGeneric("geneAnnotationFromTable")
           })

#' @rdname geneAnnotationFromTable
setMethod(f = "geneAnnotationFromTable",
          signature = signature(table = "data.frame"),
          definition = function(table) {
            
            genes <- rownames(table)
            
            # Load sequence info
            sequence.info <- data.frame(row.names = genes)
            
            for(col in c("start_position", "end_position", "length", "exon_length")) {
              if(col %in% colnames(table)) {
                sequence.info[,col] <- table[,col]
              }
            }
            
            if(ncol(sequence.info) == 0) {
              sequence.info = data.frame()
            }
            
            # Build gene filters
            gene.biotype = if("biotype" %in% colnames(table)) buildGeneFilter(setNames(rownames(table), table$biotype)) else GeneFilter()
            gene.scaffold = if("scaffold" %in% colnames(table)) buildGeneFilter(setNames(rownames(table), table$scaffold)) else GeneFilter()
            
            # GO terms are special: The annotation stores a list of GO terms, so we cannot use the handy build function
            gene.go.terms <- GeneFilter()
            
            if("go_terms" %in% colnames(table)) {

              # Problem: We have Gene -> List of GO terms. But we want GO term -> list of genes.
              # Solution: Build Gene -> List of GO terms and invert it.

              data <- list()
              for(gene in rownames(table)) {
                cell.value <- table[gene, "go_terms"]
                
                go.terms <- if(!is.na(cell.value)) na.omit(unlist(strsplit(cell.value, "|", fixed = T))) else c()

                if(length(go.terms) > 0) {
                  data[[gene]] <- go.terms
                }

              }
              
              data <- invertGeneFilter(GeneFilter(data = data))
              gene.go.terms <- GeneFilter(data = data)

            }
            
            
            
            return(GeneAnnotation(
              sequence.info = sequence.info,
              gene.biotype = gene.biotype,
              gene.go.terms = gene.go.terms,
              gene.scaffold = gene.scaffold
            ))
            
          })

#' Returns a data frame that contains the annotation data
#'
#' @param object GeneAnnotation object
#'
#' @return
#' @export
#' @rdname geneAnnotationToTable
#'
#' @examples
setGeneric(name = "geneAnnotationToTable",
           def = function(object) {
             standardGeneric("geneAnnotationToTable")
           })

#' @rdname geneAnnotationToTable
setMethod(f = "geneAnnotationToTable",
          signature = signature(object = "GeneAnnotation"),
          definition = function(object) {
            
            genes <- unique(c(rownames(object@sequence.info),
                              geneFilterGenes(object@gene.biotype),
                              geneFilterGenes(object@gene.go.terms),
                              geneFilterGenes(object@gene.scaffold)))
            
            table <- data.frame(row.names = genes,
                                "scaffold" = rep(NA, length(genes)),
                                "start_position" = rep(NA, length(genes)),
                                "end_position" = rep(NA, length(genes)),
                                "length" = rep(NA, length(genes)),
                                "exon_length" = rep(NA, length(genes)),
                                "biotype" = rep(NA, length(genes)),
                                "go_terms" = rep(NA, length(genes)),
                                check.names = F)
            
            table[rownames(object@sequence.info), "start_position"] <- object@sequence.info$start
            table[rownames(object@sequence.info), "end_position"] <- object@sequence.info$end
            table[rownames(object@sequence.info), "length"] <- object@sequence.info$length
            table[rownames(object@sequence.info), "exon_length"] <- object@sequence.info$exon_length
            
            # Extract data from filters. They need to be inverted.
            gene.go.terms.inv <- invertGeneFilter(object@gene.go.terms)
            gene.biotype.inv <- invertGeneFilter(object@gene.biotype)
            gene.scaffold.inv <- invertGeneFilter(object@gene.scaffold)
            
            if(length(gene.go.terms.inv) > 0) {
              table[names(gene.go.terms.inv), "go_terms"] <- sapply(names(gene.go.terms.inv), function(x) {
                return(paste(gene.go.terms.inv[[x]], collapse = "|"))
              }) 
            }
            if(length(gene.biotype.inv) > 0) {
              table[names(gene.biotype.inv), "biotype"] <- sapply(names(gene.biotype.inv), function(x) {
                return(paste(gene.biotype.inv[[x]], collapse = "|"))
              }) 
            }
            if(length(gene.scaffold.inv) > 0) {
              table[names(gene.scaffold.inv), "scaffold"] <- sapply(names(gene.scaffold.inv), function(x) {
                return(paste(gene.scaffold.inv[[x]], collapse = "|"))
              }) 
            }
            
            return(table)
            
          })

#' Merges two GeneAnnotation objects
#'
#' @param object1 GeneAnnotation object
#' @param object2 GeneAnnotation object
#'
#' @return
#' @export
#' @rdname mergeGeneAnnotation
#'
#' @examples
setGeneric(name = "mergeGeneAnnotation",
           def = function(object1, object2) {
             standardGeneric("mergeGeneAnnotation")
           })

#' @rdname mergeGeneAnnotation
setMethod(f = "mergeGeneAnnotation",
          signature = signature(object1 = "GeneAnnotation", object2 = "GeneAnnotation"),
          definition = function(object1, object2) {
            
            
            # Merge sequence info
            if(geneAnnotationHasSequenceInfo(object2)) {
              
              if(geneAnnotationHasSequenceInfo(object1)) {
                
                # overwritten.genes <- intersect(rownames(object1@sequence.info), rownames(object2@sequence.info))
                # 
                # if(length(overwritten.genes) > 0) {
                #   showNotification(paste("Overwriting existing sequence information of following genes:", strJoinLimited(overwritten.genes)),
                #                    type = "warning")
                # }
                
                object1@sequence.info[rownames(object2@sequence.info),] <- object2@sequence.info[,]
              }
              else {
                object1@sequence.info <- object2@sequence.info
              }
              
            }
            
            # Merge gene filters
            object1@gene.biotype <- mergeGeneFilter(object1@gene.biotype, object2@gene.biotype)
            object1@gene.go.terms <- mergeGeneFilter(object1@gene.go.terms, object2@gene.go.terms)
            object1@gene.scaffold <- mergeGeneFilter(object1@gene.scaffold, object2@gene.scaffold)
            
            return(object1)
           
          })

#' Returns a new GeneAnnotation object that only contains the genes defined in the restrict.genes vector
#'
#' @param object GeneAnnotation object
#' @param restrict.gene Vector of gene names
#'
#' @return
#' @export
#' @rdname geneAnnotationRestrictToGenes
#'
#' @examples
setGeneric(name = "geneAnnotationRestrictToGenes",
           def = function(object, restrict.gene) {
             standardGeneric("geneAnnotationRestrictToGenes")
           })

#' @rdname geneAnnotationRestrictToGenes
setMethod(f = "geneAnnotationRestrictToGenes",
          signature = signature(object = "GeneAnnotation", restrict.gene = "character"),
          definition = function(object, restrict.gene) {
            
            if(geneAnnotationHasSequenceInfo(object)) {
              
              supported.genes <- intersect(rownames(object@sequence.info), restrict.gene)
              
              if(length(supported.genes) > 0) {
                object@sequence.info <- object@sequence.info[supported.genes,]
              }
              
            }
            
            object@gene.biotype <- geneFilterRestrictToGenes(object@gene.biotype, restrict.gene)
            object@gene.go.terms <- geneFilterRestrictToGenes(object@gene.go.terms, restrict.gene)
            object@gene.scaffold <- geneFilterRestrictToGenes(object@gene.scaffold, restrict.gene)
            
            return(object)
          })

#' Returns list of genes that are annotatated with given annotation type
#'
#' @param object GeneAnnotation object
#' @param annotation scaffold, start_position, end_position, length, exon_length, biotype, go_terms
#'
#' @return
#' @export
#' @rdname geneAnnotationAnnotatedGenes
#'
#' @examples
setGeneric(name = "geneAnnotationAnnotatedGenes",
           def = function(object, annotation) {
             standardGeneric("geneAnnotationAnnotatedGenes")
           })

#' @rdname geneAnnotationAnnotatedGenes
setMethod(f = "geneAnnotationAnnotatedGenes",
          signature = signature(object = "GeneAnnotation", annotation = "character"),
          definition = function(object, annotation) {
            
            if(annotation %in% c("start_position", "end_position", "length", "exon_length")) {
              return(if(geneAnnotationHasSequenceInfo(object)) rownames(object@sequence.info)[!is.na(object@sequence.info[[annotation]])] else c())
            }
            else if(annotation == "scaffold") {
              return(geneFilterGenes(object@gene.scaffold))
            }
            else if(annotation == "biotype") {
              return(geneFilterGenes(object@gene.biotype))
            }
            else if(annotation == "go_terms") {
              return(geneFilterGenes(object@gene.go.terms))
            }
            else {
              stop(paste("Unknown annotation type ", annotation))
            }
            
          })