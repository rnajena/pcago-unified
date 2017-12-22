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
                              "GO term ids" =  "go_ids",
                              "Custom" = "custom")

# Gene annotation names that are represented in sequence info table
GeneAnnotationEntryNames.sequence.info <- c("Start position" = "start_position",
                                            "End position" = "end_position",
                                            "Length" =  "length",
                                            "Exon length" =  "exon_length")

#' Contains the annotation of various genes
#'
#' @slot sequence.info data.frame. 
#' @slot gene.biotype GeneFilter. 
#' @slot gene.go.ids GeneFilter. 
#'
#' @return
#' @export
#'
#' @examples
GeneAnnotation <- setClass(
  "GeneAnnotation",
  slots = signature(
    sequence.info = "data.frame",
    gene.biotype = "ANY",
    gene.go.ids = "ANY",
    gene.scaffold = "ANY",
    gene.custom = "ANY"
  ),
  prototype = list(
    sequence.info = data.frame(),
    gene.biotype = GeneFilter$new(),
    gene.go.ids = GeneFilter$new(),
    gene.scaffold = GeneFilter$new(),
    gene.custom = GeneFilter$new()
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
              object@gene.scaffold <- GeneFilter$new()
            }
            if(!("biotype" %in% content_types)) {
              object@gene.biotype <- GeneFilter$new()
            }
            if(!("go_ids" %in% content_types)) {
              object@gene.go.ids <- GeneFilter$new()
            }
            if(!("custom" %in% content_types)) {
              object@gene.custom <- GeneFilter$new()
            }
           
            # Restrict sequence info table
            keep.sequence.info <- intersect(content_types, GeneAnnotationEntryNames.sequence.info)
            
            if(length(keep.sequence.info) == 0) {
              object@sequence.info <- data.frame()
            }
            else {
              to.remove <- setdiff(GeneAnnotationEntryNames.sequence.info, content_types)
              
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
            print("Loading sequence info from table ...")
            sequence.info <- data.frame(row.names = genes)
            
            for(col in c("start_position", "end_position", "length", "exon_length")) {
              if(col %in% colnames(table)) {
                sequence.info[,col] <- table[,col]
              }
              else {
                sequence.info[,col] <- rep(NA, nrow(table))
              }
            }
            
            if(ncol(sequence.info) == 0) {
              sequence.info = data.frame()
            }
            
            # Build gene filters
            print("Building standard gene filters ...")
            
            gene.biotype = GeneFilter$new()
            gene.scaffold = GeneFilter$new()
            gene.custom = GeneFilter$new()
            
            if("biotype" %in% colnames(table)) gene.biotype$load_from(setNames(rownames(table), table$biotype))
            if("scaffold" %in% colnames(table)) gene.scaffold$load_from(setNames(rownames(table), table$scaffold))
            if("custom" %in% colnames(table)) gene.custom$load_from(setNames(rownames(table), table$custom))
            
            # GO terms are special: The annotation stores a list of GO terms, so we cannot use the handy build function
            print("Building GO term gene filter ...")
            gene.go.ids <- GeneFilter$new()
            
            if("go_ids" %in% colnames(table)) {

              # Problem: We have Gene -> List of GO terms. But we want GO term -> list of genes.
              # Solution: Build Gene -> List of GO terms and invert it.

              data <- list()
              for(gene in rownames(table)) {
                cell.value <- table[gene, "go_ids"]
                
                go.terms <- if(!is.na(cell.value)) na.omit(unlist(strsplit(cell.value, "|", fixed = T))) else c()

                if(length(go.terms) > 0) {
                  data[[gene]] <- go.terms
                }

              }
            
              gene.go.ids <- GeneFilter$new(data = data)$invert()

            }
            
            
            print("Building custom gene filter ...")
            if("custom" %in% colnames(table)) {
              
              data <- list()
              for(gene in rownames(table)) {
                cell.value <- table[gene, "custom"]
                
                custom <- if(!is.na(cell.value)) na.omit(unlist(strsplit(cell.value, "|", fixed = T))) else c()
                
                if(length(go.terms) > 0) {
                  data[[gene]] <- custom
                }
                
              }
              
              gene.custom <- GeneFilter$new(data = data)$invert()
              
            }
            
            
            return(GeneAnnotation(
              sequence.info = sequence.info,
              gene.biotype = gene.biotype,
              gene.go.ids = gene.go.ids,
              gene.scaffold = gene.scaffold,
              gene.custom = gene.custom
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
                              (object@gene.biotype$get_genes()),
                              (object@gene.go.ids$get_genes()),
                              (object@gene.scaffold$get_genes()),
                              (object@gene.custom$get_genes())))
            
            table <- data.frame(row.names = genes,
                                "scaffold" = rep(NA, length(genes)),
                                "start_position" = rep(NA, length(genes)),
                                "end_position" = rep(NA, length(genes)),
                                "length" = rep(NA, length(genes)),
                                "exon_length" = rep(NA, length(genes)),
                                "biotype" = rep(NA, length(genes)),
                                "go_ids" = rep(NA, length(genes)),
                                "custom" = rep(NA, length(genes)),
                                check.names = F)
            
            table[rownames(object@sequence.info), "start_position"] <- object@sequence.info$start
            table[rownames(object@sequence.info), "end_position"] <- object@sequence.info$end
            table[rownames(object@sequence.info), "length"] <- object@sequence.info$length
            table[rownames(object@sequence.info), "exon_length"] <- object@sequence.info$exon_length
            
            # Extract data from filters. They need to be inverted.
            gene.go.ids.inv <- object@gene.go.ids$invert_data()
            gene.biotype.inv <- object@gene.biotype$invert_data()
            gene.scaffold.inv <- object@gene.scaffold$invert_data()
            gene.custom.inv <- object@gene.custom$invert_data()
            
            if(length(gene.go.ids.inv) > 0) {
              table[names(gene.go.ids.inv), "go_ids"] <- sapply(names(gene.go.ids.inv), function(x) {
                return(paste(gene.go.ids.inv[[x]], collapse = "|"))
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
            if(length(gene.custom.inv) > 0) {
              table[names(gene.custom.inv), "custom"] <- sapply(names(gene.custom.inv), function(x) {
                return(paste(gene.custom.inv[[x]], collapse = "|"))
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
            object1@gene.biotype <- object1@gene.biotype$merge_with(object2@gene.biotype)
            object1@gene.go.ids <- object1@gene.go.ids$merge_with(object2@gene.go.ids)
            object1@gene.scaffold <- object1@gene.scaffold$merge_with(object2@gene.scaffold)
            object1@gene.custom <- object1@gene.custom$merge_with(object2@gene.custom)
            
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
            
            object@gene.biotype <- object@gene.biotype$restrict_to(restrict.gene)
            object@gene.go.ids <- object@gene.go.ids$restrict_to(restrict.gene)
            object@gene.scaffold <- object@gene.scaffold$restrict_to(restrict.gene)
            object@gene.custom <- object@gene.custom$restrict_to(restrict.gene)
            
            return(object)
          })

#' Returns list of genes that are annotatated with given annotation type
#'
#' @param object GeneAnnotation object
#' @param annotation scaffold, start_position, end_position, length, exon_length, biotype, go_ids
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
              return((object@gene.scaffold$get_genes()))
            }
            else if(annotation == "biotype") {
              return((object@gene.biotype$get_genes()))
            }
            else if(annotation == "go_ids") {
              return((object@gene.go.ids$get_genes()))
            }
            else if(annotation == "custom") {
              return((object@gene.custom$get_genes()))
            }
            else {
              stop(paste("Unknown annotation type ", annotation))
            }
            
          })