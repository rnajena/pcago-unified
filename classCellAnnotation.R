#'
#' Class that stores the annotation for cells
#' 

library(shiny)

#' Contains the annotation of various cells
#'
#' @slot conditions Table that determines if a cell (row) is in a condition condition (column)
#' @slot fragment.lengths Table that contains annotations for each cell (row) 
#'
#' @return
#' @export
#'
#' @examples
CellAnnotation <- setClass(
  "CellAnnotation",
  slots = signature(
    conditions = "data.frame",
    cell.info = "data.frame"
  ),
  prototype = list(
    conditions = data.frame(),
    cell.info = data.frame()
  ),
  validity = function(object) {
    
    if(nrow(object@conditions) > 0) {
      if(!all(is.logical(as.matrix(object@conditions)))) {
        return(F)
      }
    }
    
    if(nrow(object@cell.info) > 0 && colnames(object@cell.info) != c("meanfragmentlength")) {
      return("Invalid cell info object")
    }
    
    return(T)
  }
)

#' Checks if a cell annotation has a conditions table
#'
#' @param object CellAnnotation object
#'
#' @return
#' @export
#' @rdname cellAnnotationHasConditions
#'
#' @examples
setGeneric(name = "cellAnnotationHasConditions",
           def = function(object) {
             standardGeneric("cellAnnotationHasConditions")
           })

#' @rdname cellAnnotationHasConditions
setMethod(f = "cellAnnotationHasConditions",
          signature = signature(object = "CellAnnotation"),
          definition = function(object) {
            return(nrow(object@conditions) > 0)
          })

#' Checks if a cell annotation has a cell info table
#'
#' @param object CellAnnotation object
#'
#' @return
#' @export
#' @rdname cellAnnotationHasCellInfo
#'
#' @examples
setGeneric(name = "cellAnnotationHasCellInfo",
           def = function(object) {
             standardGeneric("cellAnnotationHasCellInfo")
           })

#' @rdname cellAnnotationHasCellInfo
setMethod(f = "cellAnnotationHasCellInfo",
          signature = signature(object = "CellAnnotation"),
          definition = function(object) {
            return(nrow(object@cell.info) > 0)
          })

#' Checks if a cell annotation and a list of cell names are matching
#'
#' @param object CellAnnotation object
#' @param cells vector of cells
#'
#' @return
#' @export
#' @rdname cellAnnotationMatchesCells
#'
#' @examples
setGeneric(name = "cellAnnotationMatchesCells",
           def = function(object, cells) {
             standardGeneric("cellAnnotationMatchesCells")
           })

#' @rdname cellAnnotationMatchesCells
setMethod(f = "cellAnnotationMatchesCells",
          signature = signature(object = "CellAnnotation", cells = "character"),
          definition = function(object, cells) {
            if(cellAnnotationHasConditions(object)) {
              if(!setequal(rownames(object@conditions), cells)) {
                return(F)
              }
            }
            if(cellAnnotationHasCellInfo(object)) {
              if(!setequal(rownames(object@cell.info), cells)) {
                return(F)
              }
            }
            return(T)
          })

#' Merges two CellAnnotation objects
#'
#' @param object1 CellAnnotation object
#' @param object2 CellAnnotation object
#'
#' @return
#' @export
#' @rdname mergeCellAnnotation
#'
#' @examples
setGeneric(name = "mergeCellAnnotation",
           def = function(object1, object2) {
             standardGeneric("mergeCellAnnotation")
           })

#' @rdname mergeCellAnnotation
setMethod(f = "mergeCellAnnotation",
          signature = signature(object1 = "CellAnnotation", object2 = "CellAnnotation"),
          definition = function(object1, object2) {
            
            # Conditions are just overwritten
            if(cellAnnotationHasConditions(object2)) {
              object1@conditions <- object2@conditions
            }
            
            # Merge cell info
            if(cellAnnotationHasCellInfo(object2)) {
              
              if(cellAnnotationHasCellInfo(object1)) {
                
                object1@sequence.info[rownames(object2@cell.info),] <- object2@cell.info[,]
              }
              else {
                object1@cell.info <- object2@cell.info
              }
              
            }
            
            return(object1)
            
          })