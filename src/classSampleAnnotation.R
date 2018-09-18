#'
#' Class that stores the annotation for samples
#' 

library(shiny)

SampleAnnotationNames <- c("Conditions" = "conditions",
                           "Mean fragment lengths" = "meanfragmentlength")

#' Contains the annotation of various samples
#'
#' @slot conditions Table that determines if a sample (row) is in a condition condition (column)
#' @slot fragment.lengths Table that contains annotations for each sample (row) 
#'
#' @return
#' @export
#'
#' @examples
SampleAnnotation <- setClass(
  "SampleAnnotation",
  slots = signature(
    conditions = "data.frame",
    sample.info = "data.frame"
  ),
  prototype = list(
    conditions = data.frame(),
    sample.info = data.frame()
  ),
  validity = function(object) {
    
    if(nrow(object@conditions) > 0) {
      if(!all(is.logical(as.matrix(object@conditions)))) {
        return(F)
      }
    }
    
    if(nrow(object@sample.info) > 0 && colnames(object@sample.info) != c("meanfragmentlength")) {
      return("Invalid sample info object")
    }
    
    return(T)
  }
)

#' Checks if a sample annotation has a conditions table
#'
#' @param object SampleAnnotation object
#'
#' @return
#' @export
#' @rdname sampleAnnotationHasConditions
#'
#' @examples
setGeneric(name = "sampleAnnotationHasConditions",
           def = function(object) {
             standardGeneric("sampleAnnotationHasConditions")
           })

#' @rdname sampleAnnotationHasConditions
setMethod(f = "sampleAnnotationHasConditions",
          signature = signature(object = "SampleAnnotation"),
          definition = function(object) {
            return(nrow(object@conditions) > 0)
          })

#' Checks if a sample annotation has a sample info table
#'
#' @param object SampleAnnotation object
#'
#' @return
#' @export
#' @rdname sampleAnnotationHasSampleInfo
#'
#' @examples
setGeneric(name = "sampleAnnotationHasSampleInfo",
           def = function(object) {
             standardGeneric("sampleAnnotationHasSampleInfo")
           })

#' @rdname sampleAnnotationHasSampleInfo
setMethod(f = "sampleAnnotationHasSampleInfo",
          signature = signature(object = "SampleAnnotation"),
          definition = function(object) {
            return(nrow(object@sample.info) > 0)
          })

#' Checks if a sample annotation and a list of sample names are matching
#'
#' @param object SampleAnnotation object
#' @param samples vector of samples
#'
#' @return
#' @export
#' @rdname sampleAnnotationMatchesSamples
#'
#' @examples
setGeneric(name = "sampleAnnotationMatchesSamples",
           def = function(object, samples) {
             standardGeneric("sampleAnnotationMatchesSamples")
           })

#' @rdname sampleAnnotationMatchesSamples
setMethod(f = "sampleAnnotationMatchesSamples",
          signature = signature(object = "SampleAnnotation", samples = "character"),
          definition = function(object, samples) {
            if(sampleAnnotationHasConditions(object)) {
              if(!setequal(rownames(object@conditions), samples)) {
                return(F)
              }
            }
            if(sampleAnnotationHasSampleInfo(object)) {
              if(!setequal(rownames(object@sample.info), samples)) {
                return(F)
              }
            }
            return(T)
          })

#' Merges two SampleAnnotation objects
#'
#' @param object1 SampleAnnotation object
#' @param object2 SampleAnnotation object
#'
#' @return
#' @export
#' @rdname mergeSampleAnnotation
#'
#' @examples
setGeneric(name = "mergeSampleAnnotation",
           def = function(object1, object2) {
             standardGeneric("mergeSampleAnnotation")
           })

#' @rdname mergeSampleAnnotation
setMethod(f = "mergeSampleAnnotation",
          signature = signature(object1 = "SampleAnnotation", object2 = "SampleAnnotation"),
          definition = function(object1, object2) {
            
            # Conditions are just overwritten
            if(sampleAnnotationHasConditions(object2)) {
              object1@conditions <- object2@conditions
            }
            
            # Merge sample info
            if(sampleAnnotationHasSampleInfo(object2)) {
              
              if(sampleAnnotationHasSampleInfo(object1)) {
                
                object1@sequence.info[rownames(object2@sample.info),] <- object2@sample.info[,]
              }
              else {
                object1@sample.info <- object2@sample.info
              }
              
            }
            
            return(object1)
            
          })

#' Returns list of samples that are annotatated with given annotation type
#'
#' @param object SampleAnnotation object
#' @param annotation conditions, meanfragmentlengths
#'
#' @return
#' @export
#' @rdname sampleAnnotationAnnotatedSamples
#'
#' @examples
setGeneric(name = "sampleAnnotationAnnotatedSamples",
           def = function(object, annotation) {
             standardGeneric("sampleAnnotationAnnotatedSamples")
           })

#' @rdname sampleAnnotationAnnotatedSamples
setMethod(f = "sampleAnnotationAnnotatedSamples",
          signature = signature(object = "SampleAnnotation", annotation = "character"),
          definition = function(object, annotation) {
            
            if(annotation %in% c("meanfragmentlength")) {
              return(if(sampleAnnotationHasSampleInfo(object)) rownames(object@sample.info)[!is.na(object@sample.info[[annotation]])] else c())
            }
            else if(annotation == "conditions") {
              return(rownames(object@conditions))
            }
            else {
              stop(paste("Unknown annotation type ", annotation))
            }
            
          })

#' Returns a data frame that contains the annotation data
#'
#' @param object SampleAnnotation object
#'
#' @return
#' @export
#' @rdname sampleAnnotationToTable
#'
#' @examples
setGeneric(name = "sampleAnnotationToTable",
           def = function(object) {
             standardGeneric("sampleAnnotationToTable")
           })

#' @rdname sampleAnnotationToTable
setMethod(f = "sampleAnnotationToTable",
          signature = signature(object = "SampleAnnotation"),
          definition = function(object) {
            
            samples <- unique(c(rownames(object@sample.info)))
            
            table <- data.frame(row.names = samples,
                                "meanfragmentlength" = rep(NA, length(samples)),
                                check.names = F)
            
            table[rownames(object@sample.info), "meanfragmentlength"] <- object@sample.info$meanfragmentlength
            
            return(table)
            
          })