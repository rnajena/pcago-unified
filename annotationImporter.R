#
# Contains methods that will import the annotations
#

# A list of all read count data types that will be supported
# The user selects one of those types, which will then invoke the corresponding importer
supportedAnnotationDataTypes <- c("GFF v3" = "gff3")

#' Imports annotation from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedAnnotationDataTypes
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importAnnotation <- function(filehandle, datatype) {
  
}