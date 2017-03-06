library(shiny)
library(shinyBS)

source("readCountImporter.R")
source("annotationImporter.R")
source("readCountNormalizer.R")
source("widgetGenericImporter.R")
source("widgetInPlaceHelp.R")

#' Creates the data options panel that allows the user to upload and normalize the data
#'
#' @return
#' @export
#'
#' @examples
uiPCADataPanel <- function() {
  return(bsCollapse(
      bsCollapsePanel("Read counts",
                      genericImporterInput("pca.data.readcounts",
                                     c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"),
                                     supportedReadcountDataTypes)
                      ),
      bsCollapsePanel("Annotation",
                      genericImporterInput("pca.data.annotation",
                                     c(
                                       "text/plain",
                                       ".gff",
                                       ".gff3"),
                                     supportedAnnotationDataTypes)
                     ),
      bsCollapsePanel("Normalization",
                      radioButtons("pca.data.normalization",
                                   "Apply read count normalization:",
                                   supportedReadcountNormalizationTypes),
                      helpText("RPKM (Reads per Kilobase per Million mapped reads) is not supported. See the PCAGO help for more information why RPKM is not included.")),
      bsCollapsePanel("Conditions")
    )
  )
}