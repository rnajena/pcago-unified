#'
#' Methods for read count processing
#' 

library(shiny)

#' Removes constant read count genes from the table.
#' As they result in variance = 0, scaling in the PCA step won't work
#'
#' @param readcounts 
#'
#' @return list of readcounts without constant entries (readcounts) and list of removed genes (genes.removed)
#' @export
#'
#' @examples
removeConstantReads <- function(readcounts) {
  
  invalid <- (do.call(pmin, readcounts) == do.call(pmax, readcounts))
  readcounts.removed <- readcounts[which(!invalid),]
  genes.removed <- rownames(readcounts)[invalid]
  
  return(list(readcounts = readcounts.removed, genes.removed = genes.removed))
  
}

#' Transposes the read count table
#'
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
transposeReadCounts <- function(readcounts) {
  
  return(data.frame(t(readcounts)))
  
}

#' Server function for processed read counts
#'
#' @param readcounts 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadCountProcessing <- function(readcounts, input) {
  
  return(reactive({
    
    output <- list( removed.genes = c(), readcounts = readcounts() )
    
    browser()
    
    # Transpose read counts
    if("transpose" %in% input$pca.data.readcounts.processing) {
      output$readcounts <- transposeReadCounts(output$readcounts)
    }
    
    # Remove constant read genes
    if("remove.constant" %in% input$pca.data.readcounts.processing) {
      processed <- removeConstantReads(output$readcounts)
      output$readcounts <- processed$readcounts
      output$removed.genes <- processed$genes.removed
    }
    
    # Apply normalization
    output$readcounts <- applyReadcountNormalization(output$readcounts, input$pca.data.normalization)
    
    return(output)
    
  }))
  
}