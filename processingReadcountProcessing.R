#' 
#' Processing steps for read count processing
#' 

source("widgetProcessingSteps.R")

processingReadCountsProcessing.step.transpose <- function(input, readcounts.processed, readcounts.preprocessing.output) {
  return(reactive({
    validate(need(readcounts.processed(), "No processed read counts available."),
             need(readcounts.preprocessing.output(), "No read count processing info available!"))
    
    if("transpose" %in% input$pca.data.readcounts.processing) {
      return(list(title = "Transpose read counts",
                  content = "Read counts have been transposed."))
    }
    else {
      return(NULL)
    }
    
  }))
}

processingReadcountProcessing.step.remove.constant <- function(input, readcounts.processed, readcounts.preprocessing.output) {
  return(reactive({
    
    if("remove.constant" %in% input$pca.data.readcounts.processing) {
      
      content <- "No genes have been removed."
      removed.genes <- readcounts.preprocessing.output()$removed.genes
      
      if(length(removed.genes) != 0) {
        
        genes <- paste0(removed.genes, collapse = ", ")
        content <- paste(length(removed.genes) ,"genes have been removed:", genes)
      }
      
      return(list(title = "Remove constant read count genes",
                  content = content))
    }
    else {
      return(NULL)
    }
    
  }))
}

processingReadcountProcessing.step.normalization <- function(input, readcounts.normalization.output) {
  return(reactive({
    
    if(input$pca.data.normalization == "tpm") {
      
      content <- tagList()
      
      # TODO: Table that shows that the read counts are normalized now (sum is same for all samples/cells)
      
      return(list(title = "Apply TPM normalization",
                  content = content))
      
    }
    else if(input$pca.data.normalization == "deseq2") {
      
      content <- tagList()
      
      # TODO: Table of conditions & parameters of DESeq2
      
      return(list(title = "Apply DESeq2 normalization",
                  content = content))
      
    }
    else {
      return(NULL)
    }
    
  }))
}

#' Summary of read count processing.
#'
#' @return
#' @export
#'
#' @examples
processingReadCountsProcessing <- function(input, readcounts.processed, readcounts.preprocessing.output, readcounts.normalization.output) {
  
  step.transpose <- processingReadCountsProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- processingReadCountsProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- processingReadcountProcessing.step.normalization(input, readcounts.normalization.output)
  
  processingStepsWidgetData("readcounts.processing.steps",
                            step.transpose,
                            step.remove.constant,
                            step.normalization)
  
}