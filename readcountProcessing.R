#' 
#' Contains the step details and processing outputs for readcount processing
#' The steps are function with 'step'. If we want to create an overview of the
#' processing steps, we use a function with 'at'.
#' 
#' e.g. if we want an overview of final processed readcounts, we include
#' transposition, removing constant reads and normalization
#' 

source("widgetProcessingSteps.R")
library(htmltools)

readcountProcessing.step.transpose <- function(input, readcounts.processed, readcounts.preprocessing.output) {
  return(reactive({
    validate(need(readcounts.processed(), "No processed read counts available."),
             need(readcounts.preprocessing.output(), "No read count processing info available!"))
    
    if("transpose" %in% input$pca.data.readcounts.processing) {
      
      return(list(title = "Transpose read counts",
                  content = "Read counts table have been transposed."))
    }
    else {
      return(NULL)
    }
    
  }))
}

readcountProcessing.step.remove.constant <- function(input, readcounts.processed, readcounts.preprocessing.output) {
  return(reactive({
    
    if("remove.constant" %in% input$pca.data.readcounts.processing) {
      
      content <- "No genes have been removed."
      removed.genes <- readcounts.preprocessing.output()$removed.genes
      
      if(length(removed.genes) != 0) {
        
        genes <- paste0(removed.genes, collapse = "\n")
        content <- tagList(tags$p(paste(length(removed.genes) ,"genes have been removed.")),
                           tags$textarea(genes, readonly = "readonly", style = css(width = "100%",
                                                                                   height = "200px")))  
      }
      
      return(list(title = "Remove constant read count genes",
                  content = content))
    }
    else {
      return(NULL)
    }
    
  }))
}

readcountProcessing.step.normalization <- function(input, readcounts.normalization.output) {
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

readcountProcessing.step.filter <- function(readcounts.processed, genes.filtered) {
  return(reactive({
    
    genes <- intersect(rownames(readcounts.processed()), genes.filtered()$values)
    
    # Summary of selected genes
    genesinfo <- tagList(
      tags$p(paste(length(genes), "genes selected.")),
      tags$textarea(paste(genes, collapse = "\n"), readonly = "readonly", style = css(width = "100%",
                                                              height = "200px"))
    )
    
    # Give summary of the filters the user selected
    keysinfo <- tagList()
    selectedkeys <- split(genes.filtered()$keys, sapply(genes.filtered()$keys, function(x){ unlist(strsplit(x, ".", fixed = T))[1] }))
    
    for(criterion in names(selectedkeys)) {
      
      selection <- sapply(selectedkeys[[criterion]], function(x) {
        return(paste(unlist(strsplit(x, ".", fixed = T))[-1], collapse = " "))
      })
      
      keysinfo <- tagAppendChild(keysinfo, h3(criterion))
      keysinfo <- tagAppendChild(keysinfo, paste(selection, collapse = ","))
    }
    
    # Summary of filter settings
    filterinfo <- tagList(tags$p(paste("Operation:", genes.filtered()$operation)),
                          tags$p(paste("Invert results:", genes.filtered()$invert)))
    
    # Build final UI
    content <- tagList(
      genesinfo,
      h2("Selected criteria"),
      keysinfo,
      h2("Filter settings"),
      filterinfo)
    
    return(list(title = "Filtered by annotation",
                content = content))
    
  }))
}

#' Summary of read count processing at the point when all read counts are processed
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.processed <- function(input, 
                                                        readcounts.processed, 
                                                        readcounts.preprocessing.output, 
                                                        readcounts.normalization.output) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  
  processingStepsWidgetData("readcounts.processing.steps",
                            step.transpose,
                            step.remove.constant,
                            step.normalization)
  
}

readcountProcessing.at.readcounts.filtered <- function(input, 
                                                       readcounts.processed, 
                                                       readcounts.preprocessing.output, 
                                                       readcounts.normalization.output,
                                                       genes.filtered) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  step.filter <- readcountProcessing.step.filter(readcounts.processed, genes.filtered)
  
  processingStepsWidgetData("readcounts.filtered.steps",
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter)
  
}