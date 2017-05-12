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

#' Returns information that input read counts are used
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.input.readcounts <- function(readcounts.raw) {
  return(reactive({
    return(list(
      title = "Raw read counts",
      content = paste0("Table with ", nrow(readcounts.raw()), " rows and ", ncol(readcounts.raw()), " columns")
    ))
  }))
}

#' Returns information that variances of read counts are calculated
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.calculate.variance <- function() {
  return(reactive({
    return(list(
      title = "Calculate gene variances",
      content = "Calculate variance for each gene in read count table."
    ))
  }))
}

#' Returns information about read count pre-processing step: Transpose matrix
#'
#' @param input 
#' @param readcounts.processed 
#' @param readcounts.preprocessing.output 
#'
#' @return
#' @export
#'
#' @examples
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

#' Returns information about read count pre-processing step: Remove constant reads
#'
#' @param input 
#' @param readcounts.processed 
#' @param readcounts.preprocessing.output 
#'
#' @return
#' @export
#'
#' @examples
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

#' Returns information about normalizing read counts
#'
#' @param input 
#' @param readcounts.normalization.output 
#'
#' @return
#' @export
#'
#' @examples
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

#' Returns information about filtering with annotation criteria
#'
#' @param readcounts.processed 
#' @param readcounts.filtered 
#' @param genes.filtered 
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.filter <- function(readcounts.processed, readcounts.filtered, genes.filtered) {
  return(reactive({
    
    # Summary of selected genes
    genesinfo <- tagList(
      tags$p(paste0(nrow(readcounts.filtered()), "/", nrow(readcounts.processed()), " genes selected.")),
      tags$textarea(paste(rownames(readcounts.filtered()), collapse = "\n"), readonly = "readonly", style = css(width = "100%",
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
      keysinfo <- tagAppendChild(keysinfo, paste(selection, collapse = ", "))
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

#' Returns information about filtering by a variance cut-off
#'
#' @param readcounts.filtered 
#' @param readcounts.top.variant 
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.top.variant <- function(readcounts.filtered, readcounts.top.variant) {
  return(reactive({
    
    # Summary of selected genes
    genesinfo <- tagList(
      tags$p(paste0(nrow(readcounts.top.variant()), "/", nrow(readcounts.filtered()), " top variant genes selected.")),
      tags$textarea(paste(rownames(readcounts.top.variant()), collapse = "\n"), readonly = "readonly", style = css(width = "100%",
                                                                                                                height = "200px"))
    )
    
    # Give summary of variance
    var.filtered <- sum(rowVars(assay(readcounts.filtered())))
    var.top.variant <- sum(rowVars(assay(readcounts.top.variant())))
    
    varianceinfo <- tagList(tags$p( paste0( format((var.top.variant / var.filtered) * 100, digits = 2, scientific=F), "% of available variance." ) ))
    
    # Build final UI
    content <- tagList(
      genesinfo,
      varianceinfo)
    
    return(list(title = "Filtered by variance cut-off",
                content = content))
    
  }))
}

#' Returns information about PCA
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.pca <- function(readcounts.top.variant, pca) {
  return(reactive({
    
    content <- tagList(
      h2("Input"),
      tags$p(paste0("Table with ", ncol(readcounts.top.variant()), " points with ", nrow(readcounts.top.variant()), " dimensions")),
      h2("PCA parameters"),
      tags$p(paste0("Center data: ", pca()$params$center)),
      tags$p(paste0("Scale data: ", pca()$params$scale)),
      h2("Output parameters"),
      tags$p(paste0("Relative transformed positions: ", pca()$params$relative))
    )
    
    return(list(
      title = "Apply PCA",
      content = content
    ))
    
  }))
}

#' Summary of read count processing at the point when all read counts are processed
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.processed <- function(input, 
                                                        readcounts.raw,
                                                        readcounts.processed, 
                                                        readcounts.preprocessing.output, 
                                                        readcounts.normalization.output) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  
  processingStepsWidgetData("readcounts.processing.steps",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization)
  processingStepsWidgetData("genes.variance.processing",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            readcountProcessing.step.calculate.variance())
  
}

#' Summary of read count processing at the point when all read counts have been filtered with annotation criteria
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.filtered <- function(input, 
                                                       readcounts.raw,
                                                       readcounts.processed, 
                                                       readcounts.filtered,
                                                       readcounts.preprocessing.output, 
                                                       readcounts.normalization.output,
                                                       genes.filtered) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  step.filter <- readcountProcessing.step.filter(readcounts.processed, readcounts.filtered, genes.filtered)
  
  processingStepsWidgetData("readcounts.filtered.steps",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter)
  processingStepsWidgetData("genes.variance.filtered.processing",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter,
                            readcountProcessing.step.calculate.variance())
  
}

#' Summary of read count processing at the point when all read counts have been filtered with annotation criteria
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.top.variant <- function(input, 
                                                          readcounts.raw,
                                                         readcounts.processed, 
                                                         readcounts.filtered,
                                                         readcounts.top.variant,
                                                         readcounts.preprocessing.output, 
                                                         readcounts.normalization.output,
                                                         genes.filtered) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  step.filter <- readcountProcessing.step.filter(readcounts.processed, readcounts.filtered, genes.filtered)
  step.top.variant <- readcountProcessing.step.top.variant(readcounts.filtered, readcounts.top.variant)
  
  processingStepsWidgetData("readcounts.top.variant.steps",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter,
                            step.top.variant)
  
}

#' Summary of read count processing at the point when PCA is applied
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.pca <- function(input, 
                                       readcounts.raw,
                                        readcounts.processed, 
                                        readcounts.filtered,
                                        readcounts.top.variant,
                                        readcounts.preprocessing.output, 
                                        readcounts.normalization.output,
                                        genes.filtered,
                                       pca) {
  
  step.transpose <- readcountProcessing.step.transpose(input, readcounts.processed, readcounts.preprocessing.output)
  step.remove.constant <- readcountProcessing.step.remove.constant(input, readcounts.processed, readcounts.preprocessing.output)
  step.normalization <- readcountProcessing.step.normalization(input, readcounts.normalization.output)
  step.filter <- readcountProcessing.step.filter(readcounts.processed, readcounts.filtered, genes.filtered)
  step.top.variant <- readcountProcessing.step.top.variant(readcounts.filtered, readcounts.top.variant)
  step.pca <- readcountProcessing.step.pca(readcounts.top.variant, pca)
  
  processingStepsWidgetData("pca.pc.processing",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter,
                            step.top.variant,
                            step.pca)
  processingStepsWidgetData("pca.variance.processing",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter,
                            step.top.variant,
                            step.pca)
  processingStepsWidgetData("pca.transformed.processing",
                            readcountProcessing.step.input.readcounts(readcounts.raw),
                            step.transpose,
                            step.remove.constant,
                            step.normalization,
                            step.filter,
                            step.top.variant,
                            step.pca)
  
}