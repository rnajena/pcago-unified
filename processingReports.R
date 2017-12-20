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
readcountProcessing.step.input.readcounts <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.raw, "No data available"))
    
    return(list(
      title = "Raw read counts",
      content = paste0("Table with ", nrow(dataset()$readcounts.raw), " rows and ", ncol(dataset()$readcounts.raw), " columns")
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
readcountProcessing.step.transpose <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.processed, "No data available"))
    validate(need(dataset()$readcounts.preprocessing.parameters, "No data available"))
    
    if(dataset()$readcounts.preprocessing.parameters$operation.transpose) {
      
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
readcountProcessing.step.remove.zero <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.processed, "No data available"))
    validate(need(dataset()$readcounts.preprocessing.parameters, "No data available"))
    
    if(dataset()$readcounts.preprocessing.parameters$operation.remove.zero) {
      
      content <- "No genes have been removed."
      removed.genes <- dataset()$readcounts.preprocessing.parameters$removed.genes
      
      if(length(removed.genes) != 0) {
        
        genes <- paste0(removed.genes, collapse = "\n")
        content <- tagList(tags$p(paste(length(removed.genes) ,"genes have been removed.")),
                           tags$textarea(genes, readonly = "readonly", style = css(width = "100%",
                                                                                   height = "200px")))  
      }
      
      return(list(title = "Remove zero count genes",
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
readcountProcessing.step.normalization <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.normalization.parameters, "No data available"))
    
    if(dataset()$readcounts.normalization.parameters$operation.normalization == "tpm") {
      
      output <- dataset()$readcounts.normalization.parameters
      
      content <- tagList(
        tags$p("Applied read count normalization with TPM method."),
        tags$p(paste("Use non-overlapping sum of exon lengths:", output$use.feature.exonlength)),
        tags$p(paste("Calculate effective length:", output$use.feature.effectivelength)),
        h2("Sum of normalized sample counts"),
        tags$div(class = "scrollable-x",
                 tags$table(
                   do.call(tags$tr, lapply(c("Sample", names(output$sample.sum)),function(x) { tags$td(x) })),
                   do.call(tags$tr, lapply(c("Sum of norm. counts", as.vector(output$sample.sum)),function(x) { tags$td(x) }))
                 ))
      )
      
      return(list(title = "Apply TPM normalization",
                  content = content))
      
    }
    else if(dataset()$readcounts.normalization.parameters$operation.normalization == "deseq2") {
     
      output <- dataset()$readcounts.normalization.parameters
      
      content <- tagList(
        tags$p("Applied read count normalization with DESeq2 method."),
        tags$p(paste("Design:", output$design)),
        h2("Sample conditions"),
        tags$div(class = "scrollable-x",
                 tags$table(
                   do.call(tags$tr, lapply(c("Sample", rownames(output$conditions)),function(x) { tags$td(x) })),
                   do.call(tags$tr, lapply(c("Gen. condition", as.vector(output$conditions$condition)),function(x) { tags$td(x) }))
                 )),
        tags$p(paste("Apply transformation:", output$deseq2.transformation))
      )
      
      return(list(title = "Apply DESeq2 normalization",
                  content = content))
      
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
readcountProcessing.step.remove.constant <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.postprocessing.parameters, "No data available"))
    
    if(dataset()$readcounts.postprocessing.parameters$operation.remove.constant) {
      
      content <- "No genes have been removed."
      removed.genes <- dataset()$readcounts.postprocessing.parameters$removed.genes
      
      if(length(removed.genes) != 0) {
        
        genes <- paste0(removed.genes, collapse = "\n")
        content <- tagList(tags$p(paste(length(removed.genes) ,"genes have been removed.")),
                           tags$textarea(genes, readonly = "readonly", style = css(width = "100%",
                                                                                   height = "200px")))  
      }
      
      return(list(title = "Remove constant count genes",
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
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.step.filter <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.processed, "No data available"))
    validate(need(dataset()$readcounts.filtered, "No data available"))
    validate(need(dataset()$readcounts.filtered.keywords.parameters.genes, "No data available"))
    
    # Summary of selected genes
    genesinfo <- tagList(
      tags$p(paste0(nrow(dataset()$readcounts.filtered), "/", nrow(dataset()$readcounts.processed), " genes selected.")),
      tags$textarea(paste(rownames(dataset()$readcounts.filtered), collapse = "\n"), readonly = "readonly", style = css(width = "100%",
                                                              height = "200px"))
    )
    
    # Give summary of the filters the user selected #TODO: GO terms
    genes.filtered <- dataset()$readcounts.filtered.keywords.parameters.genes
    keysinfo <- tagList()
    selectedkeys <- split(genes.filtered$keys, sapply(genes.filtered$keys, function(x){ unlist(strsplit(x, ".", fixed = T))[1] }))
    
    for(criterion in names(selectedkeys)) {
      
      selection <- sapply(selectedkeys[[criterion]], function(x) {
        return(paste(unlist(strsplit(x, ".", fixed = T))[-1], collapse = " "))
      })
      
      keysinfo <- tagAppendChild(keysinfo, h3(criterion))
      keysinfo <- tagAppendChild(keysinfo, tags$textarea( readonly="readonly", paste(selection, collapse = "\n"), style = css(width = "100%",
                                                                                                                              height = "200px")))
    }
    
    # Summary of filter settings
    filterinfo <- tagList(tags$p(paste("Operation:", genes.filtered$operation)),
                          tags$p(paste("Invert results:", genes.filtered$invert)))
    
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
readcountProcessing.step.top.variant <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.filtered, "No data available"))
    validate(need(dataset()$readcounts.top.variant, "No data available"))
    
    # Summary of selected genes
    genesinfo <- tagList(
      tags$p(paste0(nrow(dataset()$readcounts.top.variant), "/", nrow(dataset()$readcounts.filtered), " top variant genes selected.")),
      tags$textarea(paste(rownames(dataset()$readcounts.top.variant), collapse = "\n"), readonly = "readonly", style = css(width = "100%",
                                                                                                                height = "200px"))
    )
    
    # Give summary of variance
    var.filtered <- sum(rowVars(assay(dataset()$readcounts.filtered)))
    var.top.variant <- sum(rowVars(assay(dataset()$readcounts.top.variant)))
    
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
readcountProcessing.step.pca <- function(dataset) {
  return(reactive({
    
    validate(need(dataset(), "No data available"))
    validate(need(dataset()$readcounts.top.variant, "No data available"))
    validate(need(dataset()$pca.top.variant, "No data available"))
    
    pca <- dataset()$pca.top.variant
    
    content <- tagList(
      h2("Input"),
      tags$p(paste0(ncol(dataset()$readcounts.top.variant), " points with ", nrow(dataset()$readcounts.top.variant), " dimensions")),
      h2("PCA parameters"),
      tags$p(paste0("Center data: ", pca$params$center)),
      tags$p(paste0("Scale data: ", pca$params$scale)),
      h2("Output parameters"),
      tags$p(paste0("Relative transformed positions: ", pca$params$relative))
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
readcountProcessing.at.readcounts.processed <- function(dataset) {
  
  step.transpose <- readcountProcessing.step.transpose(dataset)
  step.remove.zero <- readcountProcessing.step.remove.zero(dataset)
  step.normalization <- readcountProcessing.step.normalization(dataset)
  step.remove.constant <- readcountProcessing.step.remove.constant(dataset)
  
  processingStepsWidgetData("readcounts.processing.steps",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant)
  processingStepsWidgetData("genes.variance.processing",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            readcountProcessing.step.calculate.variance())
  
}

#' Summary of read count processing at the point when all read counts have been filtered with annotation criteria
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.filtered <- function(dataset) {
  
  step.transpose <- readcountProcessing.step.transpose(dataset)
  step.remove.zero <- readcountProcessing.step.remove.zero(dataset)
  step.normalization <- readcountProcessing.step.normalization(dataset)
  step.remove.constant <- readcountProcessing.step.remove.constant(dataset)
  step.filter <- readcountProcessing.step.filter(dataset)
  
  processingStepsWidgetData("readcounts.filtered.steps",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter)
  processingStepsWidgetData("genes.variance.filtered.processing",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter,
                            readcountProcessing.step.calculate.variance())
  
}

#' Summary of read count processing at the point when all read counts have been filtered with annotation criteria
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.readcounts.top.variant <- function(dataset) {
  
  step.transpose <- readcountProcessing.step.transpose(dataset)
  step.remove.zero <- readcountProcessing.step.remove.zero(dataset)
  step.normalization <- readcountProcessing.step.normalization(dataset)
  step.remove.constant <- readcountProcessing.step.remove.constant(dataset)
  step.filter <- readcountProcessing.step.filter(dataset)
  step.top.variant <- readcountProcessing.step.top.variant(dataset)
  
  processingStepsWidgetData("readcounts.top.variant.steps",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter,
                            step.top.variant)
  
}

#' Summary of read count processing at the point when PCA is applied
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.pca <- function(dataset) {
  
  step.transpose <- readcountProcessing.step.transpose(dataset)
  step.remove.zero <- readcountProcessing.step.remove.zero(dataset)
  step.normalization <- readcountProcessing.step.normalization(dataset)
  step.remove.constant <- readcountProcessing.step.remove.constant(dataset)
  step.filter <- readcountProcessing.step.filter(dataset)
  step.top.variant <- readcountProcessing.step.top.variant(dataset)
  step.pca <- readcountProcessing.step.pca(dataset)
  
  processingStepsWidgetData("pca.pc.processing",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter,
                            step.top.variant,
                            step.pca)
  processingStepsWidgetData("pca.variance.processing",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter,
                            step.top.variant,
                            step.pca)
  processingStepsWidgetData("pca.transformed.processing",
                            readcountProcessing.step.input.readcounts(dataset),
                            step.transpose,
                            step.remove.zero,
                            step.normalization,
                            step.remove.constant,
                            step.filter,
                            step.top.variant,
                            step.pca)
  
}

#' Saves all steps as HTML
#'
#' @param filename 
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
readcountProcessing.at.pca.save <- function(filename, dataset) {
  
  step.transpose <- readcountProcessing.step.transpose(dataset)
  step.remove.zero <- readcountProcessing.step.remove.zero(dataset)
  step.normalization <- readcountProcessing.step.normalization(dataset)
  step.remove.constant <- readcountProcessing.step.remove.constant(dataset)
  step.filter <- readcountProcessing.step.filter(dataset)
  step.top.variant <- readcountProcessing.step.top.variant(dataset)
  step.pca <- readcountProcessing.step.pca(dataset)
  
  processingStepsWidget.exportHTML(filename, list(step.transpose,
                                                  step.remove.zero,
                                                  step.normalization,
                                                  step.remove.constant,
                                                  step.filter,
                                                  step.top.variant,
                                                  step.pca))
}
