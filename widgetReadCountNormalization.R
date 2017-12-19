#'
#' Defines widget that contains the functions to process read counts.
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("helpers.R")
source("defaultParameters.R")

readCountNormalizationUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    radioButtons(ns("normalization"),
                 helpIconText("Apply read count normalization", 
                              "If you already have normalized read counts, set this to 'None'.",
                              "Read count normalization"),
                 choices = supportedReadcountNormalizationTypes,
                 selected = default.data.normalization),
    conditionalPanel(conditionalPanel.equals(ns("normalization"), "'tpm'"),
                     checkboxInput(ns("normalization.tpm.effectivelength"), "Use effective fragment length", value = T),
                     checkboxInput(ns("normalization.tpm.exonlength"), "Use feature exon length", value = T)),
    conditionalPanel(conditionalPanel.equals(ns("normalization"), "'deseq2'"),
                     selectizeInput(ns("normalization.deseq2.conditions"), 
                                    helpIconText("Considered conditions", includeText('helptooltips/pca-data-readcounts-normalization-deseq2-conditions.md')), 
                                    choices = c(),
                                    multiple = T,
                                    options = list(plugins = list("remove_button", "drag_drop"))),
                     checkboxInput(ns("normalization.deseq2.rlog"), "rlog transformation", value = F),
                     actionButton(ns("normalization.deseq2.submit"), "Calculate"))
  ))
}

readCountNormalizationData_ <- function(input, 
                              output, 
                              session,
                              dataset) {
  
  stored.values <- reactiveValues(readcounts.normalized.cache = NULL)
  
  readcounts <- reactive({ 
    validate(need(dataset, "No processed read counts available!"))
    return(dataset()$readcounts.preprocessed)
  })
  sample.annotation <- reactive({ 
    validate(need(dataset, "No sample annotation available!"))
    return(dataset()$sample.annotation)
    })
  gene.annotation <- reactive({ 
    validate(need(dataset, "No sample annotation available!"))
    return(dataset()$gene.annotation)
  })
  
  # Extract the sample conditions from the sample annotation
  conditions <- reactive({
    validate(need(sample.annotation(), "No samples annotation available!"))
    return(sample.annotation()@conditions)
  })
  
  # Use the conditions to update the DESeq normalization UI
  observeEvent(conditions(), {
    
    validate(need(conditions(), "No sample conditions available!"))
    
    updateSelectizeInput(session, 
                         "normalization.deseq2.conditions",
                         choices = colnames(conditions()),
                         selected = colnames(conditions()))
  })
  
  # Reset the cache if anything changes
  observeEvent({ readcounts() 
    gene.annotation() 
    sample.annotation() 
    input$normalization
    input$normalization.deseq2.conditions 
    input$normalization.deseq2.rlog}, {
      stored.values$readcounts.normalized.cache <- NULL
    })
  
  readcounts.normalized <- reactive({
    
    validate(need(readcounts(), "[Read count processing] No read counts to process!"))
    
    if(input$normalization == "tpm") {
      
      validate(need(sample.annotation(), "[Read count processing] No sample annotation available!"),
               need(gene.annotation(), "[Read count processing] No gene annotation available!"))
      
      return(applyReadcountNormalization.TPM(readcounts = readcounts(), 
                                              use.feature.exonlength = input$normalization.tpm.exonlength,
                                              use.feature.effectivelength = input$normalization.tpm.effectivelength,
                                              gene.annotation = gene.annotation(),
                                              sample.annotation = sample.annotation()))
      
    } else if(input$normalization == "deseq2") {
      
      validate(need(stored.values$readcounts.normalized.cache, "Click 'Calculate' to normalize the read count data!"))
      
      # DESeq needs a long time to fit its models.
      # Use a cache instead
      return(stored.values$readcounts.normalized.cache)
      
    } else {
      return(list(readcounts = readcounts(), operation.normalization = "none"))
    }
    
  })
  
  # If the user clicks on "Calculate", let DESeq calculate the conditions
  observeEvent(input$normalization.deseq2.submit, {
    
    if(!sampleAnnotationHasConditions(sample.annotation())) {
      showNotification("Sample annotation has no conditions!", type = "error", duration = NULL)
      return()
    }
    if(is.null(readcounts())) {
      showNotification("No read counts to process!", type = "error", duration = NULL)
      return()
    }
    if(length(input$normalization.deseq2.conditions) == 0) {
      showNotification("No conditions selected!", type = "error", duration = NULL)
      return()
    }
  
    shinyjs::disable("normalization.deseq2.submit")
    progress <- progressNotification("Building DESeq2 data. This will take some time ...")
    
    parallel.expression <- function() {
      tryCatch({

          transform <- "none"

          if(input$normalization.deseq2.rlog) {
            transform <- "rlog"
          }

          return(applyReadcountNormalization.DESeq2(readcounts = readcounts(),
                                                    transform = transform,
                                                    sample.annotation = sample.annotation(),
                                                    selected.conditions = input$normalization.deseq2.conditions))
          },
        error = function(e){
          showNotification(paste(e), type = "error", duration = NULL)
        })
      
      return(NULL)
    }
    
    withParallel(session, input, expr = parallel.expression(),
    exprsuccess = function(result) {
      
      if(!is.null(result)) {
        
        stored.values$readcounts.normalized.cache <-result
      }
      
    },
    exprfinally = function() {
      shinyjs::enable("normalization.deseq2.submit")
      removeNotification(progress)
    })
    
  })
  
  return(reactive({ 
    dataset <- dataset()
    dataset$readcounts.normalized <- readcounts.normalized()$readcounts
    dataset$readcounts.normalization.parameters <- readcounts.normalized()
    
    return(dataset)
    }))
  
}

readCountNormalizationData <- function(id, dataset) {
  
  return(callModule(readCountNormalizationData_,
                    id,
                    dataset = dataset))
  
}