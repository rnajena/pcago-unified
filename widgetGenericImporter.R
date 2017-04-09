#'
#' Contains an importer widget that allows the user to upload a file,
#' input the data manually or choose a sample data set
#'

library(shiny)
library(shinyBS)
library(shinyjs)
source("uiHelper.R")
source("classImporterEntry.R")

#' Creates a generic import widget UI
#'
#' @param id Id of the control
#' @param reset.button Show reset button
#' @param additional.buttons Additional buttons in the bottom
#' @param additional.content Additional content after the combo box
#'
#' @return Shiny UI controls
#' @export
#'
#' @examples
genericImporterInput <- function(id, 
                                 submit.button.text = "Submit", 
                                 reset.button.text = "Reset",
                                 reset.button = T, 
                                 additional.buttons = tagList(),
                                 additional.content = tagList()) {
  
  ns <- NS(id)
  
  tags <- tagList(verticalLayout(
    radioButtons(ns("source"), "Load from ...", c("No available" = "none"), selected = "upload"),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'manual'"), 
                     textAreaInput(ns("input"), "Manual input")),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'upload'"), 
                     fileInput(ns("fileinput"), "Upload file")),
    conditionalPanel(paste(conditionalPanel.equals(ns("source"), "'manual'"), "||", conditionalPanel.equals(ns("source"), "'upload'")), 
                     selectInput(ns("importer"), "Importer", c())),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'sample'"), 
                     selectInput(ns("sample"), "Sample data", c())),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'generate'"), 
                     selectInput(ns("generator"), "Use generator", c())),
    uiOutput(ns("parameter.slot1")),
    uiOutput(ns("parameter.slot2")),
    uiOutput(ns("parameter.slot3")),
    additional.content,
    fluidPage(fluidRow(
       actionButton(ns("submit"), submit.button.text),
       if(reset.button) actionButton(ns("reset"), reset.button.text) else tagList(),
       additional.buttons)
    )))
    
  return(tags)
    
}

#' Builds UI for one parameter slot
#'
#' @param ns Session namespace
#' @param id ID of the control
#' @param param Importer parameter
#' @param ... Arguments that are passed to select.values if it's a function
#'
#' @return
#' @export
#'
#' @examples
genericImporterData.makeParameterInput <- function(ns, id, param, ...) {
  
  if(param@type == "select") {
    select.values <- param@select.values
    
    if(is.reactive(select.values)) {
      
      notification.id <- progressNotification(paste("Loading available parametes for", param@label))
      select.values <-  tryCatch(select.values(), error = function(e) { c() })
      removeNotification(notification.id)
    }
    else if(is.function(select.values)) {
      
      notification.id <- progressNotification(paste("Loading available parametes for", param@label))
      select.values <- tryCatch(select.values(...), error = function(e) { c() })
      removeNotification(notification.id)
      
    }
    
    return(selectizeInput(ns(id), 
                          label = param@label, 
                          choices = select.values,
                          options = list(
                            maxOptions = 1000000
                          )))
    
  } 
  else {
    stop(paste("Unsupported type", param@type))
  }
  
}

#' Server function of generic importer. Use within callModule and reactive context.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param importers Reactive list of ImporterEntry
#' @param samples Reactive list of ImporterEntry
#' @param generators Reactive list of ImporterEntry
#' @param exprimport The expression to be called if the submit button is clicked. Parameters are (connection, importer, parameters)
#' @param exprsample The expression to be called if the submit button is clicked, but the user wants to select a sample. Parameters are (sample, parameters)
#' @param exprgenerator install The expression to be called if the submit button is clicked, but the user wants to select a generator Parameters are (generator, parameters)
#'
#' @return Data imported by the importer
#' @export
#'
#' @examples
genericImporterData_ <- function(input, 
                                 output, 
                                 session, 
                                 importers,
                                 samples,
                                 generators,
                                 exprimport, 
                                 exprsample, 
                                 exprgenerator) {
  
  if(!is.reactive(importers) || !is.reactive(samples) || !is.reactive(generators)) {
    stop("Invalid arguments!")
  }
  
  variables <- reactiveValues(data = NULL)
  
  # Update the the UI based on available importers, generators & samples
  observe({ 
    
    sources <- c()
    
    # Update importer list (used for file + manual importing)
    if(length(importers()) > 0) {
      sources <- c(sources, "uploaded file" = "upload", "manual input" = "manual")
      
      choices <- lapply(importers(), function(x) { x@name })
      names(choices) <- lapply(importers(), function(x) { x@label })
      updateSelectizeInput(session, "importer", choices = choices)
    }
    else {
      updateSelectizeInput(session, "importer", choices = c())
    }
    
    # Update generator list
    if(length(samples()) > 0) {
      sources <- c(sources, "sample data" = "sample")
      
      choices <- lapply(samples(), function(x) { x@name })
      names(choices) <- lapply(samples(), function(x) { x@label })
      updateSelectizeInput(session, "sample", choices = choices)
    }
    else {
      updateSelectizeInput(session, "sample", choices = c())
    }
    
    # Update generator list
    if(length(generators()) > 0) {
      sources <- c(sources, "generate data" = "generate")
      
      choices <- lapply(generators(), function(x) { x@name })
      names(choices) <- lapply(generators(), function(x) { x@label })
      updateSelectizeInput(session, "generator", choices = choices)
    }
    else {
      updateSelectizeInput(session, "generator", choices = c())
    }
    
    # Radio button doesn't like empty choices
    if(length(sources) == 0) {
      sources <- c("No available" = "none")
    }
    
    # Hide choices that aren't supported
    updateRadioButtons(session, "source", choices = sources) 
  })
  
  # Returns the current selected importer object
  importer.object <- reactive({
    
    importer.object <- NULL
    
    if(input$source == "upload" || input$source == "manual") {
      importer.object <- Find(function(x) { x@name == input$importer }, importers())
    }
    else if(input$source == "sample") {
      importer.object <- Find(function(x) { x@name == input$sample }, samples())
    }
    else if(input$source == "generate") {
      importer.object <- Find(function(x) { x@name == input$generator }, generators())
    }
    
    return(importer.object)
    
  })
  
  #' Render the parameter slots
  #' We need a known amount of slots to make it possible to 
  #' build selection choices based on previous choices.
  #' Otherwise we would be forced to completely pre-occupy all possible selections
  
  output$parameter.slot1 <- renderUI({
    
    if(!is.null(importer.object())) {
      
      output <- tagList()
      ns <- session$ns
      
      if(length(importer.object()@parameters) >= 1)
      {
        param <- importer.object()@parameters[[1]]
        return(genericImporterData.makeParameterInput(ns, "parameter.slot1.value", param))
      }
      
    }
    
    return(tagList())
  })
  
  output$parameter.slot2 <- renderUI({
    
    if(!is.null(importer.object())) {
      
      output <- tagList()
      ns <- session$ns
      
      if(length(importer.object()@parameters) >= 2)
      {
        param <- importer.object()@parameters[[2]]
        return(genericImporterData.makeParameterInput(ns, "parameter.slot2.value", param, input$parameter.slot1.value))
      }
      
    }
    return(tagList())
  })
  
  output$parameter.slot3 <- renderUI({
    
    if(!is.null(importer.object())) {
      
      output <- tagList()
      ns <- session$ns
      
      if(length(importer.object()@parameters) >= 3)
      {
        param <- importer.object()@parameters[[3]]
        return(genericImporterData.makeParameterInput(ns, "parameter.slot3.value", param, input$parameter.slot1.value, input$parameter.slot2.value))
      }
      
    }
    return(tagList())
  })
  
  # Reset data to NULL if reset button is clicked
  observeEvent(input$reset, {
    
    variables$data <- NULL
    showNotification("Data has been reset.", type = "message")
    
  })
  
  # Import if the user clicks on the submit button
  observeEvent(input$submit, {
    
    notification.id <- progressNotification("Please wait ... importing data")
    
    shinyjs::disable("submit")
    on.exit({ 
      shinyjs::enable("submit")
      removeNotification(id = notification.id) 
      })
    
    # Generate parameters
    parameters <- list()
    
    for(i in seq_len(length(importer.object()@parameters))) {
      input.id <- paste0("parameter.slot", i, ".value")
      param.name <- importer.object()@parameters[[i]]
      parameters[[param.name]] <- input[[input.id]]
    }
    
    # Run the importers
    
    # Reset the old variable so we 100% trigger a change
    variables$data <- NULL
    
    if(input$source == "upload") {
      inFile <- input$fileinput
      importer <- input$importer
      
      if(!is.null(inFile)) {
        con <- file(inFile$datapath, "r")
        data <- tryCatch({exprimport(con, importer, parameters)}, 
                         error = function(e){
                           showNotification(paste(e), type = "error", duration = NULL)
                           return(NULL)
                           }, 
                         warning = function(w)
                           {
                           showNotification(paste(w), type = "warning", duration = NULL)
                           return(NULL)
                           })
        close(con)
        
        if(!is.null(data)) {
          showNotification("Data has been successfully imported.", type = "message")
        } 
        else {
          showNotification("Error while importing the data", type = "error")
        }
        
        variables$data <- data
      }
      else
      {
        showNotification("Error while importing the data: No file uploaded!", type = "error")
        return(NULL)
      }
        
    }
    else if(input$source == "manual") {
      importer <- input$importer
      con <- textConnection(input$input)
      data <- tryCatch({exprimport(con, importer, parameters)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      close(con)
      
      if(!is.null(data)) {
        showNotification("Data has been successfully imported.", type = "message")
      } 
      else {
        showNotification("Error while importing the data", type = "error")
      }
      
      variables$data <- data
    }
    else if(input$source == "sample") {
      
      sample <- input$sample
      
      data <- tryCatch({exprsample(sample, parameters)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      
      if(!is.null(data)) {
        showNotification(paste("Loaded sample", sample), type = "message")
      } 
      else {
        showNotification("Error while importing sample!", type = "error")
      }
      
      variables$data <- data
      
    }
    else if(input$source == "generate") {
      
      generator <- input$generator
      
      data <- tryCatch({exprgenerator(generator, parameters)}, 
                       error = function(e){
                         showNotification(paste(e), type = "error", duration = NULL)
                         return(NULL)
                       }, 
                       warning = function(w)
                       {
                         showNotification(paste(w), type = "warning", duration = NULL)
                         return(NULL)
                       })
      
      if(!is.null(data)) {
        showNotification(paste("Generated data using", generator), type = "message")
      } 
      else {
        showNotification("Error while importing sample!", type = "error")
      }
      
      variables$data <- data
      
    }

  })
  return(reactive( { variables$data } ))
}

#' Server function of generic importer. Use within callModule and reactive context.
#'
#' @param id UI element ID
#' @param importers Reactive list of ImporterEntry
#' @param samples Reactive list of ImporterEntry
#' @param generators Reactive list of ImporterEntry
#' @param exprimport The expression to be called if the submit button is clicked. Parameters are (connection, importer, parameters)
#' @param exprsample The expression to be called if the submit button is clicked, but the user wants to select a sample. Parameters are (sample, parameters)
#' @param exprgenerator The expression to be called if the submit button is clicked, but the user wants to select a generator Parameters are (generator, parameters)
#'
#' @return Data imported by the importer
#' @export
#'
#' @examples
genericImporterData <- function(id, 
                                importers,
                                samples,
                                generators,
                                exprimport, 
                                exprsample, 
                                exprgenerator) {
  
  return(callModule(genericImporterData_, 
                    id, 
                    importers = importers,
                    samples = samples,
                    generators = generators,
                    exprimport = exprimport, 
                    exprsample = exprsample, 
                    exprgenerator = exprgenerator))
  
}