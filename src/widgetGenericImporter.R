#'
#' Contains an importer widget that allows the user to upload a file,
#' input the data manually or choose a sample data set
#'

library(shiny)
library(shinyBS)
library(shinyjs)
source("uiHelper.R")
source("classImporterEntry.R")
source("helpers.R")

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
  
  tags <- tags$div(class = "generic-importer", 
                   verticalLayout(
    radioButtons(ns("source"), "Load from ...", c("No available" = "none"), selected = "upload"),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'manual'"), 
                     textAreaInput(ns("input"), "Manual input")),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'upload'"), 
                     fileInput(ns("fileinput"), "Upload file")),
    conditionalPanel(paste(conditionalPanel.equals(ns("source"), "'manual'"), "||", conditionalPanel.equals(ns("source"), "'upload'")), 
                     selectInput(ns("importer"), "Importer", c())),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'sample'"), 
                     selectInput(ns("sample"), "Example data", c())),
    conditionalPanel(conditionalPanel.equals(ns("source"), "'generate'"), 
                     selectInput(ns("generator"), "Generator", c())),
    subSubBox(uiOutput(ns("parameter.slot1")),
              uiOutput(ns("parameter.slot2")),
              uiOutput(ns("parameter.slot3")),
              uiOutput(ns("parameter.slot4"))),
    tags$div(
      id = ns("integration-panel"),
      hDivider(),
      selectizeInput(ns("integration.data"), options = list(plugins = list("remove_button")), multiple = T, choices = c(), label = "Currently imported data"),
      uiOutput(ns("integration.status"))
    ),
    fluidPage(fluidRow(
       actionButton(ns("submit"), submit.button.text),
       actionButton(ns("reset"), reset.button.text))
    )))
    
  return(tags)
    
}

#' Resolves an ImportParameter option
#'
#' @param option ImportParameter option
#' @param label Label of the ImporterEntry
#' @param ... Arguments if the parameter is a function
#'
#' @return
#' @export
#'
#' @examples
genericImporterData.resolveParameterInputParameterOption <- function(option, label, ...) {
  
  if(is.reactive(option)) {
    
    notification.id <- progressNotification(paste("Loading available parameters for", label))
    option <-  tryCatch(option(), error = function(e) { c() })
    removeNotification(notification.id)
  }
  else if(is.function(option)) {
    
    notification.id <- progressNotification(paste("Loading available parameters for", label))
    option <- tryCatch(option(...), error = function(e) { c() })
    removeNotification(notification.id)
  }
  
  return(option)
}

#' Builds UI for one parameter slot
#'
#' @param ns Session namespace
#' @param id ID of the control
#' @param param Importer parameter
#' @param ... Arguments that are passed to the option if it's a function
#'
#' @return
#' @export
#'
#' @examples
genericImporterData.makeParameterInputUI <- function(ns, id, param, ...) {
  
  if(param@type == "select") {
    
    select.values <- genericImporterData.resolveParameterInputParameterOption(param@select.values, param@label, ...)
    
    return(selectizeInput(ns(id), 
                          label = param@label, 
                          choices = select.values,
                          options = list(
                            maxOptions = 1000000
                          )))
    
  } 
  else if(param@type == "checkboxes") {
    
    checkboxes.options <- genericImporterData.resolveParameterInputParameterOption(param@checkboxes.options, param@label, ...)
    checkboxes.selected <- genericImporterData.resolveParameterInputParameterOption(param@checkboxes.selected, param@label, ...)
    
    return(checkboxGroupInput(ns(id),
                              label = param@label,
                              choices = checkboxes.options,
                              selected = checkboxes.selected))
    
  }
  else if(param@type == "lineedit") {
    
    lineedit.default <- genericImporterData.resolveParameterInputParameterOption(param@lineedit.default, param@label, ...)
    
    return(textInput(ns(id),
                     label = param@label,
                     value = lineedit.default))
    
  }
  else if(param@type == "checkbox") {
    
    return(checkboxInput(ns(id),
                        label = param@label,
                        value = param@checkbox.selected))
    
  }
  else {
    stop(paste("Unsupported type", param@type))
  }
  
}

genericImporterData.makeParameterInput <- function(input, ns, importer.object, slot.index) {
  return(renderUI({
    
    if(!is.null(importer.object())) {
      
      output <- tagList()
      
      if(length(importer.object()@parameters) >= slot.index)
      {
        param <- importer.object()@parameters[[slot.index]]
        ui.id <- paste0("parameter.slot", slot.index, ".value")
        
        parameters <- list(ns,
                           ui.id,
                           param)
        
        # Add the importer values from previous slots
        if(slot.index > 1) {
          for(i in 1:(slot.index - 1)) {
            parameters[[length(parameters) + 1]] <- input[[paste0("parameter.slot", i,".value")]]
          }
        }
        
        return(do.call(genericImporterData.makeParameterInputUI, parameters))
      }
      
    }
    
    return(tagList())
  }))
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
#' @param exprintegrate If not NULL, the widget will hold multiple data that then will be integrated with this function
#' @param xauto IF not NULL, this must be a function that returns a list(source = "sample", sample = <>, parameters = <>, clear = <T,F>) (Currently only supports importing samples)
#' 
#' exprintegrate: This function generates the output of this control it has following parameters:
#' * data: List of data values to integrate
#' * callback: Function with parameters choices, selected: Changes the list of collected data for user feedback
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
                                 exprgenerator,
                                 exprintegrate = NULL,
                                 xauto = NULL,
                                 parallel = F,
                                 parallel.message = "Currently importing the data. Please wait.") {
  
  if(!is.reactive(importers) || !is.reactive(samples) || !is.reactive(generators)) {
    stop("Invalid arguments!")
  }
  
  variables <- reactiveValues(data = list(),
                              data.labels = list(),
                              data.counter = 1,
                              status.choices = c(), 
                              status.selected = c())
  
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
      sources <- c(sources, "example data" = "sample")
      
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
  
  output$parameter.slot1 <- genericImporterData.makeParameterInput(input, session$ns, importer.object, 1)
  output$parameter.slot2 <- genericImporterData.makeParameterInput(input, session$ns, importer.object, 2)
  output$parameter.slot3 <- genericImporterData.makeParameterInput(input, session$ns, importer.object, 3)
  output$parameter.slot4 <- genericImporterData.makeParameterInput(input, session$ns, importer.object, 4)
  
  # Reset data to NULL if reset button is clicked
  
  reset.data <- function() {
    variables$data <- list()
    variables$data.labels <- list()
    showNotification("Data has been reset.", type = "message")
  }
  
  observeEvent(input$reset, {
    reset.data()
  })
  
  # For QOL improvement
  addData <- function(data, label) {
    
    variables$data.counter <- variables$data.counter + 1
    variables$data[[length(variables$data) + 1]] <- data
    
    # Set label
    data.labels <- variables$data.labels
    data.labels[[length(data.labels) + 1]] <- paste("item", variables$data.counter)
    names(data.labels)[length(data.labels)] <- label
    variables$data.labels <- data.labels
    
  }
  
  # xauto importer that allows triggering of importing samples from code
  if(!is.null(xauto)) {
    observeEvent(xauto(), {
      
      notification.id <- progressNotification("Please wait ... importing data")
      shinyjs::disable("submit")
      on.exit({ 
        shinyjs::enable("submit")
        removeNotification(id = notification.id) 
      })
      
      # Allow clearing of data
      if(xauto()$clear) {
        reset.data()
      }
      
      if(xauto()$source == "sample") {
        sample <- xauto()$sample
        parameters <- xauto()$parameters
        
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
          
          addData(data, paste0("Example: ", sample))
          
          showNotification(paste("Loaded example", sample), type = "message")
        } 
        else {
          showNotification("Error while importing example!", type = "error")
        }
      }
      else {
        stop("Not supported")
      }
      
    })
  }
  
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
      param.name <- importer.object()@parameters[[i]]@name
      parameters[param.name] <- list(input[[input.id]])
    }
    
    # Run the importers
    
    # Reset the old variable so we 100% trigger a change
    # Do this only if we are not integrating
    if(is.null(exprintegrate)) {
      variables$data <- list()
      variables$data.labels <- list()
    }
    
    expr.import.data <- NULL
    
    if(input$source == "upload") {
      
      expr.import.data <- function() {
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
            return(list(data = data, description = paste0("Upload: ", basename(inFile$name))))
          } 
          else {
            return(NULL)
          }
          
        }
        else
        {
          showNotification("Error while importing the data: No file uploaded!", type = "error")
          return(NULL)
        }
      }
        
    }
    else if(input$source == "manual") {
      
      expr.import.data <- function() {
       
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
            return(list(data = data, description = "Manual input"))
          } 
          else {
            return(NULL)
          }
      }
      
    }
    else if(input$source == "sample") {
      
      expr.import.data <- function() {
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
          return(list(data = data, description = paste("Example:", sample)))
        } 
        else {
          return(NULL)
        }
      }
      
    }
    else if(input$source == "generate") {
      
      expr.import.data <- function() {
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
          return(list(data = data, description = paste("Generated:", generator)))
        } 
        else {
          return(NULL)
        }
      }
      
    }
    
    # Parallel or non-parallel
    if(!parallel) {
      result <- expr.import.data()
      if(!is.null(result)) {
        showNotification("Import successful!", type = "message")
        addData(result$data, result$description)
      }
      else {
        showNotification("Error while importing data!", type = "error")
      }
    }
    else {
      withParallel(session, input, expr = expr.import.data(),
                   exprsuccess = function(result) {
                     if(!is.null(result)) {
                       showNotification("Import successful!", type = "message")
                       addData(result$data, result$description)
                     }
                     else {
                       showNotification("Error while importing data!", type = "error")
                     }
                   },
                   message = parallel.message)
    }

  })
  
  # Hide whole integration panel if no integration is enabled
  if(is.null(exprintegrate)) {
    shinyjs::hide("integration-panel")
  }
  
  # Output the callback from the integration
  output$integration.status <- renderUI({
    
    if(is.null(exprintegrate)) {
      return(tagList())
    }
    
    tag <- tags$div(
      class = "integration-status"
    )
    
    choices <- variables$status.choices
    choices.names <- if(is.null(names(choices))) choices else names(choices)
    
    # Have to use numeric for as I cannot preserve that damn name /:
    if(length(choices) > 0) {
      for(i in 1:length(choices)) {
        
        choice <- choices[i]
        choice.name <- choices.names[i]
        
        tag.icon <- if(choice %in% variables$status.selected) icon("check-square-o") else icon("square-o")
        tag <- tagAppendChild(tag, tags$div(tags$span(tag.icon), tags$span(choice.name)))
      }
    }
    
    tag <- tagAppendChild(tag, tags$div(class = "data-sets",paste(length(variables$data), "data sets loaded.")))
    
    return(tag)
  })
  
  # Update the labels in the "current data" selectize
  observeEvent(variables$data.labels, {
    updateSelectizeInput(session, "integration.data", choices = variables$data.labels, selected = variables$data.labels)
  })
  
  # Let the user remove data by removing labels
  observeEvent(input$integration.data, ignoreNULL = F, {
    
    # We have the selected data ids. Find data ids that are not in the selection. Then remove those from the list.
    selected.indices <- isolate(variables$data.labels) %in% input$integration.data
        
    variables$data <- variables$data[selected.indices]
    variables$data.labels <- variables$data.labels[selected.indices]
    
  })
  
  
  # Define a callback function for integrate
  integration.callback <- function(choices, selected) {
    variables$status.choices <- choices
    variables$status.selected <- selected
  }
  
  # The data returned is generated by exprintegrate. It has a callback-function that lets the user see which data has been integrated
  integrated.data <- reactive({
    
    if(is.null(exprintegrate)) {
      if(length(variables$data) == 0) {
        return(NULL)
      }
      else {
        return(variables$data[[1]])
      }
    }
    else {
      
      output <- tryCatch({exprintegrate(data = variables$data, callback = integration.callback)}, 
                         error = function(e){
                           # Note: Notifications only when user acts. Integration can be triggered by reactive and thus not only by user.
                           #showNotification(paste("[Integration] Error:", e), type = "error", duration = NULL)
                           
                           validate(need(F, paste("Error while integrating data:", e$message)))
                           
                           return(NULL)
                         }, 
                         warning = function(w)
                         {
                           #showNotification(paste("[Integration] Warning:", w), type = "warning", duration = NULL)
                           
                           validate(need(F, paste("Error while integrating data:", w$message)))
                           
                           return(NULL)
                         })
      return(output)
      
    }
    
  })
  
  return(integrated.data)
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
#' @param exprintegrate If not NULL, the widget will hold multiple data that then will be integrated with this function
#' @param xauto IF not NULL, this must be a function that returns a list(source = "sample", sample = <>, parameters = <>, clear = <T,F>) (Currently only supports importing samples)
#' 
#' exprintegrate: This function generates the output of this control it has following parameters:
#' * data: List of data values to integrate
#' * callback: Function with parameters choices, selected: Changes the list of collected data for user feedback
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
                                exprgenerator,
                                exprintegrate = NULL,
                                xauto = NULL,
                                parallel = F,
                                parallel.message = "Currently importing the data. Please wait.") {
  
  return(callModule(genericImporterData_, 
                    id, 
                    importers = importers,
                    samples = samples,
                    generators = generators,
                    exprimport = exprimport, 
                    exprsample = exprsample, 
                    exprgenerator = exprgenerator,
                    exprintegrate = exprintegrate,
                    xauto = xauto,
                    parallel = parallel,
                    parallel.message = parallel.message))
  
}