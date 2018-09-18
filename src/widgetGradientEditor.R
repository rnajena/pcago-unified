#' 
#' Contains a widget that allows the user to create gradients associated 
#' to a number
#' 

library(shiny)
library(RColorBrewer)
source("uiHelper.R")
source("widgetGenericImporter.R")
source("gradients.R")


#' Creates a widget that allows editing of visual parameters of a gradient
#'
#' @param id ID of the widget
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
gradientEditorUI <- function(id) {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  return(tagList(tags$div(class="color-shape-editor",
                          headerPanel(header = tags$span(
                            downloadButton(ns("export.csv"), "Export *.csv"),
                            bsButton(ns("import"), "Import", icon = icon("upload"), type = "toggle")
                          ),
                          conditionalPanel(conditionalPanel.equals(ns("import"), "true"), 
                                           genericImporterInput(ns("importer"))),
                          conditionalPanel(conditionalPanel.equals(ns("import"), "false"), 
                                           tags$div(
                                             id="condition-list",
                                             class="condition-list",
                                             radioButtons(ns("gradientstops"),
                                                          "Gradient stops",
                                                          choices = c("None"))),
                                           numericInput(ns("value"), "Value", value = 0),
                                           colourInput(ns("color"), "Color", allowTransparent = F),
                                           hDivider(),
                                           fluidPage(
                                           fluidRow(actionButton(ns("add"), "Add stop", icon = icon("plus")),
                                                    actionButton(ns("remove"), "Delete this stop", icon = icon("trash")))))
                          
                          ))))
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'#'
#' @return
#' @export
#'
#' @examples
gradientEditorValue_ <- function(input, output, session, default.gradient) {
  
  variables <- reactiveValues(visuals.table = default.gradient, update.ui = 0)
  
  # Initial radio button creation
  init.default.gradientstops <- function() {
   
      values <- 1:nrow(default.gradient)
      names <- sapply(1:nrow(default.gradient), function(x) paste("Stop", x) )
      
      updateRadioButtons(session, "gradientstops", 
                         choiceNames = names,
                         choiceValues = values)
      
  }
  
  init.default.gradientstops()
  
  # Sends JS to update the colors
  update.ui <- function() {
    validate(need(variables$visuals.table, "[Visuals editor] No visual table!"))
    
    js <- c()
    control.name <- session$ns("gradientstops")
    
    for(index in seq_len(nrow(variables$visuals.table))) {
      color <- variables$visuals.table[index, "color"]
      
      #' Calculate the best text color (or a good one at least)
      #' We just take the mean of the inverse color and binarize it 
      inv.color.value <- mean(255 - col2rgb(color))
      inv.color.value <- if(inv.color.value < 255/2) 0 else 255
      inv.color <- rgb(inv.color.value, inv.color.value, inv.color.value, maxColorValue = 255)
      
      # Build linear gradient for fancy
      if(nrow(variables$visuals.table) > 1) {
        if(index == 1) {
          
        }
        else if(index == nrow(variables$visuals.table)) {
          
        }
        else {
          prev.color <- variables$visuals.table[index - 1, "color"]
          next.color <- variables$visuals.table[index + 1, "color"]
          color <- sprintf("linear-gradient(90deg, %s, %s, %s)", prev.color, color, next.color)
        }
      }
      
      js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"background\", \"%s\")", index, control.name, color))
      js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"color\", \"%s\")", index, control.name, inv.color))
      js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"border\", \"%s\")", index, control.name, "2px solid #555"))
      
    }
    
    if(length(js) > 0) {
      shinyjs::runjs(paste(js, collapse = "; "))
    }
  }
  
  # Builds the radio buttons from the table
  update.gradientstops <- function() {
    if(nrow(variables$visuals.table) == 0) {
      browser()
      updateRadioButtons(session, "gradientstops", choices = c("None"))
    }
    else {
      
      values <- 1:nrow(variables$visuals.table)
      names <- sapply(1:nrow(variables$visuals.table), function(x) paste("Stop", x) )
      
      updateRadioButtons(session, "gradientstops", 
                         choiceNames = names,
                         choiceValues = values)
      update.ui()
    }
  }
  
  
  
  # Based on selected choice update the current color and shape settings
  # Its own function as this is also used on importing
  # ! Only depend from conditions selection otherwise 
  observeEvent({ 
    input$gradientstops
    variables$update.ui
    },{
      
      validate(need(variables$visuals.table, "[Gradient editor] No visual table to update input!"),
               need(input$gradientstops, "[Gradient editor] No current gradient stop!"))
      
      index <- input$gradientstops
      color <- variables$visuals.table[index, "color"]
      value <- variables$visuals.table[index, "value"]
      
      updateColourInput(session, "color", value = color)
      updateTextInput(session, "value", value = value)
    
  })
  
  # Change color/shape based on input
  observeEvent(input$color, {
    
    if(input$gradientstops == "None") {
      return()
    }
    
    validate(need(variables$visuals.table, "No visual table to write to!"))
    
    index <- as.integer(input$gradientstops)
    color <- input$color
    
    variables$visuals.table[index, "color"] <- color
    update.ui()
    
  })
  
  observeEvent(input$value, {
    
    if(input$gradientstops == "None") {
      return()
    }
    
    validate(need(variables$visuals.table, "[Visuals editor] No visual table to write to!"))
    
    index <- as.integer(input$gradientstops)
    value <- input$value
    
    variables$visuals.table[index, "value"] <- value
    update.ui()
    
  })
  
  # Re-send JS if anything happens
  observeEvent({
    input$gradientstops
    variables$visuals.table
  }, {
    update.ui()
  })
  
  # Add/Remove stops
  observeEvent(input$add, {
    
    if(input$gradientstops == "None") {
      return()
    }
    
    # We recalculate the values for the whole table -> smooth
    data <- variables$visuals.table
    newvalues <- seq(from = min(data[, "value"]), to = max(data[, "value"]), by = (max(data[, "value"] - min(data[, "value"]))) / (nrow(data)))
    newcolors <- colorRampPalette(data[,"color"])(n = nrow(data) + 1)
    
    
    newdata <- data.frame(value = newvalues,
                          color = newcolors,
                          stringsAsFactors = F) 
    
    variables$visuals.table <- newdata
    update.gradientstops()
  })
  
  observeEvent(input$remove, {
    if(input$gradientstops == "None") {
      return()
    }
    data <- variables$visuals.table
    
    if(nrow(data) <= 2) {
      return()
    }
    
    index <- as.integer(input$gradientstops)
    newdata <- data[-index,]
    
    variables$visuals.table <- newdata
    update.gradientstops()
    
  })
  
  #' Handler for download of current visuals table
  output$export.csv <- downloadHandler(filename = "visuals.csv",
                                       content = function(file)
                                       {
                                         write.table(variables$visuals.table,
                                                     file,
                                                     sep = ",",
                                                     row.names = F,
                                                     col.names = T)
                                       })
  
  #' Handler for import of visuals table
  visual.table.imported <- genericImporterData("importer", 
                                               importers = reactive(supportedGradientImporters),
                                               samples = reactive(availableGradientSamples),
                                               generators = reactive(supportedGradientGenerators),
                                               exprimport = function(con, importer, parameters) {
                                                 return(importGradient(filehandle = con, 
                                                                               importer = importer, 
                                                                               parameters = parameters))
                                               },
                                               exprsample = function(sample, parameters) {
                                                 return(importGradientSample(sample = sample, 
                                                                                     parameters = parameters))
                                               })
  
  #' When the user imports a visual table, apply it
  observeEvent(visual.table.imported(), {
    
    if(!is.null(visual.table.imported())) {
      variables$update.ui <- isolate(variables$update.ui) + 1
      variables$visuals.table <- visual.table.imported()
      update.gradientstops()
    }
    
  })
  
  return(reactive({
    
      if(is.null(variables$visuals.table))  {
        return(NULL)
      }
    
      validate(need(isStrictlySorted(variables$visuals.table[, "value"]), "Gradient stop values are not strictly sorted!"))
      return(variables$visuals.table)
      
    }))
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#'
#' @param id Id of the UI element
#' @param conditions Reactive vector of conditions that are present in the plot
#'
#' @return
#' @export
#'
#' @examples
gradientEditorValue <- function(id, default.gradient) {
  return(callModule(gradientEditorValue_, id, default.gradient = default.gradient))
}