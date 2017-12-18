#' 
#' Contains a widget that allows the user to select a color or a shape
#' for a condition.
#' The user also can choose to ignore a condition by selecting "None"
#' 

library(shiny)
library(RColorBrewer)
source("uiHelper.R")
source("sampleAnnotation.R")
source("sampleAnnotationVisuals.R")
source("widgetGenericImporter.R")
source("widgetOptionalColorInput.R")

visualsEditor.shapes <- c(
  "No shape" = -1,
  "● Filled circle" = 16,
  "■ Filled square" = 15,
  "▲ Filled triangle" = 17,
  "◆ Filled diamond" = 18,
  "⬤ Large filled circle" = 19,
  "⦁ Small filled circle" = 20,
  "□ Box" = 0,
  "○ Circle" = 1,
  "△ Triangle" = 2,
  "+ Plus" = 3,
  "⨯ Cross" = 4,
  "◇ Diamond" = 5,
  "▽ Down pointing triangle" = 6,
  "⊠ Box with cross" = 7,
  "✳ Asterisk" = 8,
  "◇ Diamond with cross" = 9,
  "⊕ Circle with plus" = 10,
  "✡ Up- and down pointing triangle" = 11,
  "⊞ Box with plus" = 12,
  "⊗ Circle with cross" = 13,
  "⟎ Box with down pointing triangle" = 14,
  "● Filled circle BGR" = 21,
  "■ Filled box BGR" = 22,
  "◆ Filled diamond BGR" = 23,
  "▲ Filled triangle BGR" = 24,
  "▼ Filled down pointing triangle BGR" = 25
)

#' Creates a widget that allows editing of visual parameters of a condition visual mapping table.
#'
#' @param id ID of the widget
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
visualsEditorUI <- function(id) {
  
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
                                             radioButtons(ns("conditions"),
                                                          "Available conditions",
                                                          choices = c("None"))),
                                           textInput(ns("name"), "Custom name"),
                                           optionalColorInput(ns("color"), "Color"),
                                           selectizeInput(ns("shape"), "Shape", choices = visualsEditor.shapes, selected = -1))
                          
                          ))))
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param conditions Reactive vector of conditions that are present in the plot
#'
#' @return
#' @export
#'
#' @examples
visualsEditorValue_ <- function(input, output, session, conditions, has.color = T, has.shape = T) {
  
  # Define which data this widget will request from an annotation
  expected.columns <- "name"
  if(has.color) { expected.columns <- c(expected.columns, "color") }
  if(has.shape) { expected.columns <- c(expected.columns, "shape") }
  
  # Disable inputs that don't matter
  observe({
    if(!has.color) { shinyjs::hideElement("color") }
    if(!has.shape) { shinyjs::hideElement("shape") }
  })
  
  variables <- reactiveValues(visuals.table = NULL, update.ui = 0)

  # Change the list of choices and visual table when the conditions update
  observeEvent(conditions(), {
    validate(need(conditions(), "[Visuals editor] Cannot output visual parameters withoout conditions!"))
    updateRadioButtons(session, "conditions", choices = (conditions()))
    
    # Rebuild table if conditions changed
    if(is.null(variables$visuals.table) || !identical(rownames(variables$visuals.table), (conditions()))) {
      variables$visuals.table <- generateDefaultConditionVisualsTable(conditions(), 
                                                                      expected.columns = expected.columns)
    }
    
  })
  
  # Based on selected choice update the current color and shape settings
  # Its own function as this is also used on importing
  # ! Only depend from conditions selection otherwise 
  observeEvent({ 
    input$conditions
    variables$update.ui
    },{
      
      validate(need(variables$visuals.table, "[Visuals editor] No visual table to update input!"),
               need(input$conditions, "[Visuals editor] No current condition!"),
               need(input$conditions %in% rownames(variables$visuals.table), "[Visuals editor] Condition not in visual table!"))
      
      condition <- input$conditions
      color <- variables$visuals.table[condition, "color"]
      shape <- variables$visuals.table[condition, "shape"]
      name <- variables$visuals.table[condition, "name"]
   
      if(!is.numeric(shape) || shape < 0) {
        shape <- -1
      }
      
      updateOptionalColorInput("color", value = color)
      updateSelectizeInput(session, "shape", selected = shape)
      updateTextInput(session, "name", value = name)
    
  })
  
  # Change color/shape based on input
  color.input.value <- optionalColorInputValue("color")
  observeEvent(color.input.value(), {
    
    if(!has.color) {
      return()
    }
    
    validate(need(variables$visuals.table, "No visual table to write to!"))
    
    condition <- input$conditions
    color <- color.input.value()
    
    if(is.null(color)) {
      color <- ""
    }
  
    variables$visuals.table[condition, "color"] <- color
    #variables$update.ui <- variables$update.ui + 1
    
  })
  
  observeEvent(input$shape, {
    
    if(!has.shape) {
      return()
    }
    
    validate(need(variables$visuals.table, "[Visuals editor] No visual table to write to!"))
    
    condition <- input$conditions
    shape <- as.numeric(input$shape)
    
    variables$visuals.table[condition, "shape"] <- shape
    #variables$update.ui <- variables$update.ui + 1
    
  })
  
  observeEvent(input$name, {
    
    validate(need(variables$visuals.table, "[Visuals editor] No visual table to write to!"))
    
    condition <- input$conditions
    name <- input$name
    
    variables$visuals.table[condition, "name"] <- name
    #variables$update.ui <- variables$update.ui + 1
    
  })
  
  # Observe the table and change the radio buttons with JS
  observeEvent({
    variables$visuals.table
    input$conditions
    }, {
    
    validate(need(variables$visuals.table, "[Visuals editor] No visual table!"))
    
    js <- c()
    control.name <- session$ns("conditions")
    
    for(condition in rownames(variables$visuals.table)) {
      
      if(has.color)
      {
        color <- variables$visuals.table[condition, "color"]
        
        if(color == "") {
          color <- "white"
        }
        
        #' Calculate the best text color (or a good one at least)
        #' We just take the mean of the inverse color and binarize it 
        inv.color.value <- mean(255 - col2rgb(color))
        inv.color.value <- if(inv.color.value < 255/2) 0 else 255
        inv.color <- rgb(inv.color.value, inv.color.value, inv.color.value, maxColorValue = 255)
        
        js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"background-color\", \"%s\")", condition, control.name, color))
        js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"color\", \"%s\")", condition, control.name, inv.color))
      }
      
      if(has.shape) {
        shape <- variables$visuals.table[condition, "shape"]
        js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"border\", \"%s\")", condition, control.name, "2px solid #555"))
        
        if(shape >= 0) {
          js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"border-left\", \"%s\")", condition, control.name, "10px solid #555"))
        }
        
      }
      else {
        js <- c(js, sprintf("$(\"input[value='%s'][name='%s']\").next().css(\"border\", \"%s\")", condition, control.name, "2px solid #555"))
      }
    }
    
    if(length(js) > 0) {
      shinyjs::runjs(paste(js, collapse = "; "))
    }
    
  })
  
  
  
  #' Handler for download of current visuals table
  output$export.csv <- downloadHandler(filename = "visuals.csv",
                                       content = function(file)
                                       {
                                         write.table(variables$visuals.table,
                                                     file,
                                                     sep = ",",
                                                     row.names = T,
                                                     col.names = NA) #TODO export legends
                                       })
  
  #' Handler for import of visuals table
  visual.table.imported <- genericImporterData("importer", 
                                               importers = reactive(supportedConditionVisualsImporters),
                                               samples = reactive(availableConditionVisualSamples),
                                               generators = reactive(supportedConditionVisualsGenerators),
                                               exprimport = function(con, importer, parameters) {
                                                 return(importConditionVisuals(filehandle = con, 
                                                                               importer = importer, 
                                                                               parameters = parameters, 
                                                                               conditions = conditions(), 
                                                                               expected.columns = expected.columns))
                                               },
                                               exprsample = function(sample, parameters) {
                                                 return(importConditionVisualsSample(sample = sample, 
                                                                                     parameters = parameters, 
                                                                                     conditions = conditions(), 
                                                                                     expected.columns = expected.columns))
                                               })
  
  #' When the user imports a visual table, apply it
  observeEvent(visual.table.imported(), {
    
    if(!is.null(visual.table.imported())) {
      variables$update.ui <- isolate(variables$update.ui) + 1
      variables$visuals.table <- visual.table.imported()
    }
    
  })
  
  return(reactive(variables$visuals.table))
  
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
visualsEditorValue <- function(id, conditions, has.color = T, has.shape = T) {
  return(callModule(visualsEditorValue_, id, conditions = conditions, has.color = has.color, has.shape = has.shape))
}