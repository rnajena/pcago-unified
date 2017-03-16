#' 
#' Contains a widget that allows the user to select a color or a shape
#' for a condition.
#' The user also can choose to ignore a condition by selecting "None"
#' 

library(shiny)
library(colourpicker)
library(RColorBrewer)
source("uiHelper.R")
source("conditions.R")
source("widgetGenericImporter.R")

colorShapeInput.shapes <- c(
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
colorShapeEditorInput <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(tags$div(class="color-shape-editor",
                          headerPanel(header = tags$span(
                            downloadButton(ns("export.csv"), "Export *.csv"),
                            bsButton(ns("import"), "Import", icon = icon("upload"), type = "toggle")
                          ),
                          conditionalPanel(conditionalPanel.equals(ns("import"), "true"), 
                                           genericImporterInput(ns("importer"), 
                                                                supportedConditionVisualsFileTypes, 
                                                                supportedConditionVisualsImporters)),
                          conditionalPanel(conditionalPanel.equals(ns("import"), "false"), 
                                           tags$div(
                                             id="condition-list",
                                             class="condition-list",
                                             radioButtons(ns("conditions"),
                                                          "Available conditions",
                                                          choices = c("None"))),
                                           textInput(ns("name"), "Custom name"),
                                           colourInput(ns("color"), "Color", value = "transparent", palette = "square", allowTransparent = T, transparentText = "No color"),
                                           selectizeInput(ns("shape"), "Shape", choices = colorShapeInput.shapes, selected = -1))
                          
                          ))))
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param conditions Reactive conditions table
#'
#' @return
#' @export
#'
#' @examples
colorShapeEditorValue_ <- function(input, output, session, conditions) {
  
  variables <- reactiveValues(visuals.table = NULL)
  
  # Get the current visual table or make a new one based on the current conditions
  visual.table <- reactive({ 
    
    validate(need(conditions(), "Cannot output visual parameters without conditions!"))
    
    if(is.null(variables$visuals.table) || !identical(rownames(variables$visuals.table), colnames(conditions()))) {
      variables$visuals.table <- generateDefaultConditionVisualsTable(colnames(conditions()))
    }
    
    return(variables$visuals.table)
    
  })

  # Change the list of choices when the conditions update
  observeEvent(conditions(), {
    
    validate(need(conditions(), "Cannot output visual parameters withoout conditions!"))
    
    updateRadioButtons(session, "conditions", choices = colnames(conditions()))
    
  })
  
  # Based on selected choice update the current color and shape settings
  # Its own function as this is also used on importing
  observe({
    
    validate(need(visual.table(), "No visual table to update input!"),
             need(input$conditions, "No current condition!"),
             need(input$conditions %in% rownames(visual.table()), "Condition not in visual table!"))
    
    condition <- input$conditions
    color <- visual.table()[condition, "color"]
    shape <- visual.table()[condition, "shape"]
    name <- visual.table()[condition, "name"]
    
    if(is.null(color) || color == "") {
      color <- "transparent"
    }
    if(!is.numeric(shape) || shape < 0) {
      shape <- -1
    }
    
    updateColourInput(session, "color", value = color)
    updateSelectizeInput(session, "shape", selected = shape)
    updateTextInput(session, "name", value = name)
    
  })
  
  #observeEvent(input$conditions, update.inputs)
  
  # Change color/shape based on input
  observeEvent(input$color, {
    
    validate(need(visual.table(), "No visual table to write to!"))
    
    condition <- input$conditions
    color <- input$color
    
    if(color == "transparent") {
      color <- ""
    }
  
    variables$visuals.table[condition, "color"] <- color
    
  })
  
  observeEvent(input$shape, {
    
    validate(need(visual.table(), "No visual table to write to!"))
    
    condition <- input$conditions
    shape <- as.numeric(input$shape)
    
    variables$visuals.table[condition, "shape"] <- shape
    
  })
  
  observeEvent(input$name, {
    
    validate(need(visual.table(), "No visual table to write to!"))
    
    condition <- input$conditions
    name <- input$name
    
    variables$visuals.table[condition, "name"] <- name
    
  })
  
  #' Handler for download of current visuals table
  output$export.csv <- downloadHandler(filename = "visuals.csv",
                                       content = function(file)
                                       {
                                         write.table(visual.table(),
                                                     file,
                                                     sep = ",",
                                                     row.names = T,
                                                     col.names = NA)
                                       })
  
  #' Handler for import of visuals table
  visual.table.imported <- genericImporterData("importer", exprimport = function(con, importer) {
    conditions <- colnames(isolate({conditions()}))
    imported <- importConditionVisuals(con, importer, conditions)
    return(imported)
  })
  
  #' When the user imports a visual table, apply it
  observeEvent(visual.table.imported(), {
    
    validate(need(visual.table.imported(), "Import went wrong! Won't modify table."))
    variables$visuals.table <- visual.table.imported()
    
  })
  
  return(visual.table)
  
}

#' Creates a condition visual mapping based on the settings the user specified in the input
#'
#' @param id Id of the UI element
#' @param conditions Reactive cell conditions definitions table
#'
#' @return
#' @export
#'
#' @examples
colorShapeEditorValue <- function(id, conditions) {
  return(callModule(colorShapeEditorValue_, id, conditions = conditions))
}