#' 
#' Widget that lets the user select entries from a list based on the label
#' This is used to filter genes by the features, as they are stored in feature -> list of genes 
#' 

library(shiny)

#' Creates a widget that lets the user select list entries based on the label
#'
#' @param id 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
filterSelectionInput <- function(id, header = "") {
  
  ns  <- NS(id)
  
  return(tags$div(class = "filter-selection-input",
                  tags$label(header),
                  fixedRow(
                    column(8, tags$div(class = "filter-values", selectizeInput(ns("values"), 
                                                                               label = "", 
                                                                               multiple = T,
                                                                               choices = c("All (*)" = "*"),
                                                                               selected = c("*")))),
                    column(4, tags$div(class = "filter-operation",selectizeInput(ns("operation"), 
                                                                                 label = "", 
                                                                                 choices = c("AND", "OR"),
                                                                                 selected = "OR")))
                  ),
    checkboxInput(ns("invert.selection"), "Invert selection")
  ))
  
}

#' Gets the selected list entries.
#' They are stored in label -> vector of strings fashion
#' 
#' The user can select a list of labels that should be selected and which operation should be done
#' The output is then the associated list of genes where the operations are applied.
#' 
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param values Reactive list of entries. Each list entry has a label and contains a vector of strings.
#'
#' @return Values selected by user (reactive)
#' @export
#'
#' @examples
filterSelectionValues_ <- function(input, output, session, values) {
  
  observeEvent(values(), {
    updateSelectInput(session, "values", choices = c("All (*)" = "*", names(values())), selected = c("*"))
  })
  
  selected.values <- reactive({
    
    selected.keys <- input$values
    
    # Do this to prevent breaking the axis selectize inputs
    if(length(selected.keys) == 0) {
      selected.keys <- names(values())
    }
    
    # Handle "Select All" case
    if("*" %in% selected.keys) {
      selected.keys <- names(values())
    }
    
    #' Depending on the user's selection apply union (OR) or intersect (AND) to 
    #' the list of strings in the values vector
    #' If the user wants to invert the selection, just calculate ALL_STRINGS SET_DIFFERENCE USERSELECTED_STRINGS
    
    selected.strings <- c()
    
    if(input$operation == "AND") {
      selected.strings <- Reduce(intersect, values()[selected.keys])
    }
    else if(input$operation == "OR") {
      selected.strings <- Reduce(union, values()[selected.keys])
    }
    
    if(input$invert.selection) {
      all.strings <- Reduce(union, values())
      selected.strings <- setdiff(all.strings, selected.strings)
    }
    
    return(selected.strings)
    
  })
  
  return(selected.values)
  
}

#' Gets the selected list entries.
#' They are stored in label -> vector of strings fashion
#' 
#' The user can select a list of labels that should be selected and which operation should be done
#' The output is then the associated list of genes where the operations are applied.
#'
#' @param id 
#' @param values 
#'
#' @return
#' @export
#'
#' @examples
filterSelectionValues <- function(id, values) {
  return(callModule(filterSelectionValues_,
                    id,
                    values = values))
}