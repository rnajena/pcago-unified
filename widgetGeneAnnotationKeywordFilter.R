#' 
#' Widget that lets the user select entries from a list based on the label
#' This is used to filter genes by the features, as they are stored in feature -> list of genes 
#' 
#' More specialized version of widgetFilterSelection
#' 

library(shiny)
library(shinyBS)
source("widgetGOTermFilter.R")

#' Creates a widget that lets the user select list entries based on the label
#'
#' @param id 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
geneAnnotationKeywordFilterInput <- function(id, header = "") {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns  <- NS(id)
  
  return(tagList(
    fluidPage(tags$div(class = "row", bsButton(ns("toggle.gobrowser"), "GO browser", icon = icon("code-fork"), type = "toggle"))),
    vSkip(5),
    uiOutput(ns("gobrowser.warning")),
    conditionalPanel(conditionalPanel.equals(ns("toggle.gobrowser"), "false"),
                     tags$div(class = "filter-selection-input",
                              tags$div(class = "filter-values", selectizeInput(ns("values"), 
                                                                               label = header, 
                                                                               multiple = T,
                                                                               choices = c("All (*)" = "*"),
                                                                               selected = c("*"),
                                                                               options = list(
                                                                                 render = I(includeText("scripts/geneAnnotationKeywordFilterInputSelectizeRender.js")),
                                                                                 plugins = list("remove_button", "drag_drop"),
                                                                                 maxOptions = 1000000
                                                                               ))),
                              tags$div(class = "filter-operation",selectizeInput(ns("operation"), 
                                                                                 label = "Filter settings", 
                                                                                 choices = c("AND", "OR"),
                                                                                 selected = "OR")),
                              checkboxInput(ns("invert.selection"), "Invert selection")
                     )),
    conditionalPanel(conditionalPanel.equals(ns("toggle.gobrowser"), "true"),
                     goTermFilterUI(ns("gobrowser")))
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
geneAnnotationKeywordFilterValues_ <- function(input, output, session, values) {
  
  all.choices <- reactive({
    choices <- list()
    
    # We generate the values as CATEGORY.NAME (which matches the output of unlist(,recursive = F)) )
    for(category in names(values())) {
      keys <- names(values()[[category]])
      category.values <- sapply(keys, function(x) { paste0(category, ".", x) })
      
      if(category == "Associated GO terms") {
        go.terms <- AnnotationDbi::select(GO.db, keys[keys != "No data"], columns = "TERM")
        names(category.values) <- sapply(seq_along(keys), function(i) { paste0( if(keys[i] != "No data") go.terms[i, "TERM"] else "No data", " (", length(values()[[category]][[keys[i]]]), ")") }) 
      }
      else {
        names(category.values) <- sapply(names(values()[[category]]), function(x) { paste0(x, " (", length(values()[[category]][[x]]), ")") }) 
      }
      
      choices[[category]] <- category.values
    }
    
    choices[["Misc"]] <- append(choices[["Misc"]], list("All (*)" = "*"))
    return(choices)
  })
  
  selected.goterms <- goTermFilterValue("gobrowser", goterms = reactive({
    validate(need(all.choices(), "No GO terms loaded"))
    
    goterms <- all.choices()[["Associated GO terms"]]
    goterms <- sapply(goterms, function(x) unlist(strsplit(x, ".", fixed = T))[2])
    names(goterms) <- names(all.choices()[["Associated GO terms"]])
    
    return(goterms[goterms != "No data"])
  }))
  
  filtered.choices <- reactive({
    validate(need(all.choices(), "No filter keys available!"))
    
    choices <- all.choices()
    gotermchoices <- choices[["Associated GO terms"]]
    
    selected.terms <- selected.goterms()
    selected.terms <- sapply(selected.terms, function(x) paste0("Associated GO terms.", x))
    
    choices[["Associated GO terms"]] <- c(gotermchoices[match(selected.terms, gotermchoices)], gotermchoices[gotermchoices == "Associated GO terms.No data"])
    
    return(choices)
  })
  
  output$gobrowser.warning <- renderUI({
    validate(need(all.choices(), "No filter keys loaded"))
    validate(need(filtered.choices(), "No filter keys loaded"))
    
    goterms <- all.choices()[["Associated GO terms"]]
    goterms.filtered <- filtered.choices()[["Associated GO terms"]]
    
    if(length(goterms) > 1 && length(goterms.filtered) <= 1) {
      return(tagList(iconText(icon("exclamation-triangle"), "To filter for GO terms, you have to use the GO term browser to select the terms you want to filter for."),
                     vSkip(5)))
    }
    else {
      return(tagList())
    }
  })
  
  observeEvent(filtered.choices(), {
    
    current.choices <- input$values
    rescue <- intersect(current.choices, unlist(filtered.choices()))
    
    if(length(rescue) == 0) {
      rescue <- c("*")
    }
    
    updateSelectizeInput(session, "values", choices = filtered.choices(), selected = rescue)
  })
  
  available.values <- reactive({
    return(unlist(values(), recursive = F))
  })
  
  selected.keys <- reactive({
    keys <- input$values
    
    # Do this to prevent breaking the axis selectize inputs
    if(length(keys) == 0) {
      keys <- names(available.values())
    }
    
    # Handle "Select All" case
    if("*" %in% keys) {
      keys <- names(available.values())
    }
    
    return(keys)
    
  })
  
  selected.values <- reactive({
    
    #' Depending on the user's selection apply union (OR) or intersect (AND) to 
    #' the list of strings in the values vector
    #' If the user wants to invert the selection, just calculate ALL_STRINGS SET_DIFFERENCE USERSELECTED_STRINGS
    
    selected.strings <- c()
    
    if(input$operation == "AND") {
      selected.strings <- Reduce(intersect, available.values()[selected.keys()])
    }
    else if(input$operation == "OR") {
      selected.strings <- Reduce(union, available.values()[selected.keys()])
    }
    
    if(input$invert.selection) {
      all.strings <- Reduce(union, available.values())
      selected.strings <- setdiff(all.strings, selected.strings)
    }
    
    return(selected.strings)
    
  })
  
  return(reactive( { list(values = selected.values(), 
                          keys = selected.keys(), 
                          operation = input$operation, 
                          input = input$values,
                          invert = input$invert.selection ) } ))
  
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
geneAnnotationKeywordFilterValues <- function(id, values) {
  return(callModule(geneAnnotationKeywordFilterValues_,
                    id,
                    values = values))
}