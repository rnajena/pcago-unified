#'
#' Filter widget for GO terms that supports drilldown and search
#' 

library(shiny)
library(shinyjs)
library(shinyBS)
library(GO.db)
source("classGeneAnnotation.R")
source("classGeneFilter.R")
source("uiHelper.R")

########### TODO: Export/import list #########!

goTermFilterUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    tags$div(class = "go-term-filter",
             tags$label("Browse GO terms"),
             tags$div(class = "drilldown",
                      textInput(ns("search"), "", placeholder = "Search GO term ..."),
                      selectInput(ns("drilldown.term"), "", choices = c(), multiple = T, selectize = F, size = 10),
                      fluidPage(fluidRow( 
                        actionButton(ns("drilldown.visit"), "List subterms", icon = icon("code-fork")),
                        actionButton(ns("drilldown.add"), "Add to filter", icon = icon("plus")),
                        actionButton(ns("drilldown.info"), "Show details", icon = icon("search")),
                        dropdownButton(ns("drilldown.more"), "More ...",
                                       tagList(actionButton(ns("drilldown.listall"), "List all GO terms", icon = icon("list")),
                                       actionButton(ns("drilldown.listselected"), "List selected GO terms", icon = icon("list")),
                                       actionButton(ns("drilldown.remove"), "Remove from filter", icon = icon("trash"))
                                       ))
                        ))
                      ),
             vSkip(5),
             textOutput(ns("selected.count")))
  ))
}

goTermFilterValue_ <- function(input, output, session, goterms) {
  
  vars <- reactiveValues(selected.terms = c())
  
  observeEvent(input$drilldown.add, {
    if(!is.null(input$drilldown.term)) {
      vars$selected.terms <- unique(c(vars$selected.terms, input$drilldown.term))
    }
  })
  
  observeEvent(input$drilldown.remove, {
    if(!is.null(input$drilldown.term)) {
      vars$selected.terms <- setdiff(vars$selected.terms, input$drilldown.term)
    }
  })
  
  observeEvent(input$drilldown.listall, {
    updateTextInput(session, "search", value = "*")
  })
  
  observeEvent(input$drilldown.listselected, {
    updateTextInput(session, "search", value = "+")
  })
  
  observeEvent(goterms(), {
    updateSelectInput(session, "drilldown.term", choices = c())
    vars$selected.terms <- c()
  })
  
  observe({
    
    validate(need(goterms(), "No GO terms defined"))
    
    if(is.null(input$search) || input$search == "") {
      # TODO: Browser mode
    }
    else if(input$search == "*") {
      updateSelectInput(session, "drilldown.term", choices = goterms())
    }
    else if(input$search == "+") {
      updateSelectInput(session, "drilldown.term", choices = goterms()[match(vars$selected.terms, goterms())])
    }
    else {
      updateSelectInput(session, "drilldown.term", choices = goterms()[stringi::stri_detect_fixed(names(goterms()), input$search)])
    }
  })
  
  output$selected.count <- renderText({
    validate(need(goterms(), "No terms available"))
    return(paste(length(vars$selected.terms), "terms selected"))
  })
  
  return(reactive({ vars$selected.terms }))
  
}

goTermFilterValue <- function(id, goterms) {
  return(callModule(goTermFilterValue_, 
                    id,
                    goterms = goterms))
}