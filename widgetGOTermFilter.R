#'
#' Filter widget for GO terms that supports drilldown and search
#' 

library(shiny)
library(shinyjs)
library(GO.db)
source("classGeneAnnotation.R")
source("classGeneFilter.R")

########### TODO: Export/import list #########!

goTermFilterUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    tags$div(class = "go-term-filter",
             tags$label("Browse GO terms"),
             tags$div(class = "drilldown",
                      textInput(ns("search"), "", placeholder = "Search GO term ..."),
                      selectInput(ns("drilldown.term"), "", choices = c("a", "b", "c", "d"), multiple = T, selectize = F, size = 10),
                      fluidPage(fluidRow( 
                        actionButton(ns("drilldown.visit"), "List subterms", icon = icon("code-fork")),
                        actionButton(ns("drilldown.add"), "Add to filter", icon = icon("plus")),
                        actionButton(ns("drilldown.info"), "Show details", icon = icon("search"))))
                      ))
  ))
}

goTermFilterValue_ <- function(input, output, session) {
  
  vars <- reactiveValues(selected.terms = c())
  
  observeEvent(input$drilldown.add, {
    if(!is.null(input$drilldown.term)) {
      vars$selected.terms <- unique(c(vars$selected.terms, input$drilldown.term))
    }
  })
  
  return(reactive({ vars$selected.terms }))
  
}

goTermFilterValue <- function(id) {
  return(callModule(goTermFilterValue_, 
                    id))
}