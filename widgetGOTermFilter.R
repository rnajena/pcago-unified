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

goTermFilter.buildGOInfoUI <- function(goids) {
  
  output <- tagList()
  
  for(goid in goids) {
    
    info <- GO.db::GOTERM[[goid]]
    
    if(is.null(info)) {
      output <- tagAppendChild(output, tags$div(
        class = "go-term-details",
        tags$h2("Unknown GO ID"),
        vSkip(5),
        tags$a(href=paste0("http://amigo.geneontology.org/amigo/medial_search?q=", info@GOID), target = "_blank", "Search on AmiGO 2"),
        
        hDivider()
      ))
    }
    else {
      ontology <- "Unknown"
      
      if(info@Ontology == "BP") {
        ontology <- "Biological Process (BP)"
      }
      else if(info@Ontology == "CC") {
        ontology <- "Cellular Component (CC)"
      }
      else if(info@Ontology == "MF") {
        ontology <- "Molecular Function (MF)"
      }
      
      alternative.ids <- if(length(info@Secondary) > 0) paste(info@Secondary, collapse = ", ") else "None"
      synonyms <- if(length(info@Synonym) > 0) paste(info@Synonym, collapse = ", ") else "None"
      
      output <- tagAppendChild(output, tags$div(
        class = "go-term-details",
        tags$h2(info@GOID),
        tags$h3(info@Term),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Ontology"), tags$span(class = "value", ontology)),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Alternate IDs"), tags$span(class = "value", alternative.ids)),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Synonyms"), tags$span(class = "value", synonyms)),
        vSkip(10),
        tags$div(class = "definition", info@Definition),
        vSkip(5),
        tags$a(href=paste0("http://amigo.geneontology.org/amigo/term/", info@GOID), target = "_blank", "View on AmiGO 2"),
        
        hDivider()
      ))
    }
    
  }
  
  return(output)
  
}

goTermFilterValue_ <- function(input, output, session, goterms) {
  
  vars <- reactiveValues(selected.terms = c())
  
  # Define button actions
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
  
  
  observeEvent(input$drilldown.info, {
    terms <- input$drilldown.term
    #terms <- c("GO:0009987") # for debugging
    
    if(length(terms) == 0) {
      showModal(modalDialog("Please select some GO terms from the list.", size = "l", footer = tagList(
        modalButton("OK"))
      ))
    }
    else {
      ui <- goTermFilter.buildGOInfoUI(terms)
      showModal(modalDialog(ui, size = "l", title = tagList(
        modalButton(label = "", icon = icon("times-circle")),
        dropdownButton(session$ns("goterm.details.export"), "Export report", tagList(
          downloadButton(session$ns("goterm.details.export.html"), label = "as *.html")
        ), icon = icon("download"))
      ),
      footer = tagList(modalButton("Close"))))
      
      output$goterm.details.export.html <- downloadHandler("goterm-report.html", function(filename) {
        conn <- file(filename)
        on.exit({
          close(conn)
        })
        
        writeLines(paste(ui), conn)
      })
    }
    
  })
  
  # Update drilldown + selections etc
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