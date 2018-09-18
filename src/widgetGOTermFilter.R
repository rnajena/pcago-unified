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
source("environment.R")
source("goTerms.R")
source("widgetGenericImporter.R")

goTermFilterUI <- function(id) {
  
  ns <- NS(id)
  
  return(tagList(
    headerPanel(header = tags$span(
      downloadButton(ns("export.csv"), "Export *.csv"),
      bsButton(ns("import"), "Import", icon = icon("upload"), type = "toggle")
    ),
    conditionalPanel(conditionalPanel.equals(ns("import"), "false"), 
      tags$div(class = "go-term-filter",
               textInput(ns("search"), "", placeholder = "Search GO term ..."),
               tags$div(class = "drilldown",
                        conditionalPanel(conditionalPanel.equals(ns("search"), "''"),
                          tags$div(class = "drilldown-selection",
                                   radioButtons(ns("drilldown.selection"), label = "", choices = c("Root" = "root")))
                          
                        ),
                        selectInput(ns("drilldown.term"), "", choices = c(), multiple = T, selectize = F, size = 10)),
               vSkip(5),
               fluidPage(fluidRow( 
                 actionButton(ns("drilldown.visit"), "List subterms", icon = icon("code-fork")),
                 actionButton(ns("drilldown.add"), "Add to filter", icon = icon("plus")),
                 actionButton(ns("drilldown.info"), "Show details", icon = icon("search")),
                 dropdownButton(ns("drilldown.more"), "More ...",
                                tagList(actionButton(ns("drilldown.listall"), "List all GO terms", icon = icon("list")),
                                        actionButton(ns("drilldown.listselected"), "List selected GO terms", icon = icon("list")),
                                        actionButton(ns("drilldown.remove"), "Remove from filter", icon = icon("trash"))
                                ))
               )),
               vSkip(5),
               textOutput(ns("selected.count")))),
    conditionalPanel(conditionalPanel.equals(ns("import"), "true"),
                     genericImporterInput(ns("importer")))
  )))
}

goTermFilterValue_ <- function(input, output, session, goterms) {
  
  vars <- reactiveValues(selected.terms = c(), root.terms = c(), drilldown = c("Root" = "root"))
  
  drilldown.term <- reactive({
    if(is.null(input$drilldown.term)) {
      return(c())
    }
    else {
      return(input$drilldown.term[input$drilldown.term != "none"])
    }
  })
  
  # Define button actions
  observeEvent(input$drilldown.add, {
    if(!is.null(drilldown.term())) {
      vars$selected.terms <- unique(c(vars$selected.terms, drilldown.term()))
    }
  })
  
  observeEvent(input$drilldown.remove, {
    if(!is.null(drilldown.term())) {
      vars$selected.terms <- setdiff(vars$selected.terms, drilldown.term())
    }
  })
  
  observeEvent(input$drilldown.listall, {
    updateTextInput(session, "search", value = "*")
  })
  
  observeEvent(input$drilldown.listselected, {
    updateTextInput(session, "search", value = "+")
  })
  
  observeEvent(input$drilldown.visit, {
    if(length(drilldown.term()) == 1) {
      
      # The drilldown discards all options after the current selected one
      index <- 1 
      
      if(is.null(input$search) || input$search == "") {
        index <- match(input$drilldown.selection, vars$drilldown)
        
        if(is.na(index)) {
          index <- 1
        }
      }
      
      newdrill <- c(vars$drilldown[1:index], goterms()[goterms() == drilldown.term()])
      vars$drilldown <- newdrill
      updateRadioButtons(session, "drilldown.selection", choices = newdrill, selected = newdrill[length(newdrill)])
      updateTextInput(session, "search", value = "")
      
    }
    else {
      showNotification("Please select one GO term.", type = "warning")
    }
  })
  
  observeEvent(input$drilldown.info, {
    
    terms <- drilldown.term()
    #terms <- c("GO:0009987") # for debugging
    
    if(length(terms) == 0) {
      showModal(modalDialog("Please select some GO terms from the list.", size = "l", footer = tagList(
        modalButton("OK"))
      ))
    }
    else if(length(terms) > go.browser.detailed.maxterms) {
      showModal(modalDialog(paste("You can only view up to", go.browser.detailed.maxterms, "GO terms."), size = "l", footer = tagList(
        modalButton("OK"))
      ))
    }
    else {
      ui <- goTermsBuildInfoUI(terms)
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
    vars$drilldown <- c("Root" = "root")
    updateRadioButtons(session, "drilldown.selection", choices = vars$drilldown)
    vars$root.terms <- goTermsFindRootGOIds(goterms())
  })
  
  observe({
    
    validate(need(goterms(), "No GO terms defined"))
    
    if(is.null(input$search) || input$search == "") {
      if(is.null(input$drilldown.selection) || input$drilldown.selection == "root") {
        updateSelectInput(session, "drilldown.term", choices = vars$root.terms)
      }
      else {
        updateSelectInput(session, "drilldown.term", choices = goTermsFindChildGOIds(goterms(), input$drilldown.selection))
      }
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
  
  # Import / export
  output$export.csv <- downloadHandler("selected_goterms.csv",
                                       contentType = "text/csv",
                                       content = function(filename) {
                                         exportSelectedGOTerms.CSV(filename, vars$selected.terms)
                                       })
  
  selected.ids.imported <- genericImporterData("importer", 
                                               importers = reactive(supportedSelectedGOTermsImporters),
                                               samples = reactive(availableSelectedGOTermsSamples),
                                               generators = reactive(supportedSelectedGOTermsGenerators),
                                               exprimport = function(con, importer, parameters) {
                                                 return(importSelectedGOTerms(filehandle = con, 
                                                                               importer = importer, 
                                                                               parameters = parameters, 
                                                                               available.terms = goterms()))
                                               },
                                               exprsample = function(sample, parameters) {
                                                 return(importSampleAnnotationSample(sample = sample, 
                                                                                     parameters = parameters,
                                                                                     available.terms = goterms()))
                                               })
  
  #' When the user imports a visual table, apply it
  observeEvent(selected.ids.imported(), {
    
    if(!is.null(selected.ids.imported())) {
      vars$selected.terms <- selected.ids.imported()
    }
    
  })
  
  
  return(reactive({ vars$selected.terms }))
  
}

goTermFilterValue <- function(id, goterms) {
  return(callModule(goTermFilterValue_, 
                    id,
                    goterms = goterms))
}