#'
#' Contains a data table widget with download buttons
#'

library(DT)
library(shiny)
library(openxlsx)
source("uiHelper.R")

#' Creates a UI with a data table output and download buttons
#'
#' @param id ID of the control
#' @param ... Additional items to include into header bar
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTableOutput <- function(id, 
                                        custom.header.items = NULL,
                                        custom.export.items = NULL,
                                        settings.panel = NULL,
                                        settings.panel.label = "Settings") {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  export.items <- tagList(
    downloadButton(ns("export.csv"), "as *.csv"),
    downloadButton(ns("export.tsv"), "as *.tsv"),
    downloadButton(ns("export.xlsx"), "as *.xlsx"))
  
  if(!is.null(custom.export.items)) {
    export.items <- tagAppendChildren(export.items, list = custom.export.items)
  }
  
  settings.button <- if(!is.null(settings.panel)) { bsButton(ns("settings"), settings.panel.label, icon = icon("cog"), type = "toggle") } else NULL
  
  return(headerPanel(header = tags$span(class="headerbar-row",
                                        settings.button,
                                        dropdownButton(ns("menu.export"), "Export data", 
                                                       icon = icon("download"),
                                                       export.items),
                                        custom.header.items,
                                        conditionalPanel(conditionalPanel.equals(ns("settings"), "true"),
                                                         tags$div(class = "settings-panel",
                                                                  hDivider(),
                                                                  settings.panel)),
                                        bsButton(ns("collapse"), "Collapse", icon = icon("eye-slash"), type = "toggle")),
                     conditionalPanel(conditionalPanel.equals(ns("collapse"), "false"), DT::dataTableOutput(ns("table")))))
}

#' Fills the data table with given data output.
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param data Data to be displayed. Should be a reactive. The reactive can return a data.frame, matrix or SummarizedExperiment
#' @param export.filename Filename of the downloaded file
#' @param export.rownames Export row names, too. Default = TRUE
#' @param export.colnames Parameter of write.table.
#' @param xauto If not NULL, this is a function that returns a list list(filename = <>, format = <>). Used for automated export
#'
#' @return
#' @export
#'
#' @examples
downloadableDataTable_ <- function(input, output, session, data, export.filename, export.rownames = T, export.colnames = NA, xauto = NULL) {
  
  table.data <- reactive({
    
    validate(need(data(), "No data to display!"),
             need(is.data.frame(data()) || is.matrix(data()) || is.SummarizedExperiment(data()), "Data is not a data frame!"))
    
    if(is.SummarizedExperiment(data())) {
      return(assay(data()))
    }
    else {
      return(data())
    }
  })
  
  
  
  output$table <- DT::renderDataTable(table.data(), options = list(scrollX = TRUE))
  output$export.csv <- downloadHandler(paste0(export.filename, ".csv"), 
                                       function(file) {
                                         
                                        write.table(table.data(),
                                                  file,
                                                  sep = ",",
                                                  row.names = export.rownames,
                                                  col.names = export.colnames)
                                         
                                       })
  output$export.tsv <- downloadHandler(paste0(export.filename, ".csv"), 
                                       function(file) {
                                         
                                         write.table(table.data(),
                                                     file,
                                                     sep = "\t",
                                                     row.names = export.rownames,
                                                     col.names = export.colnames)
                                         
                                       })
  output$export.xlsx <- downloadHandler(paste0(export.filename, ".xlsx"), 
                                       function(file) {
                                         
                                         write.xlsx(table.data(),
                                                     file,
                                                     row.names = export.rownames,
                                                     col.names = is.na(export.colnames))
                                         
                                       })
  
  # xauto exporter that allows triggering of exporting data from code
  xautovars <- reactiveValues(xautocounter = 1)
  
  if(!is.null(xauto)) {
    
    observeEvent(xauto(), {
      
      file <- xauto()$filename
      
      if(xauto()$format == "csv") {
        write.table(table.data(),
                    file,
                    sep = ",",
                    row.names = export.rownames,
                    col.names = export.colnames)
        xautovars$xautocounter <- xautovars$xautocounter + 1
      }
      else {
        stop("Not supported")
      }
      
    })
  }
  
  return(reactive({ xautovars$xautocounter }))
  
}

#' Fills the data table with given data output.
#' 
#' @param data Data to be displayed. Should be a reactive. The reactive can return a data.frame, matrix or SummarizedExperiment.
#' @param export.filename Filename of the downloaded file
#' @param export.rownames Export row names, too. Default = TRUE
#' @param export.colnames Parameter of write.table.
#' @param xauto If not NULL, this is a function that returns a list list(filename = <>, format = <>). Used for automated export
#'
#' @return Variable that indicates that xauto is finished
#' @export
#'
#' @examples
downloadableDataTable <- function(id, data, export.filename = "table", export.rownames = T, export.colnames = NA, xauto = NULL) {
  
  return(callModule(downloadableDataTable_,
                    id, 
                    data = data, 
                    export.filename = export.filename,
                    export.rownames = export.rownames,
                    export.colnames = export.colnames,
                    xauto = xauto))
  
}