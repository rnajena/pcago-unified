#'
#' Contains a plot output with download buttons
#'

library(DT)
library(shiny)
source("uiHelper.R")
source("classPlotSettings.R")

#' Creates a UI with a plot output and download buttons
#'
#' @param id ID of the control
#' @param custom.header.items Additional items to include into header bar
#' @param custom.export.items Additional items in the export menu
#' @param settings.panel An additional panel with settings that can be expanded
#'
#' @return
#' @export
#'
#' @examples
downloadablePlotOutput <- function(id, 
                                   custom.header.items = NULL,
                                   custom.export.items = NULL,
                                   settings.panel = NULL,
                                   settings.panel.label = "Settings") {
  
  if(!is.character(id)) {
    stop("Invalid arguments!")
  }
  
  ns <- NS(id)
  
  export.items <- tagList(
    downloadButton(ns("export.svg"), "as *.svg"),
    downloadButton(ns("export.png"), "as *.png"),
    downloadButton(ns("export.pdf"), "as *.pdf"),
    downloadButton(ns("export.tiff"), "as *.tiff")
  )
  
  if(!is.null(custom.export.items)) {
    export.items <- tagAppendChildren(export.items, list = custom.export.items)
  }
  
  settings.button <- if(!is.null(settings.panel)) { bsButton(ns("settings"), settings.panel.label, icon = icon("cog"), type = "toggle") } else NULL
  
  return(tags$div(class = "downloadable-plot",
                   headerPanel(header = tags$span(class="headerbar-row",
                                                  settings.button,
                                                  dropdownButton(ns("menu.export"), "Export image", 
                                                                 icon = icon("download"),
                                                                 export.items),
                                                  custom.header.items,
                                                  conditionalPanel(conditionalPanel.equals(ns("settings"), "true"),
                                                                   tags$div(class = "settings-panel",
                                                                            hDivider(),
                                                                            settings.panel))
                                                  ),
                               uiOutput(ns("plot.container"))
                               # plotOutput(ns("plot"))
                               )))
}


#' Plots a downloadable plot
#' This function is supposed to be called by callModule. Use the one without an underscore for easier access.
#'
#' @param input 
#' @param output 
#' @param session 
#' @param exprplot Function that takes width, height, format and filename and saves a plot to filename. Optionally returns plot settings object
#' @param plot.settings Reactive that returns a plot settings object
#' @param render.format Format of data sent to user (default: png)
#' @param render.alt Alt text for img object (default: "Plot")
#' @param export.filename Base filename of exported plot
#'
#' @return
#' @export
#'
#' @examples
downloadablePlot_ <- function(input, 
                              output, 
                              session, 
                              exprplot, 
                              plot.settings = reactive({ PlotSettings() }), 
                              render.format = "png", 
                              render.alt = "Plot", 
                              export.filename = "plot") {
  
  out.width <- reactive({ session$clientData[[paste0("output_", session$ns("plot"), "_width")]] })
  out.height <- reactive({ session$clientData[[paste0("output_", session$ns("plot"), "_height")]] })
  
  # If the user didn't set any output sizes, overwrite them here
  plot.settings.defaults <- reactive({
    return(plotSettingsSetNA(plot.settings(), PlotSettings(width = out.width(), height = out.height(), dpi = default.plot.dpi, scale = 1) ))
    })
  
  output$plot.container <- renderUI({
    
    width <- "100%"
    height <- "700px"
    
    if(!is.na(plot.settings()@width)) {
      width <- paste0(plot.settings()@width, "px")
    }
    if(!is.na(plot.settings()@height)) {
      height <- paste0(plot.settings()@height, "px")
    }
    
    return(plotOutput(session$ns("plot"), width = width, height = height))
  })
  
  output$plot <- renderImage({
    
    validate(need(out.width(), "No output width!"),
             need(out.height(), "No output height!"))
    
    out.file <- tempfile(fileext=paste0(".", render.format))
    
    plot.settings.output <- exprplot(plot.settings = plot.settings.defaults(), filename = out.file, format = render.format)
    width <- out.width()
    height <- out.height()
    
    if(!is.null(plot.settings.output)) {
      
      if(!is(plot.settings.output, "PlotSettings")) {
        stop("Return value of exprplot must be NULL or a PlotSettings object!")
      }
      
      if(!is.na(plot.settings.output@width)) {
        width <- plot.settings.output@width
      }
      if(!is.na(plot.settings.output@height)) {
        height <- plot.settings.output@height
      }
    }
    
    return(list(src = out.file,
         width = width,
         height = height,
         alt = render.alt))
  })
  
  output$export.svg <- downloadHandler(filename = paste0(export.filename, ".svg"),
                                       content = function(file) {
                                         exprplot(plot.settings = plot.settings.defaults(), filename = file, format = "svg")
                                       },
                                       contentType = "image/svg")

  output$export.png <- downloadHandler(filename = paste0(export.filename, ".png"),
                                       content = function(file) {
                                         exprplot(plot.settings = plot.settings.defaults(), filename = file, format = "png")
                                       },
                                       contentType = "image/png")
  output$export.pdf <- downloadHandler(filename = paste0(export.filename, ".pdf"),
                                       content = function(file) {
                                         exprplot(plot.settings = plot.settings.defaults(), filename = file, format = "pdf")
                                       },
                                       contentType = "application/pdf")
  output$export.tiff <- downloadHandler(filename = paste0(export.filename, ".tiff"),
                                       content = function(file) {
                                         exprplot(plot.settings = plot.settings.defaults(), filename = file, format = "tiff")
                                       },
                                       contentType = "image/tiff")
  
}

#' Plots a downloadable plot
#'
#' @param exprplot Function that takes width, height, format and filename and saves a plot to filename
#' @param plot.settings Reactive that returns a plot settings object. Optionally returns plot settings object.
#' @param render.format Format of data sent to user (default: png)
#' @param render.alt Alt text for img object (default: "Plot")
#' @param export.filename Base filename of exported plot
#'
#' @return
#' @export
#'
#' @examples
downloadablePlot <- function(id, 
                             exprplot, 
                             plot.settings = reactive({ PlotSettings() }), 
                             render.format = "png", 
                             render.alt = "Plot", 
                             export.filename = "plot") {
  
  return(callModule(downloadablePlot_,
                    id,
                    exprplot = exprplot,
                    plot.settings = plot.settings,
                    render.format = render.format,
                    render.alt = render.alt,
                    export.filename = export.filename))
  
}