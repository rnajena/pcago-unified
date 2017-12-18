#' 
#' Plot module for variance plot
#' 

library(shiny)
library(ggplot2)
library(VennDiagram)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")
source("defaultParameters.R")

plotConditionsVennDiagramPlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot")))
}

plotConditionsVennDiagramPlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel(recommendedDataText("Venn diagram"),
                    value = "venndiagram",
                    selectizeInput(ns("conditions"), "Displayed sets", 
                                                  choices = c(),
                                                  multiple = T,
                                                  options = list(maxItems = 5))),
    bsCollapsePanel(optionalDataText("Visualization"), 
                    value = "visualization",
                    visualsEditorUI(ns("visuals"))),
    bsCollapsePanel(optionalDataText("General settings"), 
                    value = "generalsettings",
                    generalPlotSettingsInput(ns("plot.settings"),
                                                                 legend.shape = F,
                                                                 legend.color = F))
  ))
  
}

plotConditionsVennDiagramPlot.save <- function(selected.conditions, conditions, pca.conditions.plot.visuals, plot.settings, format, filename){
  
  validate(need(conditions(), "No sample conditions to plot!"),
           need(pca.conditions.plot.visuals(), "No sample condition visuals available!"))
  
  validate(need(selected.conditions, "No sample conditions selected!"),
           need(length(selected.conditions) > 1, "Not enough sample conditions selected!"),
           need(length(selected.conditions) <= 5, "Too many sample conditions selected!"))
  
  x <- lapply(selected.conditions, function(x) { 
    
    indices <- conditions()[[x]]
    return(rownames(conditions())[indices]) 
    
  })
  names(x) <- selected.conditions
  
  plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(
    width = default.plot.width,
    height = default.plot.height,
    dpi = default.plot.dpi,
    scale = 1,
    title = "Conditions",
    subtitle = ""
  ))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  visuals <- na.omit(pca.conditions.plot.visuals()[selected.conditions,])
  
  validate(need(nrow(visuals) > 0, "No visual data!"))
  
  fill <- sapply(rownames(visuals), function(x) { if(visuals[x, "color"] == "") "black" else visuals[x, "color"] })
  name <- sapply(rownames(visuals), function(x) { if(visuals[x, "name"] == "") x else visuals[x, "name"] })
  
  venn.diagram(x, 
               filename, 
               width = width,
               height = height,
               resolution = dpi,
               pointsize = scale * 12,
               imagetype = format,
               main = title,
               sub = subtitle,
               fill = as.vector(fill),
               category.names = as.vector(name),
               main.fontfamily = "sans",
               main.fontface = 2,
               main.cex = 1.2,
               sub.fontfamily = "sans",
               sub.cex = 1.1,
               cat.fontfamily = "sans",
               fontfamily = "sans",
               ext.text = T,
               euler.d = T,
               scaled = T)
  
  return(plot.settings)
  
}

plotConditionsVennDiagramPlot_ <- function(input, 
                                  output, 
                                  session, 
                                  conditions,
                                  xauto = NULL) {
  
  plot.settings <- generalPlotSettings("plot.settings")
  visuals <- visualsEditorValue("visuals", reactive({colnames(conditions())}), has.shape = F)
  
  # Update selectize
  observeEvent(conditions(), {
    
    validate(need(conditions(), "No conditions available!"))
    
    available.conditions <- colnames(conditions())
    selected.conditions <- if(length(available.conditions) > 0) available.conditions[1:min(5,length(available.conditions))] else c()
    
    updateSelectizeInput(session, 
                         "conditions", 
                         choices = available.conditions, 
                         selected = selected.conditions)
    
  })
  
  # Render plot
  downloadablePlot("plot", 
                   plot.settings = plot.settings, 
                   exprplot = function(plot.settings, format, filename) 
                   { 
                     return(plotConditionsVennDiagramPlot.save(
                       selected.conditions = input$conditions,
                       conditions = conditions,
                       pca.conditions.plot.visuals = visuals,
                       plot.settings = plot.settings,
                       format = format,
                       filename = filename
                     ))
                   })
  
  # xauto exporter that allows triggering of exporting data from code
  xautovars <- reactiveValues(xautocounter = 1)
  
  if(!is.null(xauto)) {
    observeEvent(xauto(), {
      
      format <- xauto()$format
      filename <- xauto()$filename
      
      plotConditionsVennDiagramPlot.save(
        selected.conditions = input$conditions,
        conditions = conditions,
        pca.conditions.plot.visuals = visuals,
        plot.settings = plot.settings(),
        format = format,
        filename = filename
      )
      
      xautovars$xautocounter <- xautovars$xautocounter + 1
      
    })
  }
  
  return(reactive({ xautovars$xautocounter }))
  
}

#' Venn diagram plot
#'
#' @param id 
#' @param conditions 
#' @param xauto If not null returns a function that returns list(filename = <>, format = <svg,png,tiff>)
#'
#' @return
#' @export
#'
#' @examples
plotConditionsVennDiagramPlot <- function(id, conditions, xauto = NULL) {
  
  return(callModule(plotConditionsVennDiagramPlot_, 
                    id, 
                    conditions = conditions,
                    xauto = xauto))
  
}