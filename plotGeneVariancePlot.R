#' 
#' Plot module for variance plot
#' 

library(shiny)
library(ggplot2)
library(scatterplot3d)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")

plotGeneVariancePlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot"),
                                custom.header.items = tagList(
                                  bsButton(ns("logarithmic"), 
                                           "Logarithmic",
                                           icon = icon("superscript"),
                                           type = "toggle",
                                           value = T)
                                )))
}

plotGeneVariancePlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel("General settings", generalPlotSettingsInput(ns("plot.settings")))
  ))
  
}

plotGeneVariancePlot.save <- function(gene.variances, plot.settings, format, filename, logarithmic = F){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = 640, 
                                                  height = 480,
                                                  dpi = 96,
                                                  scale = 1,
                                                  title = "Gene variances",
                                                  subtitle = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  
  # Soft and hard parameter checking
  validate(need(gene.variances, "No gene variances available!"))
  
  if(!is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) || !is.character(format) || !is.character(filename)) {
    stop("Invalid arguments!")
  }
  
  plot.aes <- if(logarithmic) aes(x=1:nrow(gene.variances), y=log(var)) else aes(x=1:nrow(gene.variances), y=var)
  plot.y.label <- if(logarithmic) expression(log(sigma^2)) else expression(sigma^2)
  
  p <- ggplot(gene.variances, plot.aes) + geom_point()
  p <- p + labs(x = "Top n-th variant gene", y = plot.y.label, title = title, subtitle = subtitle)
  ggsave(filename, p, width = width / 72, height = height / 72, dpi = dpi, scale = scale, device = format)
  
  return(plot.settings)
  
}

plotGeneVariancePlot_ <- function(input, 
                                  output, 
                                  session, 
                                  gene.variances) {
  
  plot.settings <- generalPlotSettings("plot.settings")
  downloadablePlot("plot", 
                   plot.settings = plot.settings, 
                   exprplot = function(plot.settings, format, filename) 
                   { 
                     return(plotGeneVariancePlot.save(gene.variances(), plot.settings, format, filename, 
                                                      logarithmic = input$logarithmic))
                   })
  
}

plotGeneVariancePlot <- function(id, gene.variances) {
  
  return(callModule(plotGeneVariancePlot_, 
                    id, 
                    gene.variances = gene.variances))
  
}