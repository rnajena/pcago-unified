#' 
#' Plot module for variance plot
#' 

library(shiny)
library(ggplot2)
library(scatterplot3d)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")

plotPCAVariancePlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot")))
}

plotPCAVariancePlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel("General settings", generalPlotSettingsInput(ns("plot.settings"),
                                                                 legend.color = F,
                                                                 legend.shape = F))
  ))
  
}

plotPCAVariancePlot.save <- function(pca, plot.settings, format, filename){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = 640, 
                                                  height = 480,
                                                  dpi = 96,
                                                  scale = 1,
                                                  title = "Principal component variances",
                                                  subtitle = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  
  plot.y.label <- sprintf("Relative variance (to %s)", paste(sum(pca$var)))
  
  
  p <- ggplot(pca$var, aes(x=factor(rownames(pca$var), levels = rownames(pca$var)), y=var.relative)) + geom_point()
  p <- p + labs(x = "Principal component", y = plot.y.label, title = title, subtitle = subtitle)
  ggsave(filename, p, width = width / 72, height = height / 72, dpi = dpi, scale = 0.75 / scale, device = format)
  
  return(plot.settings)
  
}

plotPCAVariancePlot_ <- function(input, 
                          output, 
                          session, 
                          pca) {
  
  plot.settings <- generalPlotSettings("plot.settings")
  downloadablePlot("plot", 
                   plot.settings = plot.settings, 
                   exprplot = function(plot.settings, format, filename) 
                   { 
                     return(plotPCAVariancePlot.save(pca(), plot.settings, format, filename))
                   })
  
}

plotPCAVariancePlot <- function(id, pca) {
  
  return(callModule(plotPCAVariancePlot_, 
                    id, 
                    pca = pca))
  
}