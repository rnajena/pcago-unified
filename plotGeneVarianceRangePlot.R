#' 
#' Plot module for variance plot
#' 

library(shiny)
library(shinyBS)
library(ggplot2)
source("uiHelper.R")
source("widgetVisualsEditor.R")

plotGeneVarianceRangePlotUI <- function(id, ...) {
  
  ns <- NS(id)
  return(verticalLayout(bsButton(ns("logarithmic"), 
                                 "Logarithmic",
                                 icon = icon("superscript"),
                                 type = "toggle",
                                 value = T),
                        hDivider(),
                        plotOutput(ns("plot"), ...)))
}


plotGeneVarianceRangePlot_ <- function(input, 
                                 output, 
                                 session, 
                                 dataset) {
  
  output$plot <- renderPlot({
    
    validate(need(dataset(), "No gene variances to display!"))
    validate(need(dataset()$variances.filtered, "No gene variances to display!"))
    
    logarithmic <- input$logarithmic
    genes.count <- dataset()$readcounts.top.variant.parameters.count
    data <- dataset()$variances.filtered
    data$logvar <- log(data$var)
    data.selection <- data[1:genes.count,]
    
    if(logarithmic) {
      p <- ggplot(data, aes(x=1:nrow(data), y=logvar))
      p <- p + geom_vline(xintercept = genes.count, color = "red")
      p <- p + geom_ribbon(data = data.selection, aes(ymin = min(data$logvar), ymax = logvar, x = 1:genes.count), fill = "#da4453")
      p <- p + geom_point()
      p <- p + labs(x = "Top n-th variant gene", y = expression(log(sigma^2)))
      
      return(p)
    }
    else {
      p <- ggplot(data, aes(x=1:nrow(data), y=var))
      p <- p + geom_vline(xintercept = genes.count, color = "red")
      p <- p + geom_ribbon(data = data.selection, aes(ymin = min(data$var), ymax = var, x = 1:genes.count), fill = "#da4453")
      p <- p + geom_point()
      p <- p + labs(x = "Top n-th variant gene", y = expression(sigma^2))
      
      return(p)
    }
  })
  
}

plotGeneVarianceRangePlot <- function(id, dataset) {
  
  return(callModule(plotGeneVarianceRangePlot_, 
                    id, 
                    dataset = dataset))
  
}