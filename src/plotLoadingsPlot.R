#' 
#' Plot module for PCA sample plot
#' 

library(shiny)
library(ggplot2)
library(scatterplot3d)
library(plot3D)
library(scales)

source("widgetDownloadablePlot.R")
source("widgetInPlaceHelp.R")
source("widgetNumericRangeInput.R")
source("environment.R")
source("defaultParameters.R")
source("widgetGradientEditor.R")

plotLoadingsPlotSettingsUI.3dplotProvider <- c("Isometric" = "isometric", "Perspective" = "perspective")

plotLoadingsPlot.defaultGradient <- importGradientSample("Gradients/LoadingPlotDefault.csv", list())

plotLoadingsPlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot")))
}

plotLoadingsPlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel(recommendedDataText("Plot type"),
                    value = "plottype",
                    selectizeInput(ns("plot3dprovider"), "3D plot type", choices = plotLoadingsPlotSettingsUI.3dplotProvider),
                    conditionalPanel(conditionalPanel.equals(ns("plot3dprovider"), "'perspective'"),
                                     numericInput(ns("plot3d.theta"), "Rotation (Degree)", value = 40),
                                     numericInput(ns("plot3d.phi"), "Viewing angle (Degree)", value = 40),
                                     numericInput(ns("plot3d.nticks"), "Number of ticks", value = 5))
    ),
    bsCollapsePanel(recommendedDataText("Axes"),
                    value = "axes",
                    selectizeInput(ns("axes"),
                                   "Plot axes (x, y, z)",
                                   choices = c(),
                                   multiple = T,
                                   options = list(maxItems = 3, plugins = c("remove_button", "drag_drop")))),
    bsCollapsePanel(recommendedDataText("Principal components"),
                    value = "pc",
                    numericInput(ns("top"), helpIconText("Displayed principal components", includeMarkdown("helptooltips/pca-pca-plot-loadings-top.md")), value = 100)),
    bsCollapsePanel(optionalDataText("Visualization"),
                    value = "visualization",
                    gradientEditorUI(ns("color"))),
    bsCollapsePanel(optionalDataText("General settings"), 
                    value = "generalsettings",
                    generalPlotSettingsInput(ns("plot.settings")))
  ))
  
}

plotLoadingsPlot.save <- function(pca, 
                                pca.full,
                                axes, 
                                plot.settings,
                                format,
                                plot3dprovider,
                                plot3d.theta,
                                plot3d.phi,
                                plot3d.nticks,
                                top,
                                color,
                                filename ){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = default.plot.width, 
                                                  height = default.plot.height,
                                                  dpi = default.plot.dpi,
                                                  scale = 1,
                                                  title = "PCA loadings plot",
                                                  subtitle = "",
                                                  legend.color = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  customlabel.color <- plot.settings@legend.color
  
  label.color <- if(customlabel.color == "") "Vector length" else customlabel.color
  
  # Soft and hard parameter checking
  validate(
    need(pca, "No PCA results available!"),
    need(axes, "No axes to draw!"),
    need(color, "No colors defined!"),
    need(is.integer(plot3d.nticks), "Invalid number of ticks!"))
  validate(need(nrow(color) > 1, "Too few colors defined!"))
  validate(need(plot3d.nticks >= 0, "Invalid number of ticks!"))
  validate(need(is.integer(top), "Invalid number of displayed principal components!"))
  validate(need(top > 0, "Invalid number of displayed principal components!"))
  validate(need(top <= 100, "Too high number of displayed principal components! (Supported: max. 100)"))
  
  if(!is.character(format) || !is.character(filename)) {
    stop("Invalid arguments!")
  }
  
  # Fetch needed variables from PCA and visual parameters
  pca.transformed <- pca$transformed
  pca.var <- pca$var
  
  pc.lab <- function(pc) {
    return(paste0(pc, ": ", round(pca.var[pc, "var.relative"] * 100, 2), "% variance"))
  }
  
  # Determine how many dimensions should be drawn
  dimensions.available <- colnames(pca.transformed)
  dimensions.requested <- intersect(axes, dimensions.available)
  dimensions.plot <- min(length(dimensions.requested), length(dimensions.available)) 
  
  validate(need(dimensions.plot > 1, "Too few axes to draw!"),
           need(dimensions.plot <= 3, "Too many axes to draw!"))
  
  # xlim, ylim and zlim calculation based on full PCA
  xaxislimit <- c(-1, 1)
  yaxislimit <- c(-1, 1)
  zaxislimit <- c(-1, 1)
  
  # Fetch the loadings to be displayed
  loadings <- as.data.frame(pca$pc)[, dimensions.requested]
  
  # Restrict the loadings to n longest vectors within selected dimension space
  loadings$norm <- rowSums(loadings^2)
  loadings <- loadings[order(loadings$norm, decreasing = T)[1:min(top, nrow(loadings))], ]
  
  # Plot based on dimensions
  if(dimensions.plot == 2) {
    
    x <- list(title = dimensions.requested[1])
    y <- list(title = dimensions.requested[2])
    
    p <- ggplot(loadings, aes_string(x = dimensions.requested[1],
                                     y = dimensions.requested[2])) +
      scale_color_gradientn(name = label.color, colors = color$color, values = rescale(color$value), limits = c(min(color$value), max(color$value))) +
      coord_cartesian(xlim = xaxislimit, ylim = yaxislimit) +
      geom_segment(aes_string(x = 0, y = 0, xend = dimensions.requested[1], yend = dimensions.requested[2], colour = "norm"), arrow = arrow()) +
      geom_text(aes_string(x = paste(dimensions.requested[1], "*1.1"), y = paste(dimensions.requested[2], "*1.1"), colour = "norm", label = "rownames(loadings)"))
    
    #p <- p + theme_classic()
    
    p <- p + labs(title = title, 
                  subtitle = subtitle,
                  x = pc.lab(dimensions.requested[1]),
                  y = pc.lab(dimensions.requested[2]))
    
    ggsave(filename, p, width = width / 72, height = height / 72, dpi = dpi, scale = 0.75 / scale)
    
  }
  else if(dimensions.plot == 3) {
    
    if(plot3dprovider == "isometric") {
      saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
        par(oma = c(1,7,1,1))
        
        s3d <- scatterplot3d(
          x = loadings[[dimensions.requested[1]]],
          y = loadings[[dimensions.requested[2]]],
          z = loadings[[dimensions.requested[3]]],
          xlab = pc.lab(dimensions.requested[1]),
          ylab = pc.lab(dimensions.requested[2]),
          zlab = pc.lab(dimensions.requested[3]),
          #xlim = xaxislimit,
          #ylim = yaxislimit,
          #zlim = zaxislimit,
          color = rep("transparent", nrow(loadings)),
          type = "h",
          main = title,
          sub = subtitle
        )
        
        s3d.coords.origin <-  s3d$xyz.convert(0, 0, 0)
        s3d.coords <- s3d$xyz.convert(loadings[[dimensions.requested[1]]], loadings[[dimensions.requested[2]]], loadings[[dimensions.requested[3]]])
        
        arrows(x0 = rep(s3d.coords.origin$x, nrow(loadings)),
               y0 = rep(s3d.coords.origin$y, nrow(loadings)),
               x1 = s3d.coords$x,
               y1 = s3d.coords$y)
        
        text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
             labels=rownames(loadings),       # text to plot
             pos=4, cex=.5)                  # shrink text 50% and place to right of points)
        
      })
    }
    else if(plot3dprovider == "perspective") {
      saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
        arrows3D(x0 = rep(0, nrow(loadings)),
                 y0 = rep(0, nrow(loadings)),
                 z0 = rep(0, nrow(loadings)),
                 x1 = loadings[[dimensions.requested[1]]],
                 y1 = loadings[[dimensions.requested[2]]],
                 z1 = loadings[[dimensions.requested[3]]],
                 xlab = pc.lab(dimensions.requested[1]),
                 ylab = pc.lab(dimensions.requested[2]),
                 zlab = pc.lab(dimensions.requested[3]),
                 xlim = xaxislimit,
                 ylim = yaxislimit,
                 zlim = zaxislimit,
                 theta = plot3d.theta,
                 phi = plot3d.phi,
                 
                 colkey = list(at = c(min(color$value), max(color$value))),
                 colvar = loadings$norm,
                 col = color$color,
                 breaks = approx(color$value, n = nrow(color) + 1)$y,
                 
                 nticks = if(plot3d.nticks < 1) 1 else plot3d.nticks,
                 ticktype = if(plot3d.nticks < 1) "simple" else "detailed",
                 main = title,
                 sub = subtitle)
        text3D(x = loadings[[dimensions.requested[1]]] *1.1,
               y = loadings[[dimensions.requested[2]]] *1.1,
               z = loadings[[dimensions.requested[3]]] *1.1,
               labels = rownames(loadings),
               add = T)
      })
    }
    else {
      stop("Unknown plot3d provider!")
    }
    
  }
  
  return(plot.settings)
  
}

#' Logic of the PCA sample plot
#'
#' @param input 
#' @param output 
#' @param session 
#' @param dataset 
#' @param pca.center 
#' @param pca.scale 
#' @param pca.relative 
#'
#' @return
#' @export
#'
#' @examples
plotLoadingsPlot_ <- function(input, 
                            output, 
                            session, 
                            dataset,
                            pca.center,
                            pca.scale,
                            pca.relative,
                            xauto = NULL) {
  
  readcounts.processed <- reactive({
    validate(need(dataset(), "No processed read counts available!"))
    validate(need(dataset()$readcounts.processed, "No processed read counts available!"))
    return(dataset()$readcounts.processed)
  })
  readcounts.filtered <- reactive({
    validate(need(dataset(), "No filtered read counts available!"))
    validate(need(dataset()$readcounts.filtered, "No filtered read counts available!"))
    return(dataset()$readcounts.filtered)
  })
  readcounts.top.variant <- reactive({
    validate(need(dataset(), "No top variant read counts available!"))
    validate(need(dataset()$readcounts.top.variant, "No top variant read counts available!"))
    return(dataset()$readcounts.top.variant)
  })
  gene.variances <- reactive({
    validate(need(dataset(), "No gene variances available!"))
    validate(need(dataset()$variances.filtered, "No gene variances available!"))
    return(dataset()$variances.filtered)
  })
  
  pca <- serverPCA(pca.center,
                   pca.scale,
                   pca.relative,
                   readcounts.top.variant)
  pca.full <- serverPCA(pca.center,
                        pca.scale,
                        pca.relative,
                        readcounts.filtered)

  plot.settings <- generalPlotSettings("plot.settings")
  
  color <- gradientEditorValue("color", plotLoadingsPlot.defaultGradient)
  
  # Update the axis selectize
  observeEvent(readcounts.top.variant(), {
    
    validate(need(readcounts.top.variant(), "Cannot update input without read counts!"))
    
    # New method: We know how many PCx we will get. So allow them. Remove them at plot step
    components <- sapply(1:ncol(readcounts.top.variant()), function(x) { paste0("PC", x) })
    selection <- input$axes
    
    if(length(selection) == 0) {
      selection <- intersect(c("PC1", "PC2"), components)
    }
    
    updateSelectizeInput(session, "axes", choices = components, selected = selection)
    
  })
  
  # Render the plot
  downloadablePlot("plot", plot.settings = plot.settings, exprplot = function( plot.settings, format, filename ){
    
    validate(need(pca(), "No PCA results to plot!"))
    
    plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste("PCA calculated on", nrow(readcounts.top.variant()), "genes")))
    
    return(plotLoadingsPlot.save(pca = pca(),
                               pca.full = pca.full(),
                               axes = input$axes,
                               plot.settings = plot.settings,
                               format = format,
                               plot3dprovider = input$plot3dprovider,
                               plot3d.phi = input$plot3d.phi,
                               plot3d.theta = input$plot3d.theta,
                               plot3d.nticks = input$plot3d.nticks,
                               top = input$top,
                               color = color(),
                               filename = filename))
  })
  
  # xauto exporter that allows triggering of exporting data from code
  xautovars <- reactiveValues(xautocounter = 1)
  
  if(!is.null(xauto)) {
    observeEvent(xauto(), {
      
      filename <- xauto()$filename
      format <- xauto()$format
      
      validate(need(pca(), "No PCA results to plot!"))
      
      plot.settings <- plotSettingsSetNA(plot.settings(), PlotSettings(subtitle = paste("PCA calculated on", nrow(readcounts.top.variant()), "genes")))
      
      return(plotLoadingsPlot.save(pca = pca(),
                                 pca.full = pca.full(),
                                 axes = input$axes,
                                 plot.settings = plot.settings,
                                 format = format,
                                 plot3dprovider = input$plot3dprovider,
                                 plot3d.phi = input$plot3d.phi,
                                 plot3d.theta = input$plot3d.theta,
                                 plot3d.nticks = input$plot3d.nticks,
                                 top = input$top,
                                 color = color(),
                                 filename = filename))
      
      xautovars$xautocounter <- xautovars$xautocounter + 1
      
    })
  }
  
  return(reactive({ xautovars$xautocounter }))
}

plotLoadingsPlot <- function(id, 
                           dataset,
                           pca.center,
                           pca.scale,
                           pca.relative,
                           xauto = NULL) {
  
  return(callModule(plotLoadingsPlot_, 
                    id, 
                    dataset = dataset,
                    pca.center = pca.center,
                    pca.scale = pca.scale,
                    pca.relative = pca.relative,
                    xauto = xauto))
  
}