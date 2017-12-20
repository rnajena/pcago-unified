#' 
#' Plot module for variance plot
#' 

library(shiny)
library(ggplot2)
library(dendextend)
library(ctc)
library(gplots)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")
source("widgetGradientEditor.R")
source("defaultParameters.R")

plotAgglomerativeClusteringPlotUI.dist.methodsSelection <- c(
  "Euclidean" = "euclidean",
  "Maximum" = "maximum",
  "Manhattan" = "manhattan",
  "Canberra" = "canberra",
  "Binary" = "binary",
  "Minkowski" = "minkowski"
)

plotAgglomerativeClusteringPlotUI.hclust.methodsSelection <- c(
  "Average" = "average",
  "McQuitty" = "mcquitty",
  "Median" = "median",
  "Centroid" = "centroid",
  "Ward D" = "ward.D",
  "Ward D2" = "ward.D2",
  "Single linkage" = "single",
  "Complete linkage" = "complete"
)

plotAgglomerativeClusteringPlotUI.plotTypes <- c(
  "Dendrogram (horizontal)" = "dendrogram.horizontal",
  "Dendrogram (vertical)" = "dendrogram.vertical",
  "Heatmap Pearson correlation" = "cor.pearson",
  "Heatmap Spearman correlation" = "cor.spearman"
)

plotAgglomerativeClusteringPlotUI.dendrogramPositions <- c(
  "Row + Column" = "both",
  "Row only" = "row",
  "Column only" = "column",
  "No dendrograms" = "none"
)

plotAgglomerativeClusteringPlotUI.heatmapDensityInfo <- c(
  "No density info" = "none",
  "Histogram" = "histogram",
  "Density plot" = "density"
)

plotAgglomerativeClusteringPlotUI.heatmapTrace <- c(
  "No trace" = "none",
  "Row + Column" = "both",
  "Row only" = "row",
  "Column only" = "column"
)

plotAgglomerativeClusteringPlot.defaultGradient <- importGradientSample("Gradients/HeatmapRdBu.csv", list())

plotAgglomerativeClusteringPlotUI <- function(id) {
  
  ns <- NS(id)
  
  export.tree.buttons <- tagList(
    downloadButton(ns("export.newick"), "as *.newick")
  )
  
  return(downloadablePlotOutput(ns("plot"),
                                custom.header.items = dropdownButton(ns("export.tree"), 
                                                                     "Export tree",
                                                                     icon = icon("download"),
                                                                     export.tree.buttons)))
}

plotAgglomerativeClusteringPlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel(recommendedDataText("Plot type"),
                    value = "plottype",
                    selectizeInput(ns("plottype"), "Plot type", choices = plotAgglomerativeClusteringPlotUI.plotTypes),
                    conditionalPanel(paste(conditionalPanel.equals(ns("plottype"), "'cor.pearson'"), "||", conditionalPanel.equals(ns("plottype"), "'cor.spearman'")),
                                     selectizeInput(ns("heatmap.dendrograms"), "Visible dendrograms", choices = plotAgglomerativeClusteringPlotUI.dendrogramPositions),
                                     selectizeInput(ns("heatmap.density.info"), "Density info", choices = plotAgglomerativeClusteringPlotUI.heatmapDensityInfo),
                                     selectizeInput(ns("heatmap.trace"), "Trace line", choices = plotAgglomerativeClusteringPlotUI.heatmapTrace),
                                     colourInput(ns("heatmap.tracecol"), "Trace line color", value = "cyan", allowTransparent = T))
                    ),
    bsCollapsePanel(recommendedDataText("Hierarchical clustering"),
                    value = "hierarchical.clustering",
                    selectizeInput(ns("method.dist"), "Distance method", choices = plotAgglomerativeClusteringPlotUI.dist.methodsSelection),
                    selectizeInput(ns("method.hclust"), "Clustering method", choices = plotAgglomerativeClusteringPlotUI.hclust.methodsSelection)),
    bsCollapsePanel(optionalDataText("Visualization"), 
                    value = "visualization",
                    conditionalPanel(paste(conditionalPanel.equals(ns("plottype"), "'cor.pearson'"), "||", conditionalPanel.equals(ns("plottype"), "'cor.spearman'")), gradientEditorUI(ns("heatmap.color"))),
                    conditionalPanel(paste(conditionalPanel.equals(ns("plottype"), "'dendrogram.horizontal'"), "||", conditionalPanel.equals(ns("plottype"), "'dendrogram.vertical'")), visualsEditorUI(ns("visuals")))
                    ),
    bsCollapsePanel(optionalDataText("General settings"), 
                    value = "generalsettings",
                    generalPlotSettingsInput(ns("plot.settings"),
                                                                 legend.color = F,
                                                                 legend.shape = F))
  ))
  
}

plotAgglomerativeClusteringPlot.save <- function(readcounts, 
                                                 plot.settings, 
                                                 sample.visuals, 
                                                 format, 
                                                 filename, 
                                                 method.distance = "euclidian", 
                                                 method.cluster = "average",
                                                 plottype = "dendrogram.horizontal",
                                                 heatmap.dendrograms = "both",
                                                 heatmap.density.info = "none",
                                                 heatmap.trace = "none",
                                                 heatmap.tracecol = "cyan",
                                                 heatmap.color = plotAgglomerativeClusteringPlot.defaultGradient){
  
  validate(need(is.matrix(readcounts()) || is.SummarizedExperiment(readcounts()), "No data to plot!"),
           need(sample.visuals(), "No sample visuals available!"))
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = default.plot.width, 
                                                  height = default.plot.height,
                                                  dpi = default.plot.dpi,
                                                  scale = 1,
                                                  title = "Hierarchical Clustering",
                                                  subtitle = ""))
  
  if(plottype == "cor.pearson") {
    plot.settings <- plotSettingsSetNA(plot.settings,
                                       PlotSettings(legend.color = "Pearson correlation"))
  }
  else if(plottype == "cor.spearman") {
    plot.settings <- plotSettingsSetNA(plot.settings,
                                       PlotSettings(legend.color = "Spearman correlation"))
  }
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  legend.color <- plot.settings@legend.color
  
  palette.colors <- sample.visuals()$palette.colors
  palette.shapes <- sample.visuals()$palette.shapes
  
  saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
    
    X <- if(is.matrix(readcounts())) readcounts() else assay(readcounts())
    
    if(plottype == "dendrogram.horizontal") {
      # Agglomerative clustering plot
      dend <- t(X) %>%
        dist(method = method.distance) %>%
        hclust(method = method.cluster) %>%
        as.dendrogram
      
      dend.samples <- labels(dend)
      dend.factors <- sample.visuals()$factors[dend.samples,]
      
      dend <- dend %>% dendextend::set("leaves_pch", palette.shapes[as.numeric(dend.factors$shape)]) %>%
        dendextend::set("leaves_col", palette.colors[as.numeric(dend.factors$color)])
      
      par(mar = c(5,4,4,10))
      dend %>% plot(main = title, sub = subtitle, horiz = T, cex = 0.6)
      
      # par(mar = c(5,4,4,10))
      # dend <- hclust(dist(t(X), method = method.distance), method = method.cluster)
      # plot(dend, main = title, sub = subtitle)
    }
    else if(plottype == "dendrogram.vertical") {
      # Agglomerative clustering plot
      dend <- t(X) %>%
        dist(method = method.distance) %>%
        hclust(method = method.cluster) %>%
        as.dendrogram
      
      dend.samples <- labels(dend)
      dend.factors <- sample.visuals()$factors[dend.samples,]
      
      dend <- dend %>% dendextend::set("leaves_pch", palette.shapes[as.numeric(dend.factors$shape)]) %>%
        dendextend::set("leaves_col", palette.colors[as.numeric(dend.factors$color)])
      
      par(mar = c(5,4,4,10))
      dend %>% plot(main = title, sub = subtitle, horiz = F, cex = 0.6)
      
      # par(mar = c(5,4,4,10))
      # dend <- hclust(dist(t(X), method = method.distance), method = method.cluster)
      # plot(dend, main = title, sub = subtitle)
    }
    else {
      # Heatmap plots
      
      validate(need(nrow(heatmap.color) > 1, "No valid gradient map defined!"))
      
      cor.method <- "pearson"
      
      if(plottype == "cor.kendall") {
        cor.method <- "kendall"
      }
      else if(plottype == "cor.spearman") {
        cor.method <- "spearman"
      }
      
      palette <- colorRampPalette(heatmap.color[, "color"])
      color.breaks <- c()
      
      for(i in 1:(nrow(heatmap.color) - 1)) {
        color.breaks <- c(color.breaks, seq(heatmap.color[i, "value"], heatmap.color[i + 1, "value"], length.out = 100))
      }
      
      color.breaks <- unique(color.breaks)
      
      heatmap.2(cor(X, method = cor.method),
                hclustfun = function(x) hclust(x, method = method.cluster),
                distfun = function(x) dist(x, method = method.distance),
                symm = T,
                main = title,
                sub = subtitle,
                margins = c(12, 9),
                dendrogram = heatmap.dendrograms,
                density.info = heatmap.density.info,
                trace = heatmap.trace,
                tracecol = heatmap.tracecol,
                col = palette,
                breaks = color.breaks,
                key.xlab = legend.color)
      
    }
    
    
  })
  
  return(plot.settings)
  
}

plotAgglomerativeClusteringPlot.saveNewick <- function(readcounts,
                                           filename,
                                           method.distance = "euclidian", 
                                           method.cluster = "average") {
  
  validate(need(is.matrix(readcounts()) || is.SummarizedExperiment(readcounts()), "No data to cluster!"))
  
  X <- if(is.matrix(readcounts())) readcounts() else assay(readcounts())
  
  clust <- t(X) %>% 
    dist(method = method.distance) %>%
    hclust(method = method.cluster)
  
  write(ctc::hc2Newick(clust), file = filename)
  
}

plotAgglomerativeClusteringPlot_ <- function(input, 
                                  output, 
                                  session, 
                                  conditions,
                                  readcounts,
                                  default.title,
                                  xauto) {
  
  plot.settings <- generalPlotSettings("plot.settings")
  visuals.conditions <- visualsEditorValue("visuals", reactive({colnames(conditions())}))
  visuals.sample <- reactive({ calculateSampleVisuals(colnames(readcounts()), conditions(), visuals.conditions()) })
  
  heatmap.color <- gradientEditorValue("heatmap.color", default.gradient = plotAgglomerativeClusteringPlot.defaultGradient)
  
  # Provide plot height that scales with sample count
  plot.settings.dynamic <- reactive({
    
    validate(need(is.matrix(readcounts()) || is.SummarizedExperiment(readcounts()), "No data to build plot settings from!"))
    
    settings <- plotSettingsSetNA(plot.settings(),
                                  PlotSettings(dpi = default.plot.dpi,
                                               scale = 1))
    
    # Calculate the plot size based on the count of samples
    if(input$plottype == "dendrogram.horizontal") {
      height <- (1 + 0.4 * ncol(readcounts())) * settings@dpi * settings@scale
      settings <- plotSettingsSetNA(plot.settings(),
                                    PlotSettings(height = height))
    }
    else  if(input$plottype == "dendrogram.vertical") {
      width <- (1 + 0.4 * ncol(readcounts())) * settings@dpi * settings@scale
      settings <- plotSettingsSetNA(plot.settings(),
                                    PlotSettings(width = width))
    }
    else {
      height <- (1 + 0.2 * ncol(readcounts())) * settings@dpi * settings@scale
      settings <- plotSettingsSetNA(plot.settings(),
                                    PlotSettings(height = height))
    }
    
    return(settings)
    
  })
  
  # Plot
  downloadablePlot("plot", 
                   plot.settings = plot.settings.dynamic, 
                   exprplot = function(plot.settings, format, filename) 
                   {
                     distance.methods <- plotAgglomerativeClusteringPlotUI.dist.methodsSelection
                     clustering.methods <- plotAgglomerativeClusteringPlotUI.hclust.methodsSelection
                     
                     distance.method.name <- names(distance.methods)[distance.methods == input$method.dist]
                     clustering.method.name <- names(clustering.methods)[clustering.methods == input$method.hclust]
                     
                     plot.settings <- plotSettingsSetNA(plot.settings,
                                                        PlotSettings(subtitle = paste0(distance.method.name, " distance, ", clustering.method.name),
                                                                     title = default.title()))
                     
                     return(plotAgglomerativeClusteringPlot.save(readcounts, 
                                                                 plot.settings, 
                                                                 format, 
                                                                 filename,
                                                                 sample.visuals = visuals.sample,
                                                                 method.distance = input$method.dist,
                                                                 method.cluster = input$method.hclust,
                                                                 plottype = input$plottype,
                                                                 heatmap.dendrograms = input$heatmap.dendrograms,
                                                                 heatmap.density.info = input$heatmap.density.info,
                                                                 heatmap.trace = input$heatmap.trace,
                                                                 heatmap.tracecol = input$heatmap.tracecol,
                                                                 heatmap.color = heatmap.color()))
                   })
  
  # Download tree as NEWICK
  output$export.newick <- downloadHandler("clustering.newick", function(file) {
    plotAgglomerativeClusteringPlot.saveNewick(readcounts, file, input$method.dist, input$method.hclust)
  })
  
  # xauto exporter that allows triggering of exporting data from code
  xautovars <- reactiveValues(xautocounter = 1)
  
  if(!is.null(xauto)) {
    observeEvent(xauto(), {
      if(!is.null(xauto()$filename.newick)) {
        plotAgglomerativeClusteringPlot.saveNewick(readcounts, xauto()$filename.newick, input$method.dist, input$method.hclust)
      }
      if(!is.null(xauto()$filename.svg)) {
        
        filename <- xauto()$filename.svg
        distance.methods <- plotAgglomerativeClusteringPlotUI.dist.methodsSelection
        clustering.methods <- plotAgglomerativeClusteringPlotUI.hclust.methodsSelection
        
        distance.method.name <- names(distance.methods)[distance.methods == input$method.dist]
        clustering.method.name <- names(clustering.methods)[clustering.methods == input$method.hclust]
        
        plot.settings <- plotSettingsSetNA(plot.settings(),
                                           PlotSettings(subtitle = paste0(distance.method.name, " distance, ", clustering.method.name),
                                                        title = default.title()))
        
        return(plotAgglomerativeClusteringPlot.save(readcounts, 
                                                    plot.settings, 
                                                    "svg", 
                                                    filename,
                                                    sample.visuals = visuals.sample,
                                                    method.distance = input$method.dist,
                                                    method.cluster = input$method.hclust,
                                                    plottype = input$plottype,
                                                    heatmap.dendrograms = input$heatmap.dendrograms,
                                                    heatmap.density.info = input$heatmap.density.info,
                                                    heatmap.trace = input$heatmap.trace,
                                                    heatmap.tracecol = input$heatmap.tracecol,
                                                    heatmap.color = heatmap.color()))
        
      }
      
      xautovars$xautocounter <- xautovars$xautocounter + 1
      
    })
  }
  
  return(reactive({ xautovars$xautocounter }))
  
}

#' Agglomerative clustering plot
#'
#' @param id 
#' @param conditions 
#' @param readcounts 
#' @param default.title 
#' @param xauto If not null returns a function that returns list(filename.svg = <>, filename.newick = <>)
#'
#' @return
#' @export
#'
#' @examples
plotAgglomerativeClusteringPlot <- function(id, 
                                            conditions, 
                                            readcounts, 
                                            default.title = reactive({"Hierarchical clustering"}),
                                            xauto = NULL) {
  
  return(callModule(plotAgglomerativeClusteringPlot_, 
                    id, 
                    conditions = conditions,
                    readcounts = readcounts,
                    default.title = default.title,
                    xauto = xauto))
  
}