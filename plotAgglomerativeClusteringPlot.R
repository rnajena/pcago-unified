#' 
#' Plot module for variance plot
#' 

library(shiny)
library(ggplot2)
library(dendextend)
library(ctc)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")

plotAgglomerativeClusteringPlotUI.dist.methodsSelection <- c(
  "Euclidean" = "euclidean",
  "Maximum" = "maximum",
  "Manhattan" = "manhattan",
  "Canberra" = "canberra",
  "Binary" = "binary",
  "Minkowski" = "minkowski"
)

plotAgglomerativeClusteringPlotUI.hclust.methodsSelection <- c(
  "UPGMA/Average" = "average",
  "WPGMA/McQuitty" = "mcquitty",
  "WPGMC/Median" = "median",
  "UPGMC/Centroid" = "centroid",
  "Ward D" = "ward.D",
  "Ward D2" = "ward.D2",
  "Single linkage" = "single",
  "Complete linkage" = "complete"
)

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
    bsCollapsePanel("Hierarchical clustering",
                    selectizeInput(ns("method.dist"), "Distance method", choices = plotAgglomerativeClusteringPlotUI.dist.methodsSelection),
                    selectizeInput(ns("method.hclust"), "Clustering method", choices = plotAgglomerativeClusteringPlotUI.hclust.methodsSelection)),
    bsCollapsePanel("Visualization", visualsEditorUI(ns("visuals"))),
    bsCollapsePanel("General settings", generalPlotSettingsInput(ns("plot.settings"),
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
                                                 method.cluster = "average"){
  
  validate(need(is.matrix(readcounts()) || is.SummarizedExperiment(readcounts()), "No data to plot!"),
           need(sample.visuals(), "No sample visuals available!"))
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = 640, 
                                                  height = 480,
                                                  dpi = 96,
                                                  scale = 1,
                                                  title = "Hierarchical Clustering",
                                                  subtitle = ""))
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  
  palette.colors <- sample.visuals()$palette.colors
  palette.shapes <- sample.visuals()$palette.shapes
  
  saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
    
    X <- if(is.matrix(readcounts())) readcounts() else assay(readcounts())
    
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
                                  default.title) {
  
  plot.settings <- generalPlotSettings("plot.settings")
  visuals.conditions <- visualsEditorValue("visuals", reactive({colnames(conditions())}))
  visuals.sample <- reactive({ calculateSampleVisuals(colnames(readcounts()), conditions(), visuals.conditions()) })
  
  # Provide plot height that scales with sample count
  plot.settings.dynamic <- reactive({
    
    validate(need(is.matrix(readcounts()) || is.SummarizedExperiment(readcounts()), "No data to build plot settings from!"))
    
    settings <- plotSettingsSetNA(plot.settings(),
                                  PlotSettings(dpi = 96,
                                               scale = 1))
    
    # Calculate the plot height based on the count of samples
    height <- (1 + 0.4 * ncol(readcounts())) * settings@dpi * settings@scale
    settings <- plotSettingsSetNA(plot.settings(),
                                  PlotSettings(height = height))
    
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
                                                                 method.cluster = input$method.hclust))
                   })
  
  # Download tree as NEWICK
  output$export.newick <- downloadHandler("clustering.newick", function(file) {
    plotAgglomerativeClusteringPlot.saveNewick(readcounts, file, input$method.dist, input$method.hclust)
  })
  
}

plotAgglomerativeClusteringPlot <- function(id, conditions, readcounts, default.title = reactive({"Hierarchical clustering"})) {
  
  return(callModule(plotAgglomerativeClusteringPlot_, 
                    id, 
                    conditions = conditions,
                    readcounts = readcounts,
                    default.title = default.title))
  
}