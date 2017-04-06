library(shiny)
library(shinyBS)
library(ggplot2)
library(scatterplot3d)
library(VennDiagram)
library(dendextend)

#' Title
#'
#' @param readcounts 
#' @param plot.settings 
#' @param cell.visuals 
#' @param format 
#' @param filename 
#' @param method.distance 
#' @param method.cluster 
#'
#' @return
#' @export
#'
#' @examples
saveAgglomerativeClusterPlot <- function(readcounts, 
                                         plot.settings, 
                                         cell.visuals, 
                                         format, 
                                         filename, 
                                         method.distance = "euclidian", 
                                         method.cluster = "average") {
  
  validate(need(readcounts(), "No data to plot!"),
           need(cell.visuals(), "No cell visuals available!"))
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                         PlotSettings(width = 640, 
                                      height = 480,
                                      dpi = 96,
                                      title = "Cluster Dendrogram",
                                      subtitle = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  
  palette.colors <- cell.visuals()$palette.colors
  palette.shapes <- cell.visuals()$palette.shapes
  
  saveRPlot(width, height, dpi, filename, format, expr = function() {
    
    #plot(hclust(dist(t(readcounts()), method = method.distance), method = method.cluster))
    dend <- t(readcounts()) %>% 
      dist(method = method.distance) %>%
      hclust(method = method.cluster) %>%
      as.dendrogram
      
    dend.cells <- labels(dend)
    dend.factors <- cell.visuals()$factors[dend.cells,]
    
    dend <- dend %>% dendextend::set("leaves_pch", palette.shapes[as.numeric(dend.factors$shape)]) %>%
      dendextend::set("leaves_col", palette.colors[as.numeric(dend.factors$color)])
    
    par(mar = c(5,4,4,10))
    dend %>% plot(main = title, sub = subtitle, horiz = T, cex = 0.6)
    
  })
  
  return(plot.settings)
}
