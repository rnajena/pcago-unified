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

#' Saves a Venn diagram of cell conditions
#'
#' @param selected.conditions 
#' @param conditions 
#' @param pca.conditions.plot.visuals 
#' @param plot.settings 
#' @param format 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
saveCellConditionVennDiagramPlot <- function(selected.conditions, conditions, pca.conditions.plot.visuals, plot.settings, format, filename) {
  validate(need(conditions(), "No conditions to plot!"),
           need(pca.conditions.plot.visuals(), "No condition visuals available!"))
  
  validate(need(selected.conditions, "No conditions selected!"),
           need(length(selected.conditions) > 0, "No conditions selected!"),
           need(length(selected.conditions) <= 5, "Too many conditions selected!"))
  
  x <- lapply(selected.conditions, function(x) { 
    
    indices <- conditions()[[x]]
    return(rownames(conditions())[indices]) 
    
  })
  names(x) <- selected.conditions
  
  plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(
    width = 640,
    height = 480,
    dpi = 96,
    title = "Conditions",
    subtitle = ""
  ))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
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

#' Plots the principal component variances
#'
#' @param pca 
#' @param plot.settings 
#' @param format 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
savePCAVariancePlot <- function(pca, plot.settings, format, filename) {
  plot.settings <- plotSettingsSetNA(plot.settings, 
                         PlotSettings(width = 640, 
                                      height = 480,
                                      dpi = 96,
                                      title = "Principal component variances",
                                      subtitle = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  
  plot.y.label <- paste0("Relative variance (to ", sum(pca$var), ")")
  
  
  p <- ggplot(pca$var, aes(x=factor(rownames(pca$var), levels = rownames(pca$var)), y=var.relative)) + geom_point()
  p <- p + labs(x = "Principal component", y = plot.y.label, title = title, subtitle = subtitle)
  ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
  
  return(plot.settings)
}

#' Saves a plot of gene variances to a file with given format
#'
#' @param gene.variances Gene variances
#' @param width 
#' @param height 
#' @param dpi 
#' @param format 
#' @param filename 
#' @param logarithmic Display logarithmic values
#'
#' @return
#' @export
#'
#' @examples
saveGeneVariancePlot <- function(gene.variances, plot.settings, format, filename, logarithmic = F){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                         PlotSettings(width = 640, 
                         height = 480,
                         dpi = 96,
                         title = "Gene variances",
                         subtitle = ""))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
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
  ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
  
  return(plot.settings)
  
}