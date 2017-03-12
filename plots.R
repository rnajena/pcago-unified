
library(shiny)
library(shinyBS)

#' Summary of read count processing.
#' Use as expression inside renderUI
#'
#' @return
#' @export
#'
#' @examples
serverReadCountsProcessingOutput <- function(input, readcounts.processed, readcounts.processing.output) {
  
  validate(need(readcounts.processed(), "No processed read counts available."))
  
  panels <- list()
  
  # Transpose processing
  if("transpose" %in% input$pca.data.readcounts.processing) {
    panels[[length(panels) + 1]] <- bsCollapsePanel(title = "Transpose table", "Read counts have been transposed.")
  }
  
  # Remove constant reads processing
  if("remove.constant" %in% input$pca.data.readcounts.processing) {
    
    content <- "No genes have been removed."
    removed.genes <- readcounts.processing.output()$removed.genes
    
    if(length(removed.genes) != 0) {
      
      genes <- paste(removed.genes, collapse = ", ")
      content <- paste(length(removed.genes) ,"genes have been removed:", genes)
    }
    
    panels[[length(panels) + 1]] <- bsCollapsePanel(title = "Remove genes with constant read counts", content)
  }
  
  if(length(panels) == 0) {
    return(tags$div)
  }
  else {
    return(do.call(bsCollapse, panels))
  }
  
}

#' Saves a plot of gene variances to a file with given format
#'
#' @param annotation Gene annotation
#' @param width 
#' @param height 
#' @param dpi 
#' @param format 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
geneVariancePlot <- function(annotation, width, height, dpi, format, filename){
  
  p <- ggplot(annotation, aes(x=1:nrow(annotation), y=log(var))) + geom_point()
  ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
  
}

#' Saves a plot of PCA transformed cells with given axes to a file with given format
#'
#' @param pca.transformed Transformed cells
#' @param visuals.cell Visual parameters for each cell
#' @param axes The axes to be plotted (PC1, PC2, ...). Up to 3 axes can be plotted.
#' @param width 
#' @param height 
#' @param dpi 
#' @param format 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
pcaCellPlot <- function(pca.transformed, visuals.cell, axes, width, height, dpi, format, filename ){
  
  validate(need(axes, "No axes to draw!"))
  
  # Fetch needed variables from PCA and visual parameters
  pca.transformed <- pca.transformed
  pca.transformed$color <- visuals.cell$factors$color
  pca.transformed$shape <- visuals.cell$factors$shape
  
  palette.colors <- visuals.cell$palette.colors
  palette.shapes <- visuals.cell$palette.shapes
  
  # Determine how many dimensions should be drawn
  dimensions.available <- ncol(pca.transformed)
  dimensions.requested <- axes
  
  dimensions.plot <- min(length(dimensions.requested), dimensions.available) 
  
  # Plot based on dimensions
  if(dimensions.plot == 1) {
    
    x <- list(title = dimensions.requested[1])
    
    p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1])) + 
      geom_histogram(aes(fill = factor(color)), bins = 100)
    
    ggsave(filename, p, width = width / dpi, height = height / dpi)
    
  }
  else if(dimensions.plot == 2) {
    
    x <- list(title = dimensions.requested[1])
    y <- list(title = dimensions.requested[2])
    
    p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1],
                                            y = dimensions.requested[2])) + 
      geom_point(aes(colour = color, shape = shape))
    p <- p + scale_color_manual(values = palette.colors)
    p <- p + scale_shape_manual(values = palette.shapes)
    
    ggsave(filename, p, width = width / dpi, height = height / dpi)
    
  }
  else if(dimensions.plot == 3) {
    
    if(format == "svg") {
      svg(filename = filename,
          width = width / dpi,
          height = height / dpi)
    }
    else if(format == "png") {
      png(filename = filename,
          width = width,
          height = height,
          res = dpi)
    }
    
    par(oma = c(1,7,1,1))
    
    scatterplot3d(
      x = pca.transformed[[dimensions.requested[1]]],
      y = pca.transformed[[dimensions.requested[2]]],
      z = pca.transformed[[dimensions.requested[3]]],
      color = palette.colors[as.numeric(pca.transformed$color)],
      pch = palette.shapes[as.numeric(pca.transformed$shape)],
      xlab = dimensions.requested[1],
      ylab = dimensions.requested[2],
      zlab = dimensions.requested[3],
      type = "h"
    )
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    legend("topleft",
           legend = levels(pca.transformed$color),
           col = palette.colors,
           pch = 16,
           bty = "n",
           xpd = T,
           title = "Color",
           title.adj = 0) # wtf?
    legend("bottomleft",
           legend = levels(pca.transformed$shape),
           col = "black",
           pch = palette.shapes,
           bty = "n",
           xpd = T,
           title = "Shape",
           title.adj = 0)
    
    dev.off()
    
  }
  
}