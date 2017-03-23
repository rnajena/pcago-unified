library(shiny)
library(shinyBS)

#' Saves a plot of gene variances to a file with given format
#'
#' @param gene.variances Gene variances
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
saveGeneVariancePlot <- function(gene.variances, width, height, dpi, format, filename){
  
  # Soft and hard parameter checking
  validate(need(gene.variances, "No gene variances available!"))
  
  if(!is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) || !is.character(format) || !is.character(filename)) {
    stop("Invalid arguments!")
  }
  
  p <- ggplot(gene.variances, aes(x=1:nrow(gene.variances), y=log(var))) + geom_point()
  p <- p + labs(x = "Top n-th variant gene", y = "log(σ²)")
  ggsave(filename, p, width = width / dpi, height = height / dpi, device = format)
  
}

#' Saves a plot of PCA transformed cells with given axes to a file with given format
#'
#' @param pca.transformed Transformed cells
#' @param visuals.conditions Visual parameters for each condition
#' @param visuals.cell Visual parameters for each cell
#' @param customlabel.color Custom label for color
#' @param customlabel.shape Custom label for shape
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
savePCACellPlot <- function(pca, 
                            visuals.conditions, 
                            visuals.cell, 
                            customlabel.color,
                            customlabel.shape,
                            axes, 
                            width, 
                            height, 
                            dpi, 
                            format,
                            filename,
                            title = "Cell PCA", 
                            subtitle = NULL ){
  
  # Soft and hard parameter checking
  validate(
    need(pca, "No PCA results available!"),
    need(axes, "No axes to draw!"),
    need(visuals.conditions, "No condition visual parameters available!"),
    need(visuals.cell, "No cell visual parameters available!"))
  
  if(!is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) || !is.character(format) || !is.character(filename)) {
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
  
  validate(need(dimensions.plot > 0, "No axes to draw!"),
           need(dimensions.plot <= 3, "Too many axes to draw!"))
  
  # Add visual properties to the variables
  pca.transformed$color <- visuals.cell$factors$color
  pca.transformed$shape <- visuals.cell$factors$shape
  
  palette.colors <- visuals.cell$palette.colors
  palette.shapes <- visuals.cell$palette.shapes
  
  label.color <- if(customlabel.color == "") "Color" else customlabel.color
  label.shape <- if(customlabel.shape == "") "Shape" else customlabel.shape
  
  # Plot based on dimensions
  if(dimensions.plot == 1) {
    
    x <- list(title = dimensions.requested[1])
    
    p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1])) + 
      geom_histogram(aes(fill = color), bins = 100)
    p <- p + scale_fill_manual(name = label.color,
                                values = palette.colors,
                                breaks = levels(pca.transformed$color),
                                labels = conditionName(visuals.conditions, levels(pca.transformed$color)))
    p <- p + labs(title = title, 
                  subtitle = subtitle,
                  x = pc.lab(dimensions.requested[1]))
    #p <- p + scale_linetype_manual(values = palette.shapes)
    #TODO: Fix Color ; Add linetype as replacement for pch?
    
    if(setequal(levels(pca.transformed$color), c("{default}"))) {
      p <- p + guides(color = F)
    }
    
    ggsave(filename, p, width = width / dpi, height = height / dpi)
    
  }
  else if(dimensions.plot == 2) {
    
    x <- list(title = dimensions.requested[1])
    y <- list(title = dimensions.requested[2])
    
    p <- ggplot(pca.transformed, aes_string(x = dimensions.requested[1],
                                            y = dimensions.requested[2])) + 
      geom_point(aes(colour = color, shape = shape))
    
    #' Add legends to the plot and map the correct colors to the factor levels
    p <- p + scale_color_manual(name = label.color,
                                values = palette.colors,
                                breaks = levels(pca.transformed$color),
                                labels = conditionName(visuals.conditions, levels(pca.transformed$color)))
    p <- p + scale_shape_manual(name = label.shape,
                                values = palette.shapes,
                                breaks = levels(pca.transformed$shape),
                                labels = conditionName(visuals.conditions, levels(pca.transformed$shape)))
    p <- p + labs(title = title, 
                  subtitle = subtitle,
                  x = pc.lab(dimensions.requested[1]),
                  y = pc.lab(dimensions.requested[2]))
    
    if(setequal(levels(pca.transformed$color), c("{default}"))) {
      p <- p + guides(color = F)
    }
    if(setequal(levels(pca.transformed$shape), c("{default}"))) {
      p <- p + guides(shape = F)
    }
    
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
      xlab = pc.lab(dimensions.requested[1]),
      ylab = pc.lab(dimensions.requested[2]),
      zlab = pc.lab(dimensions.requested[3]),
      type = "h",
      main = title,
      sub = subtitle
    )
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    legend("topleft",
           legend = conditionName(visuals.conditions, levels(pca.transformed$color)),
           col = palette.colors,
           pch = 16,
           bty = "n",
           xpd = T,
           title = label.color,
           title.adj = 0) # wtf?
    legend("bottomleft",
           legend = conditionName(visuals.conditions, levels(pca.transformed$shape)),
           col = "black",
           pch = palette.shapes,
           bty = "n",
           xpd = T,
           title = label.shape,
           title.adj = 0)
    
    dev.off()
    
  }
  
}