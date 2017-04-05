library(shiny)
library(shinyBS)
library(ggplot2)
library(scatterplot3d)
library(VennDiagram)

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
  
  plot.settings <- setNA(plot.settings, PlotSettings(
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
  plot.settings <- setNA(plot.settings, 
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
  
  plot.settings <- setNA(plot.settings, 
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

#' Saves a plot of PCA transformed cells with given axes to a file with given format
#'
#' @param pca.transformed Transformed cells
#' @param visuals.conditions Visual parameters for each condition
#' @param visuals.cell Visual parameters for each cell
#' @param customlabel.color Custom label for color legend
#' @param customlabel.shape Custom label for shape legend
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
                            axes, 
                            plot.settings,
                            format,
                            filename ){
  
  plot.settings <- setNA(plot.settings, 
                         PlotSettings(width = 640, 
                         height = 480,
                         dpi = 96,
                         title = "Cell plot",
                         subtitle = "",
                         legend.color = "Color",
                         legend.shape = "Shape"))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  title <- plot.settings@title
  subtitle <- plot.settings@subtitle
  customlabel.color <- plot.settings@legend.color
  customlabel.shape <- plot.settings@legend.shape
  
  # Soft and hard parameter checking
  validate(
    need(pca, "No PCA results available!"),
    need(axes, "No axes to draw!"),
    need(visuals.conditions, "No condition visual parameters available!"),
    need(visuals.cell, "No cell visual parameters available!"))
  
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
    #p <- p + theme_classic()
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
    #p <- p + theme_classic()
    
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
    saveRPlot(width, height, dpi, filename, format, expr = function() {
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
      
      # Print legends outside of plot. Who thought that taking the plot API from S language is a good idea?
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
    })
  }
  
  return(plot.settings)
  
}
