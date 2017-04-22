#' 
#' Plot module for PCA cell plot
#' 

library(shiny)
library(ggplot2)
library(scatterplot3d)
source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")

plotCellPlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot"),
                                custom.header.items = tagList(
                                  downloadButton(ns("export.mp4"), "Export *.mp4")
                                )))
}

plotCellPlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel("Axes",
                    selectizeInput(ns("axes"),
                                   "Visible axes (x, y, z)",
                                   choices = c(),
                                   multiple = T,
                                   options = list(maxItems = 3, plugins = c("remove_button", "drag_drop")))),
    bsCollapsePanel("Visualization", visualsEditorUI(ns("visuals"))),
    bsCollapsePanel("General settings", generalPlotSettingsInput(ns("plot.settings")))
  ))
  
}

plotCellPlot.save <- function(pca, 
                            visuals.conditions, 
                            visuals.cell, 
                            axes, 
                            plot.settings,
                            format,
                            filename ){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = 640, 
                                                  height = 480,
                                                  dpi = 96,
                                                  scale = 1,
                                                  title = "Cell plot",
                                                  subtitle = "",
                                                  legend.color = "Color",
                                                  legend.shape = "Shape"))
  
  width <- plot.settings@width
  height <- plot.settings@height
  dpi <- plot.settings@dpi
  scale <- plot.settings@scale
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
    
    ggsave(filename, p, width = width / dpi, height = height / dpi, scale = 0.75 / scale)
    
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
    
    ggsave(filename, p, width = width / 72, height = height / 72, dpi = dpi, scale = 0.75 / scale)
    
  }
  else if(dimensions.plot == 3) {
    saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
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
        sub = subtitle,
        pointsize = scale * 12
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

#' Renders a movie showing the PCA cell plot over increasing amount of top variant genes
#'
#' @param filename File name of the rendered video
#' @param animation.params Animation parameters
#' @param axes Vector of displayed axes (PC1, PC2, ...). 
#' @param visuals.conditions Visual definitions for each condition
#' @param visuals.cell Visual definitions for each cell
#' @param readcounts.filtered Filtered read counts. Contains read counts with specific set of genes
#' @param gene.variances Gene variances
#' @param pca.center Center data before PCA
#' @param pca.scale Scale data before PCA
#' @param pca.relative Make transformed cell coordinates relative
#' @param updateProgress Function that takes progress input. Parameters are detail (current operation) and value (current progress)
#'
#' @return
#' @export
#'
#' @examples
plotCellPlot.saveMovie <- function(filename,
                                 plot.settings,
                                 animation.params,
                                 axes, 
                                 visuals.conditions,
                                 visuals.cell,
                                 readcounts.filtered,
                                 gene.variances,
                                 pca.center,
                                 pca.scale,
                                 pca.relative,
                                 updateProgress = NULL) {
  
  if(!is.character(filename) ||!is.character(axes) || missing(visuals.conditions) || missing(visuals.cell) ||
     !is.SummarizedExperiment(readcounts.filtered) || !is.data.frame(gene.variances) || !is.logical(pca.center) || !is.logical(pca.scale)) {
    stop("Invalid arguments!")
  }
  
  basefile <- tempfile()
  genecounts <- unique(c(seq(animation.params$from, animation.params$to, animation.params$by), animation.params$to))
  
  for(i in 1:length(genecounts)) {
    
    if(is.function(updateProgress)) {
      updateProgress(value = i / length(genecounts), detail = "Generating plots")  
    }
    
    readcounts.top.variant <- selectTopVariantGeneReadcounts(readcounts.filtered, gene.variances, genecounts[i])
    plot.filename <- paste0(basefile, "_", i, ".png", collapse = "")
    
    pca <- applyPCA(readcounts.top.variant, center = pca.center, scale = pca.scale, relative = pca.relative)
    plotCellPlot.save(pca = pca,
                    visuals.conditions = visuals.conditions,
                    visuals.cell = visuals.cell,
                    axes = axes,
                    plot.settings = plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste(genecounts[i], "genes"))),
                    format = "png",
                    filename = plot.filename)
    
  }
  
  # Convert to mp4
  if(is.function(updateProgress)) {
    updateProgress(detail = "Rendering video file")
  }
  
  spf <- animation.params$delay / 1000 # Seconds per frames
  fps <- 1 / spf
  
  
  system(paste("ffmpeg",
               "-framerate", fps, 
               "-i", paste0(basefile, "_%d.png"), 
               "-c:v", "libx264",
               filename))
  
  showNotification("Your video file has been successfully rendered.", type = "message")
}

plotCellPlot_ <- function(input, 
                          output, 
                          session, 
                          readcounts.processed, 
                          readcounts.filtered, 
                          readcounts.top.variant, 
                          animation.params,
                          gene.variances,
                          conditions, 
                          pca) {
  
  visuals.conditions <- visualsEditorValue("visuals", reactive({colnames(conditions())}))
  visuals.cell <- reactive({ calculateCellVisuals(colnames(readcounts.processed()), conditions(), visuals.conditions()) })
  plot.settings <- generalPlotSettings("plot.settings")
  
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
    
    validate(need(pca(), "No PCA results to plot!"),
             need(visuals.cell(), "No visual parameters!"))
    
    plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste(nrow(readcounts.top.variant()), "genes")))
    
    return(plotCellPlot.save(pca = pca(),
                            visuals.conditions = visuals.conditions(),
                            visuals.cell = visuals.cell(),
                            axes = input$axes,
                            plot.settings = plot.settings,
                            format = format,
                            filename = filename))
  })
  
  # Download mp4 movie
  output$export.mp4 <- downloadHandler("cell-plot-movie.mp4", function(file) {
    
    validate(
      need(readcounts.filtered(), "No filtered read counts!")
    )
    
    # Disable the export button, so the user doesn't spam it
    on.exit({
      shinyjs::enable("export.mp4")
    })
    shinyjs::disable("export.mp4")
    
    
    withProgressCustom(function(updateProgress) {
      
      plotCellPlot.saveMovie(
        filename = file,
        animation.params = animation.params(),
        axes = input$axes,
        plot.settings = plot.settings(),
        visuals.conditions = visuals.conditions(),
        visuals.cell = visuals.cell(),
        readcounts.filtered = readcounts.filtered(),
        gene.variances = gene.variances(),
        pca.center = pca()$params$center,
        pca.scale = pca()$params$scale,
        pca.relative = pca()$params$relative,
        updateProgress = updateProgress
      )
      
    }, message = "Creating movie")
    
  })
}

plotCellPlot <- function(id, 
                         readcounts.processed, 
                         readcounts.filtered, 
                         readcounts.top.variant, 
                         gene.variances,
                         animation.params,
                         conditions, 
                         pca) {
  
  return(callModule(plotCellPlot_, 
                    id, 
                    readcounts.processed = readcounts.processed,
                    readcounts.filtered = readcounts.filtered, 
                    readcounts.top.variant = readcounts.top.variant, 
                    gene.variances = gene.variances,
                    animation.params = animation.params,
                    conditions = conditions, 
                    pca = pca))
  
}