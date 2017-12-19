#' 
#' Plot module for PCA sample plot
#' 

library(shiny)
library(ggplot2)
library(scatterplot3d)
library(plot3D)

source("widgetVisualsEditor.R")
source("widgetDownloadablePlot.R")
source("widgetInPlaceHelp.R")
source("widgetNumericRangeInput.R")
source("environment.R")
source("defaultParameters.R")

plotSamplePlotSettingsUI.axisLimitModes <- c("Auto" = "auto", "Auto (All genes)" = "allgenes", "Manual" = "manual")

plotSamplePlotSettingsUI.3dplotProvider <- c("Isometric" = "isometric", "Perspective" = "perspective")

plotSamplePlotUI <- function(id) {
  
  ns <- NS(id)
  
  return(downloadablePlotOutput(ns("plot"),
                                custom.header.items = tagList(
                                  actionButton(ns("export.mp4"), "Export *.mp4", icon = icon("download"))
                                )))
}

plotSamplePlotSettingsUI <- function(id) {
  
  ns <- NS(id)
  
  return(bsCollapse(
    bsCollapsePanel(recommendedDataText("Plot type"),
                    value = "plottype",
                     selectizeInput(ns("plot3dprovider"), "3D plot type", choices = plotSamplePlotSettingsUI.3dplotProvider),
                     conditionalPanel(conditionalPanel.equals(ns("plot3dprovider"), "'perspective'"),
                                      numericInput(ns("plot3d.theta"), "Rotation (Degree)", value = 40),
                                      numericInput(ns("plot3d.phi"), "Viewing angle (Degree)", value = 40),
                                      numericInput(ns("plot3d.nticks"), "Number of ticks", value = 5))
                    ),
    bsCollapsePanel(recommendedDataText("Axes"),
                    value = "axes",
                    selectizeInput(ns("axes"),
                                   "Visible axes (x, y, z)",
                                   choices = c(),
                                   multiple = T,
                                   options = list(maxItems = 3, plugins = c("remove_button", "drag_drop")))),
    bsCollapsePanel(optionalDataText("Visualization"), 
                    value = "visualization",
                    visualsEditorUI(ns("visuals"))),
    bsCollapsePanel(optionalDataText("Axis limits"),
                    value = "axis.limits",
                    tags$label("x Axis limit"),
                    tags$div(class = "row sub-labeled",
                             column(width = 3, selectizeInput(ns("axis.limitx.mode"), "Mode", choices = plotSamplePlotSettingsUI.axisLimitModes)),
                             column(width = 9, numericRangeInput(ns("axis.limitx"), "min", "max"))
                             
                    ),
                    tags$label("y Axis limit"),
                    tags$div(class = "row sub-labeled",
                             column(width = 3, selectizeInput(ns("axis.limity.mode"), "Mode", choices = plotSamplePlotSettingsUI.axisLimitModes)),
                             column(width = 9, numericRangeInput(ns("axis.limity"), "min", "max"))
                             
                    ),
                    tags$label("z Axis limit"),
                    tags$div(class = "row sub-labeled",
                             column(width = 3, selectizeInput(ns("axis.limitz.mode"), "Mode", choices = plotSamplePlotSettingsUI.axisLimitModes)),
                             column(width = 9, numericRangeInput(ns("axis.limitz"), "min", "max"))
                             
                    )),
    bsCollapsePanel(optionalDataText("Misc"), 
                    value = "misc",
                    checkboxInput(ns("stabilizeplot"),
                                  helpIconText("Stabilize axes against flips", includeText("helptooltips/pca-pca-plot-stabilize-points.md")),
                                  value = T)),
    bsCollapsePanel(optionalDataText("General settings"), 
                    value = "generalsettings",
                    generalPlotSettingsInput(ns("plot.settings")))
  ))
  
}

plotSamplePlot.save <- function(pca, 
                                pca.full,
                                visuals.conditions, 
                                visuals.sample, 
                                axes, 
                                plot.settings,
                                axislimits,
                                stabilizeplot,
                                format,
                                plot3dprovider,
                                plot3d.theta,
                                plot3d.phi,
                                plot3d.nticks,
                                filename ){
  
  plot.settings <- plotSettingsSetNA(plot.settings, 
                                     PlotSettings(width = default.plot.width, 
                                                  height = default.plot.height,
                                                  dpi = default.plot.dpi,
                                                  scale = 1,
                                                  title = "PCA samples plot",
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
    need(visuals.sample, "No sample visual parameters available!"),
    need(is.integer(plot3d.nticks), "Invalid number of ticks!"))
  validate(need(plot3d.nticks >= 0, "Invalid number of ticks!"))
  
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
  
  # xlim, ylim and zlim calculation based on full PCA
  xaxislimit <- NULL
  yaxislimit <- NULL
  zaxislimit <- NULL
  
  if(axislimits$x$mode == "allgenes") {
    xaxislimit <- c(min(pca.full$transformed[, dimensions.requested[1]]),
                  max(pca.full$transformed[, dimensions.requested[1]]))
  }
  else if(axislimits$x$mode == "manual") {
    xaxislimit <- c(axislimits$x$range$from, axislimits$x$range$to)
  }
  
  if(dimensions.plot > 1) {
    if(axislimits$y$mode == "allgenes") {
      yaxislimit <- c(min(pca.full$transformed[, dimensions.requested[2]]),
                      max(pca.full$transformed[, dimensions.requested[2]]))
    }
    else if(axislimits$y$mode == "manual") {
      yaxislimit <- c(axislimits$y$range$from, axislimits$y$range$to)
    }
  }
  
  if(dimensions.plot > 2) {
    if(axislimits$z$mode == "allgenes") {
      zaxislimit <- c(min(pca.full$transformed[, dimensions.requested[3]]),
                      max(pca.full$transformed[, dimensions.requested[3]]))
    }
    else if(axislimits$z$mode == "manual") {
      zaxislimit <- c(axislimits$z$range$from, axislimits$z$range$to)
    }
  }
  
  
  # As two opposite eigenvectors have the same information it can happen that either a rotation vector
  # or its negative is calculated. This "flips" the affected axis. 
  # We correct this behavior by using the full PCA as reference
  # The correlation between the intersection of the rotation vector of the gene subset and the full PCA is
  # either 1 or -1 (ideal data). If it is negative, we can just negate the affected dimension.
  if(stabilizeplot) {
    for(dim in seq_len(dimensions.plot)) {
      similarity <- cor(pca$pc[, dimensions.requested[dim]], pca.full$pc[rownames(pca$pc), dimensions.requested[dim]])
      if(!is.na(similarity) && similarity < 0) {
        pca.transformed[,dimensions.requested[dim]] <- -pca.transformed[,dimensions.requested[dim]]
      }
    }
  }
  
  # Add visual properties to the variables
  pca.transformed$color <- visuals.sample$factors$color
  pca.transformed$shape <- visuals.sample$factors$shape
  
  palette.colors <- visuals.sample$palette.colors
  palette.shapes <- visuals.sample$palette.shapes
  
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
    p <- p + coord_cartesian(xlim = xaxislimit, ylim = yaxislimit)
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
    
    if(plot3dprovider == "isometric") {
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
          xlim = xaxislimit,
          ylim = yaxislimit,
          zlim = zaxislimit,
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
    else if(plot3dprovider == "perspective") {
      saveRPlot(width, height, dpi, scale, filename, format, expr = function() {
      points3D(x = pca.transformed[[dimensions.requested[1]]],
               y = pca.transformed[[dimensions.requested[2]]],
               z = pca.transformed[[dimensions.requested[3]]],
               xlab = pc.lab(dimensions.requested[1]),
               ylab = pc.lab(dimensions.requested[2]),
               zlab = pc.lab(dimensions.requested[3]),
               xlim = xaxislimit,
               ylim = yaxislimit,
               zlim = zaxislimit,
               pch = palette.shapes[as.numeric(pca.transformed$shape)],
               colvar = as.numeric(pca.transformed$color),
               col = palette.colors,
               theta = plot3d.theta,
               phi = plot3d.phi,
               colkey = F,
               nticks = if(plot3d.nticks < 1) 1 else plot3d.nticks,
               ticktype = if(plot3d.nticks < 1) "simple" else "detailed",
               type = "p",
               main = title,
               sub = subtitle)
        
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
    else {
      stop("Unknown plot3d provider!")
    }
    
  }
  
  return(plot.settings)
  
}

#' Renders a movie showing the PCA sample plot over increasing amount of top variant genes
#'
#' @param filename File name of the rendered video
#' @param animation.params Animation parameters
#' @param axes Vector of displayed axes (PC1, PC2, ...). 
#' @param visuals.conditions Visual definitions for each condition
#' @param visuals.sample Visual definitions for each sample
#' @param readcounts.filtered Filtered read counts. Contains read counts with specific set of genes
#' @param gene.variances Gene variances
#' @param pca.center Center data before PCA
#' @param pca.scale Scale data before PCA
#' @param pca.relative Make transformed sample coordinates relative
#' @param updateProgress Function that takes progress input. Parameters are detail (current operation) and value (current progress)
#'
#' @return
#' @export
#'
#' @examples
plotSamplePlot.saveMovie <- function(filename,
                                 plot.settings,
                                 animation.params,
                                 axes, 
                                 visuals.conditions,
                                 visuals.sample,
                                 readcounts.filtered,
                                 gene.variances,
                                 pca.center,
                                 pca.scale,
                                 pca.relative,
                                 axislimits,
                                 stabilizeplot,
                                 plot3dprovider,
                                 plot3d.theta,
                                 plot3d.phi,
                                 plot3d.nticks,
                                 updateProgress = NULL) {
  
  if(!is.character(filename) ||!is.character(axes) || missing(visuals.conditions) || missing(visuals.sample) ||
     !is.SummarizedExperiment(readcounts.filtered) || !is.data.frame(gene.variances) || !is.logical(pca.center) || !is.logical(pca.scale)) {
    stop("Invalid arguments!")
  }
  
  basefile <- tempfile()
  genecounts <- unique(c(seq(animation.params$from, animation.params$to, animation.params$by), animation.params$to))
  
  pca.full <- applyPCA(readcounts.filtered, center = pca.center, scale = pca.scale, relative = pca.relative)
  
  imagefiles <- c()
  
  for(i in 1:length(genecounts)) {
    
    if(is.function(updateProgress)) {
      updateProgress(value = i / length(genecounts), detail = "Generating plots")  
    }
    
    readcounts.top.variant <- selectTopVariantGeneReadcounts(readcounts.filtered, gene.variances, genecounts[i])
    plot.filename <- paste0(basefile, "_", i, ".png", collapse = "")
    imagefiles <- c(imagefiles, plot.filename)
    
    pca <- applyPCA(readcounts.top.variant, center = pca.center, scale = pca.scale, relative = pca.relative)
    plotSamplePlot.save(pca = pca,
                        pca.full = pca.full,
                        visuals.conditions = visuals.conditions,
                        visuals.sample = visuals.sample,
                        axes = axes,
                        plot.settings = plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste(genecounts[i], "genes"))),
                        axislimits = axislimits,
                        stabilizeplot = stabilizeplot,
                        format = "png",
                        plot3dprovider = plot3dprovider,
                        plot3d.theta = plot3d.theta,
                        plot3d.phi = plot3d.phi,
                        plot3d.nticks = plot3d.nticks,
                        filename = plot.filename)
    
  }
  
  # Convert to mp4
  if(is.function(updateProgress)) {
    updateProgress(detail = "Rendering video file")
  }
  
  spf <- animation.params$delay / 1000 # Seconds per frames
  fps <- 1 / spf
  
  
  # system(paste("bash -lc '",
  #              "ffmpeg",
  #              "-framerate", fps, 
  #              "-i", paste0(basefile, "_%d.png"), 
  #              "-c:v", "libx264",
  #              filename,
  #              "'"))
  system2(ffmpeg.path, c("-framerate", fps, "-i", paste0(basefile, "_%d.png"), "-c:v", "libx264", filename))
  file.remove(imagefiles)
  
  showNotification("Your video file has been successfully rendered.", type = "message")
}

#' Logic of the PCA sample plot
#'
#' @param input 
#' @param output 
#' @param session 
#' @param dataset 
#' @param animation.params 
#' @param pca.center 
#' @param pca.scale 
#' @param pca.relative 
#'
#' @return
#' @export
#'
#' @examples
plotSamplePlot_ <- function(input, 
                          output, 
                          session, 
                          dataset,
                          animation.params,
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
  conditions <- reactive({
    validate(need(dataset(), "No sample conditions available!"))
    validate(need(dataset()$sample.annotation, "No sample conditions available!"))
    validate(need(dataset()$sample.annotation@conditions, "No sample conditions available!"))
    return(dataset()$sample.annotation@conditions)
  })
  
  pca <- serverPCA(pca.center,
              pca.scale,
              pca.relative,
              readcounts.top.variant)
  pca.full <- serverPCA(pca.center,
                        pca.scale,
                        pca.relative,
                        readcounts.filtered)
  
  visuals.conditions <- visualsEditorValue("visuals", reactive({colnames(conditions())}))
  visuals.sample <- reactive({ calculateSampleVisuals(colnames(readcounts.processed()), conditions(), visuals.conditions()) })
  plot.settings <- generalPlotSettings("plot.settings")
  
  # Axis limits selection 
  min.axis.limit <- reactive({ return(min(pca.full()$transformed)) })
  max.axis.limit <- reactive({ return(max(pca.full()$transformed)) })
  
  axislimits.x <- numericRangeInputValue("axis.limitx", min.axis.limit, max.axis.limit)
  axislimits.y <- numericRangeInputValue("axis.limity", min.axis.limit, max.axis.limit)
  axislimits.z <- numericRangeInputValue("axis.limitz", min.axis.limit, max.axis.limit)
 
  axislimits <- reactive({
    return(list("x" = list("mode" = input$axis.limitx.mode, "range" = axislimits.x()),
           "y" = list("mode" = input$axis.limity.mode, "range" = axislimits.y()),
           "z" = list("mode" = input$axis.limitz.mode, "range" = axislimits.z())))
  })
  
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
             need(visuals.sample(), "No visual parameters!"))
    
    plot.settings <- plotSettingsSetNA(plot.settings, PlotSettings(subtitle = paste(nrow(readcounts.top.variant()), "genes")))
    
    return(plotSamplePlot.save(pca = pca(),
                               pca.full = pca.full(),
                              visuals.conditions = visuals.conditions(),
                              visuals.sample = visuals.sample(),
                              axes = input$axes,
                              plot.settings = plot.settings,
                              axislimits = axislimits(),
                              stabilizeplot = input$stabilizeplot,
                              format = format,
                              plot3dprovider = input$plot3dprovider,
                              plot3d.phi = input$plot3d.phi,
                              plot3d.theta = input$plot3d.theta,
                              plot3d.nticks = input$plot3d.nticks,
                              filename = filename))
  })
  
  observeEvent(input$export.mp4, {
    
    validate(need(readcounts.filtered(), "No filtered read counts!"))
    shinyjs::disable("export.mp4")
    
    movie.file <- tempfile(pattern = "samples-plot-mp4", fileext = ".mp4")
    progress <- shiny::Progress$new()
    progress$set(message = "Creating movie ...", value = 0)
    
    # Status callback function
    updateProgress <- function(detail = NULL, value = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    parallel.expr <- function() {
      plotSamplePlot.saveMovie(
        filename = movie.file,
        animation.params = animation.params(),
        axes = input$axes,
        plot.settings = plot.settings(),
        axislimits = axislimits(),
        stabilizeplot = input$stabilizeplot,
        visuals.conditions = visuals.conditions(),
        visuals.sample = visuals.sample(),
        readcounts.filtered = readcounts.filtered(),
        gene.variances = gene.variances(),
        pca.center = pca.center(),
        pca.scale = pca.scale(),
        pca.relative = pca.relative(),
        plot3dprovider = input$plot3dprovider,
        plot3d.phi = input$plot3d.phi,
        plot3d.theta = input$plot3d.theta,
        plot3d.nticks = input$plot3d.nticks,
        updateProgress = updateProgress
      )
      return("done")
    }
    
    withParallel(session, input, expr = parallel.expr(),
    exprsuccess = function(result) {
      showModal(modalDialog(
            "The movie is ready for download!",
            footer = tagList(
              modalButton("Close"),
              downloadButton(session$ns("export.mp4.download"), "Download now")
            )
          ))
      output$export.mp4.download <- downloadHandler("pcago-pca-movie.mp4",
                                                    content = function(filename) {
                                                      file.copy(movie.file, filename, overwrite = T)
                                                    },
                                                    contentType = "video/mp4")
    },
    exprfinally = function() {
      progress$close()
      shinyjs::enable("export.mp4")
    })
    
  })
  
  
  # # It seems that for long processes, downloadHandler can time out for some reason
  # # Use a modal approach
  # observeEvent(input$export.mp4, {
  #   
  #   validate(need(readcounts.filtered(), "No filtered read counts!"))
  #   
  #   shinyjs::disable("export.mp4")
  #   on.exit({
  #     shinyjs::enable("export.mp4")
  #   })
  #   
  #   movie.file <- tempfile(pattern = "samples-plot-mp4", fileext = ".mp4")
  #   
  #   withProgressCustom(function(updateProgress) {
  # 
  #         plotSamplePlot.saveMovie(
  #           filename = movie.file,
  #           animation.params = animation.params(),
  #           axes = input$axes,
  #           plot.settings = plot.settings(),
  #           axislimits = axislimits(),
  #           stabilizeplot = input$stabilizeplot,
  #           visuals.conditions = visuals.conditions(),
  #           visuals.sample = visuals.sample(),
  #           readcounts.filtered = readcounts.filtered(),
  #           gene.variances = gene.variances(),
  #           pca.center = pca.center(),
  #           pca.scale = pca.scale(),
  #           pca.relative = pca.relative(),
  #           plot3dprovider = input$plot3dprovider,
  #           plot3d.phi = input$plot3d.phi,
  #           plot3d.theta = input$plot3d.theta,
  #           plot3d.nticks = input$plot3d.nticks,
  #           updateProgress = updateProgress
  #         )
  # 
  #       }, message = "Creating movie")
  #   
  #   # Use a modal to download the plot
  #   showModal(modalDialog(
  #     "The movie is ready for download!",
  #     footer = tagList(
  #       modalButton("Close"),
  #       downloadButton(session$ns("export.mp4.download"), "Download now")
  #     )
  #   ))
  #   
  #   output$export.mp4.download <- downloadHandler("pcago-pca-movie.mp4",
  #                                                   content = function(filename) {
  #                                                     file.copy(movie.file, filename, overwrite = T)
  #                                                   },
  #                                                   contentType = "video/mp4")
  #   
  # })
  
  # xauto exporter that allows triggering of exporting data from code
  xautovars <- reactiveValues(xautocounter = 1)
  
  if(!is.null(xauto)) {
    observeEvent(xauto(), {
      
      filename <- xauto()$filename
      format <- xauto()$format
      
      validate(need(pca(), "No PCA results to plot!"),
               need(visuals.sample(), "No visual parameters!"))
      
      plot.settings <- plotSettingsSetNA(plot.settings(), PlotSettings(subtitle = paste(nrow(readcounts.top.variant()), "genes")))
      
      return(plotSamplePlot.save(pca = pca(),
                                 pca.full = pca.full(),
                                 visuals.conditions = visuals.conditions(),
                                 visuals.sample = visuals.sample(),
                                 axes = input$axes,
                                 plot.settings = plot.settings,
                                 axislimits = axislimits(),
                                 stabilizeplot = input$stabilizeplot,
                                 format = format,
                                 plot3dprovider = input$plot3dprovider,
                                 plot3d.phi = input$plot3d.phi,
                                 plot3d.theta = input$plot3d.theta,
                                 plot3d.nticks = input$plot3d.nticks,
                                 filename = filename))
      
      xautovars$xautocounter <- xautovars$xautocounter + 1
      
    })
  }
  
  return(reactive({ xautovars$xautocounter }))
}

plotSamplePlot <- function(id, 
                         dataset,
                         animation.params,
                         pca.center,
                         pca.scale,
                         pca.relative,
                         xauto = NULL) {
  
  return(callModule(plotSamplePlot_, 
                    id, 
                    dataset = dataset,
                    animation.params = animation.params,
                    pca.center = pca.center,
                    pca.scale = pca.scale,
                    pca.relative = pca.relative,
                    xauto = xauto))
  
}