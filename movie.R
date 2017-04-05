#'
#' Methods for creating animated plots through videos
#' 

library(shiny)

#' Renders a movie showing the PCA cell plot over increasing amount of top variant genes
#'
#' @param filename File name of the rendered video
#' @param width Plot width in pixels
#' @param height Plot height in pixels
#' @param dpi Plot DPI
#' @param animation.params Animation parameters
#' @param axes Vector of displayed axes (PC1, PC2, ...). 
#' @param customlabel.color Custom label for color
#' @param customlabel.shape Custom label for shape
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
savePCACellPlotMovie <- function(filename,
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
     !is.data.frame(readcounts.filtered) || !is.data.frame(gene.variances) || !is.logical(pca.center) || !is.logical(pca.scale)) {
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
    savePCACellPlot(pca = pca,
                visuals.conditions = visuals.conditions,
                visuals.cell = visuals.cell,
                axes = axes,
                plot.settings = setNA(plot.settings, PlotSettings(subtitle = paste(genecounts[i], "genes"))),
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