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
#' @param genes.count.from From which top variant gene count the animation should start
#' @param genes.count.to To which top variant gene count the animation should play
#' @param genes.count.by Step size to increase gene count
#' @param axes Vector of displayed axes (PC1, PC2, ...). 
#' @param customlabel.color Custom label for color
#' @param customlabel.shape Custom label for shape
#' @param visuals.conditions Visual definitions for each condition
#' @param visuals.cell Visual definitions for each cell
#' @param readcounts.filtered Filtered read counts. Contains read counts with specific set of genes
#' @param gene.variances Gene variances
#' @param pca.center Center data before PCA
#' @param pca.scale Scale data before PCA
#' @param updateProgress Function that takes progress input. Parameters are detail (current operation) and value (current progress)
#'
#' @return
#' @export
#'
#' @examples
savePCACellPlotMovie <- function(filename,
                             width,
                             height,
                             dpi,
                             genes.count.from, 
                             genes.count.to, 
                             genes.count.by, 
                             time.per.frame,
                             axes, 
                             customlabel.color,
                             customlabel.shape,
                             visuals.conditions,
                             visuals.cell,
                             readcounts.filtered,
                             gene.variances,
                             pca.center,
                             pca.scale,
                             updateProgress = NULL) {
  
  if(!is.character(filename) || !is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) ||
     !is.numeric(genes.count.from) || !is.numeric(genes.count.to) || !is.numeric(genes.count.by) ||
     !is.numeric(time.per.frame) || !is.character(axes) || missing(visuals.conditions) || missing(visuals.cell) ||
     !is.character(customlabel.color) || !is.character(customlabel.shape) ||
     !is.data.frame(readcounts.filtered) || !is.data.frame(gene.variances) || !is.logical(pca.center) || !is.logical(pca.scale)) {
    stop("Invalid arguments!")
  }
  
  basefile <- tempfile()
  genecounts <- unique(c(seq(genes.count.from, genes.count.to, genes.count.by), genes.count.to))
  
  for(i in 1:length(genecounts)) {
    
    if(is.function(updateProgress)) {
      updateProgress(value = i / length(genecounts), detail = "Rendering plots")  
    }
    
    readcounts.top.variant <- selectTopVariantGeneReadcounts(readcounts.filtered, gene.variances, genecounts[i])
    plot.filename <- paste0(basefile, "_", i, ".png", collapse = "")
    
    pca <- applyPCA(readcounts.top.variant, center = pca.center, scale = pca.scale)
    savePCACellPlot(pca,
                visuals.conditions,
                visuals.cell,
                axes,
                customlabel.color,
                customlabel.shape,
                width,
                height,
                dpi,
                "png",
                plot.filename,
                subtitle = paste(genecounts[i], "genes"))
    
  }
  
  # Convert to mp4
  if(is.function(updateProgress)) {
    updateProgress(detail = "Rendering video file")
  }
  
  spf <- time.per.frame / 1000 # Seconds per frames
  fps <- 1 / spf
  
  
  system(paste("ffmpeg",
               "-framerate", fps, 
               "-i", paste0(basefile, "_%d.png"), 
               "-c:v", "libx264",
               filename))
  
  showNotification("Your video file has been successfully rendered.", type = "message")
}