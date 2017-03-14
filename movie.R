#'
#' Methods for creating animated plots through videos
#' 

library(shiny)

#' Renders a movie showing the PCA cell plot over increasing amount of top variant genes
#'
#' @param filename File name of the rendered video
#' @param genes.count.from From which top variant gene count the animation should start
#' @param genes.count.to To which top variant gene count the animation should play
#' @param genes.count.by Step size to increase gene count
#' @param axes Vector of displayed axes (PC1, PC2, ...). 
#' @param visuals.cell Visual definitions for each cell
#' @param readcounts.filtered Filtered read counts. Contains read counts with specific set of genes
#' @param annotation Gene annotation
#' @param pca.center Center data before PCA
#' @param pca.scale Scale data before PCA
#' @param updateProgress Function that takes progress input. Parameters are detail (current operation) and value (current progress)
#'
#' @return
#' @export
#'
#' @examples
pcaCellPlotMovie <- function(filename,
                             genes.count.from, 
                             genes.count.to, 
                             genes.count.by, 
                             time.per.frame,
                             axes, 
                             visuals.cell,
                             readcounts.filtered,
                             annotation,
                             pca.center,
                             pca.scale,
                             updateProgress = NULL) {
  
  basefile <- tempfile()
  genecounts <- unique(c(seq(genes.count.from, genes.count.to, genes.count.by), genes.count.to))
  
  for(i in 1:length(genecounts)) {
    
    if(is.function(updateProgress)) {
      updateProgress(value = i / length(genecounts), detail = "Rendering plots")  
    }
    
    readcounts.selected <- selectTopVariantGenes(readcounts.filtered, annotation, genecounts[i])
    plot.filename <- paste0(basefile, "_", i, ".png", collapse = "")
    
    pca <- applyPCA(readcounts.selected, center = pca.center, scale = pca.scale)
    pcaCellPlot(pca,
                visuals.cell,
                axes,
                640,
                480,
                96,
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