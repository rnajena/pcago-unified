library(shiny)

source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("movie.R")

#' Handles some server side navigation features
#'
#' @param input 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
serverNavigation <- function(input, session) {
  
  #' Start page button. User can click it to go to the "Analyze" section
  observeEvent(input$about.goto.analyze, {
    updateNavbarPage(session, "main.nav", "analyze")
  })
  
  # Navigation quick links
  # Offer quick links in the navigation as compromise between hierarchical layout and discoverability
  observeEvent(input$pca.nav, {
    if(input$pca.nav == "pca.cells.plot.quicklink") {
      updateNavbarPage(session, "pca.nav", selected = "pca.cells.plot")
    }
  })
  
}

#' Server function for processed read counts
#'
#' @param readcounts 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadCountProcessing <- function(readcounts, input) {
  
  return(reactive({
    
    validate(need(readcounts(), "No read counts to process!"))
    
    output <- list( removed.genes = c(), readcounts = readcounts() )
    
    # Transpose read counts
    if("transpose" %in% input$pca.data.readcounts.processing) {
      output$readcounts <- transposeReadCounts(output$readcounts)
    }
    
    # Prevent too many cells
    if(ncol(output$readcounts) > 100) {
      showNotification("There are over 100 cells! I won't calculate with that! Maybe you need to transpose your data?", type = "error")
      return(NULL)
    }
    
    # Remove constant read genes
    if("remove.constant" %in% input$pca.data.readcounts.processing) {
      processed <- removeConstantReads(output$readcounts)
      output$readcounts <- processed$readcounts
      output$removed.genes <- processed$genes.removed
    }
    
    # Apply normalization
    output$readcounts <- applyReadcountNormalization(output$readcounts, input$pca.data.normalization)
    
    return(output)
    
  }))
  
}

#' Server function for PCA
#'
#' @param input 
#' @param readcounts.top.variant
#'
#' @return
#' @export
#'
#' @examples
serverPCA <- function(input, readcounts.top.variant) {
  
  return(reactive( {
    
    no.constant <- "remove.constant" %in% input$pca.data.readcounts.processing
    center <-input$pca.pca.settings.center
    scale <- input$pca.pca.settings.scale
    
    validate(
      need(readcounts.top.variant(), "No data to apply PCA to!"),
      need(!scale || no.constant, "Constant read count genes must be removed for scaling!")
    )
    
    applyPCA(readcounts.top.variant(), center = center, scale = scale) 
    
  }))
  
}

#' Builds cell visual table that finds the conditions applying to a cell and generates the
#' visual parameters for it
#'
#' @param input 
#' @param readcounts.processed 
#' @param conditions 
#' @param visuals.conditions 
#'
#' @return
#' @export
#'
#' @examples
serverGetCellVisualsTable <- function(input, readcounts.processed, conditions, visuals.conditions) {
  
  validate(
    need(readcounts.processed(), "No data to build visual parameter table from!"),
    need(conditions(), "No conditions for visual mapping!"),
    need(visuals.conditions(), "No condition visual mapping!")
  )
  
  cells <- colnames(readcounts.processed())
  cells.conditions <- conditions()
  visuals.conditions <- visuals.conditions()
  
  # Setup output
  factors <- data.frame(row.names = cells,
                        color = rep("#000000", length(cells)),
                        shape = rep(16, length(cells)),
                        stringsAsFactors = F)
  
  palette.colors <- c()
  palette.colors.conditions <- c()
  palette.shapes <- c()
  palette.shapes.conditions <- c()
  
  # Go through each cell and select the color & shape based on the first condition providing it
  for(cell in cells) {
    
    color <- ""
    color.condition <- ""
    shape <- -1
    shape.condition <- ""
    
    for(condition in rownames(visuals.conditions)) {
      
      if(!cells.conditions[cell, condition]) {
        next()
      }
      
      if(color == "") {
        mapping.color <- visuals.conditions[condition, "color"]
        
        color <- mapping.color
        color.condition <- condition
      }
      
      if(shape == -1) {
        shape <- visuals.conditions[condition, "shape"]
        shape.condition <- condition
      }
      
    }
    
    if(color == "") {
      color = "#000000"
      color.condition <- condition.default
    }
    if(shape == -1) {
      shape = 16
      shape.condition <- condition.default
    }
    
    factors[cell, "color"] <- color.condition # todo: user name for condition
    factors[cell, "shape"] <- shape.condition
    
    if(!(color.condition %in% palette.colors.conditions)) { 
      
      palette.colors <- c(palette.colors, color) 
      palette.colors.conditions <- c(palette.colors.conditions, color.condition)
    }
    if(!(shape.condition %in% palette.shapes.conditions)) {
      
      palette.shapes <- c(palette.shapes, shape) 
      palette.shapes.conditions <- c(palette.shapes.conditions, shape.condition)
    }
    
  }
  
  #Convert to factors
  factors$color <- factor(factors$color, levels = palette.colors.conditions)
  factors$shape <- factor(factors$shape, levels = palette.shapes.conditions)
  
  return(list("factors" = factors, 
              "palette.colors" = palette.colors, 
              "palette.shapes" = palette.shapes))
  
}

#' Summary of read count processing.
#' Use as expression inside renderUI
#'
#' @return
#' @export
#'
#' @examples
serverReadCountsProcessingOutput <- function(input, readcounts.processed, readcounts.processing.output) {
  
  validate(need(readcounts.processed(), "No processed read counts available."),
           need(readcounts.processing.output(), "No read count processing info available!"))
  
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
      
      genes <- paste0(removed.genes, collapse = ", ")
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