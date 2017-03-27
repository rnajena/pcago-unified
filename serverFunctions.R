library(shiny)

source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
source("plots.R")
source("movie.R")
source("widgetCellConditionImporter.R")

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

#' Provides the gene annotation
#'
#' @param readcounts 
#'
#' @return
#' @export
#'
#' @examples
serverGeneInfoAnnotation <- function(readcounts) {
  
  return(integratingGenericImporterData("pca.data.annotation.importer", exprimport = function(con, importer) {
    return(importGeneInformationFromAnnotation(con, importer, readcounts()))
  },
  exprsample = function(sample) {
    return(importSampleGeneInformationFromAnnotation(sample, readcounts()))
  },
  exprintegrate = function(data, callback) {
    output <- Annotation()
    genes <- rownames(readcounts())
    
    choices = c("sequence.info",
                "features", 
                "go")
    selected = c()
    
    # Merge data into output
    for(current.data in data) {
      if(!is.null(data)) {
        output <- mergeAnnotation(output, current.data)
      }
    }
    
    sequence.info.genes <- intersect(rownames(output@sequence.info), genes)
    feature.genes <- intersect(geneFilterGenes(output@gene.features), genes)
    go.genes <- intersect(geneFilterGenes(output@gene.go.terms), genes)
    
    names(choices) <- c(
      if(length(sequence.info.genes) == 0) "Sequence info" else paste0("Sequence info (", length(sequence.info.genes), "/", length(genes), ")"),
      if(length(feature.genes) == 0) "Associated features" else paste0("Associated features (", length(feature.genes), "/", length(genes), ")"),
      if(length(go.genes) == 0) "GO terms" else paste0("GO terms (", length(go.genes), "/", length(genes), ")")
    )
    
    if(length(sequence.info.genes) > 0) { selected <- c(selected, "sequence.info") }
    if(length(feature.genes) > 0) { selected <- c(selected, "features") }
    if(length(go.genes) > 0) { selected <- c(selected, "go") }
    
    callback(choices, selected)
    return(output)
  }))
  
}

#' Lets the user choose a set of genes based on the features
#'
#' @param readcounts.processed 
#' @param gene.info.annotation 
#'
#' @return
#' @export
#'
#' @examples
serverFilteredGenes <- function(readcounts.processed, gene.info.annotation) {
  
  return(filterSelectionValues("pca.pca.genes.set",  reactive({
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    all.genes <- rownames(readcounts.processed())
    unused.genes <- rownames(readcounts.processed())
    
    if(!is.null(gene.info.annotation())) {
      
      annotation <- gene.info.annotation()
      annotation <- annotationRestrictToGenes(annotation, all.genes) # The annotation is for the complete set of genes. But we want to filter processed readcounts
      
      if(length(geneFilterKeys(annotation@gene.features)) > 0) {
        gene.criteria[["Associated features"]] <- annotation@gene.features@data
        unused.genes <- setdiff(unused.genes, geneFilterGenes(annotation@gene.features))
      }
      if(length(geneFilterKeys(annotation@gene.go.terms)) > 0) {
        gene.criteria[["Associated GO terms"]] <- annotation@gene.go.terms@data
        unused.genes <- setdiff(unused.genes, geneFilterGenes(annotation@gene.go.terms))
      }
      
    }
    
    # Now look for genes that haven't been covered and create a list
    if(length(unused.genes) > 0) {
      gene.criteria[["Misc"]] <- list("Without criteria" = unused.genes)
    }
    
    return(gene.criteria)
    
  })))
}

#' Filters the read count table by only returning the rows that are in the list of genes.
#'
#' @param genes.filtered 
#' @param readcounts.processed 
#'
#' @return
#' @export
#'
#' @examples
serverFilterReadcounts <- function(genes.filtered, readcounts.processed) {
  return(reactive({
    
    keep.genes <- rownames(readcounts.processed())
    keep.genes <- intersect(keep.genes, genes.filtered())
    
    keep.readcounts <- readcounts.processed()[keep.genes,]
    
    return(keep.readcounts)
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

serverGeneVarianceTableData <- function(gene.variances) {
  return(reactive({
    validate(need(gene.variances(), "No annotation available!"))
    
    table <- data.frame(row.names = rownames(gene.variances()), 
                        Variance = gene.variances()$var,
                        "Relative variance" = gene.variances()$var / sum(gene.variances()$var))
    table <- table[order(table$Variance, decreasing = T), ,drop = F]
    
    return(table)
    
  }))
}

serverGeneAnnotationTableData <- function(readcounts, gene.info.annotation) {
  return(reactive({
    validate(need(gene.info.annotation(), "No annotation available!"))
    
    genes <- rownames(readcounts())
    table <- data.frame(row.names = genes,
                        "Sequence" = rep(NA, length(genes)),
                        "Start" = rep(NA, length(genes)),
                        "End" = rep(NA, length(genes)),
                        "Length" = rep(NA, length(genes)),
                        "Features" = rep(NA, length(genes)))
    
    sequence.info <- gene.info.annotation()@sequence.info
    features <- gene.info.annotation()@gene.features
    features.inv <- invertGeneFilter(features)
    go.terms <- gene.info.annotation()@gene.go.terms
    
    if(nrow(sequence.info) > 0) {
      indices <- match(genes, rownames(sequence.info))
      
      table$Sequence <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "sequence"] })
      table$Start <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "start"] })
      table$End <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "end"] })
      table$Length <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "length"] })
    }
    
    table$Features <- sapply(genes, function(x) { if(x %in% names(features.inv)) paste(features.inv[[x]], collapse = "; ") else NA })
    
    
    return(table)
    
  }))
}