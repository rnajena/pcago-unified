library(shiny)

source("readcounts.R")
source("annotation.R")
source("conditions.R")
source("pca.R")
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
#' @param gene.info.annotation Gene annotation object
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadCountProcessing <- function(readcounts, gene.info.annotation, input) {
  
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
    if(input$pca.data.normalization == "tpm") {
      
      validate(need(gene.info.annotation(), "No annotation available!"),
               need(input$pca.data.normalization.tpm.mufld, "No mean sequence length set!"))
      
      output$readcounts <- applyReadcountNormalization.TPM(readcounts(), 
                                                    input$pca.data.normalization.tpm.mufld,
                                                    gene.info.annotation()@sequence.info)
    }
    
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
  
  return(integratingGenericImporterData("pca.data.annotation.importer", 
                                        importers = reactive(supportedAnnotationImporters),
                                        samples = reactive(availableAnnotationSamples),
                                        generators = reactive(supportedAnnotationGenerators),
                                        exprimport = function(con, importer, parameters) {
                                          return(importGeneInformationFromAnnotation(con, importer, readcounts()))
                                        },
                                        exprsample = function(sample, parameters) {
                                          return(importSampleGeneInformation(sample, readcounts()))
                                        },
                                        exprgenerator = function(generator, parameters) {
                                          return(generateGeneInformation(generator, parameters, readcounts()))
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
    
    if(!is.null(gene.info.annotation())) {
      
      annotation <- gene.info.annotation()
      annotation <- annotationRestrictToGenes(annotation, all.genes) # The annotation is for the complete set of genes. But we want to filter processed readcounts
      
      {
        unused.genes <- setdiff(all.genes, geneFilterGenes(annotation@gene.features))
        gene.criteria[["Associated features"]] <- annotation@gene.features@data
        
        if(length(unused.genes) > 0) {
          gene.criteria[["Associated features"]][["No data"]] <- unused.genes
        }
      }
      {
        unused.genes <- setdiff(all.genes, geneFilterGenes(annotation@gene.go.terms))
        gene.criteria[["Associated GO terms"]] <- annotation@gene.go.terms@data
        
        if(length(unused.genes) > 0) {
          gene.criteria[["Associated GO terms"]][["No data"]] <- unused.genes
        }
        
      }
      {
        unused.genes <- setdiff(all.genes, geneFilterGenes(annotation@gene.scaffold))
        gene.criteria[["Scaffold"]] <- annotation@gene.scaffold@data
        
        if(length(unused.genes) > 0) {
          gene.criteria[["Scaffold"]][["No data"]] <- unused.genes
        }
       
      }
      
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
    keep.genes <- intersect(keep.genes, genes.filtered()$values)
    
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
    relative <- input$pca.pca.settings.relative
    
    validate(
      need(readcounts.top.variant(), "No data to apply PCA to!"),
      need(!scale || no.constant, "Constant read count genes must be removed for scaling!")
    )
    
    applyPCA(readcounts.top.variant(), center = center, scale = scale, relative = relative) 
    
  }))
  
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
    
    notification.id <- progressNotification("Building table data ...")
    on.exit({
      removeNotification(notification.id)
    })
    
    genes <- rownames(readcounts())
    table <- data.frame(row.names = genes,
                        "Scaffold" = rep(NA, length(genes)),
                        "Start" = rep(NA, length(genes)),
                        "End" = rep(NA, length(genes)),
                        "Length" = rep(NA, length(genes)),
                        "Features" = rep(NA, length(genes)))
    
    sequence.info <- gene.info.annotation()@sequence.info
    features <- gene.info.annotation()@gene.features
    features.inv <- invertGeneFilter(features)
    go.terms <- gene.info.annotation()@gene.go.terms
    go.terms.inv <- invertGeneFilter(go.terms)
    
    if(nrow(sequence.info) > 0) {
      indices <- match(genes, rownames(sequence.info))
      
      table$Sequence <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "scaffold"] })
      table$Start <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "start_position"] })
      table$End <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "end_position"] })
      table$Length <- sapply(indices, function(i) { if(is.na(i)) NA else sequence.info[i, "length"] })
    }
    
    table$Features <- sapply(genes, function(x) { if(x %in% names(features.inv)) paste(features.inv[[x]], collapse = "; ") else NA })
    table[["GO terms"]] <- sapply(genes, function(x) { if(x %in% names(go.terms.inv)) paste(go.terms.inv[[x]], collapse = "; ") else NA })
    
    
    return(table)
    
  }))
}