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

#' Does preprocessing steps
#'
#' @param readcounts 
#' @param gene.info.annotation Gene annotation object
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadCountPreProcessing <- function(readcounts, input) {
  
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
    
    return(output)
    
  }))
  
}

#' Applies read count normalization
#'
#' @param readcounts 
#' @param gene.info.annotation 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadcountNormalization <- function(readcounts, conditions, gene.info.annotation, input) {
  
  return(reactive({
    
    validate(need(readcounts(), "No readcounts to process!"))
    
    readcounts <- readcounts()
    
    # Apply normalization
    if(input$pca.data.normalization == "tpm") {
      
      validate(need(gene.info.annotation(), "No annotation available!"),
               need(input$pca.data.normalization.tpm.mufld, "No mean sequence length set!"),
               need(is.integer(assay(readcounts)), "The read counts must be integers to be able to normalize them!"))
      
      return(applyReadcountNormalization.TPM(readcounts, 
                                            input$pca.data.normalization.tpm.mufld,
                                            gene.info.annotation()@sequence.info))
    }
    else if(input$pca.data.normalization == "deseq2") {
      
      validate(need(conditions(), "No condition assignments available!"),
               need(input$pca.data.normalization.deseq2.conditions, "No conditions selected!"),
               need(length(setdiff(input$pca.data.normalization.deseq2.conditions ,colnames(conditions()))) == 0, "Wrong conditions selected!"),
               need(is.integer(assay(readcounts)), "The read counts must be integers to be able to normalize them!"))
      
      return(applyReadcountNormalization.DESeq2(readcounts, 
                                               conditions(), 
                                               input$pca.data.normalization.deseq2.conditions))
      
    }
    else 
    {
      return(list(readcounts = readcounts))
    }
    
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
                                          feature.genes <- intersect(geneFilterGenes(output@gene.biotype), genes)
                                          go.genes <- intersect(geneFilterGenes(output@gene.go.terms), genes)
                                          
                                          names(choices) <- c(
                                            if(length(sequence.info.genes) == 0) "Sequence info" else sprintf("Sequence info (%d/%d)", length(sequence.info.genes), length(genes)),
                                            if(length(feature.genes) == 0) "Biotype" else sprintf("Biotype (%d/%d)", length(feature.genes), length(genes)),
                                            if(length(go.genes) == 0) "GO terms" else sprintf("GO terms (%d/%d)", length(go.genes), length(genes))
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
    
    validate(need(readcounts.processed(), "No readcounts to process!"))
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    all.genes <- rownames(readcounts.processed())
    
    if(!is.null(gene.info.annotation())) {
      
      annotation <- gene.info.annotation()
      annotation <- annotationRestrictToGenes(annotation, all.genes) # The annotation is for the complete set of genes. But we want to filter processed readcounts
      
      {
        unused.genes <- setdiff(all.genes, geneFilterGenes(annotation@gene.biotype))
        gene.criteria[["Biotype"]] <- annotation@gene.biotype@data
        
        if(length(unused.genes) > 0) {
          gene.criteria[["Biotype"]][["No data"]] <- unused.genes
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
    
    validate(
      need(genes.filtered(), "No genes selected!"),
      need(length(genes.filtered()$values) > 0, "No genes selected!"))
    
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
    
    return(applyPCA(readcounts.top.variant(), 
                    center = center, 
                    scale = scale, 
                    relative = relative))
    
  }))
  
}

#' Builds the data of the gene variance table.
#'
#' @param gene.variances 
#'
#' @return
#' @export
#'
#' @examples
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

#' Builds the data of the gene annotation table.
#' This is needed, as annotations are stored to allow quick filtering and not for display
#'
#' @param readcounts 
#' @param gene.info.annotation 
#'
#' @return
#' @export
#'
#' @examples
serverGeneAnnotationTableData <- function(readcounts, gene.info.annotation) {
  
  return(reactive({
    validate(need(gene.info.annotation(), "No annotation available!"))
    
    notification.id <- progressNotification("Building table data ...")
    on.exit({
      removeNotification(notification.id)
    })
    
    return(annotationToTable(gene.info.annotation()))
    
  }))
}