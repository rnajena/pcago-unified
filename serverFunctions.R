library(shiny)

source("readcounts.R")
source("geneAnnotation.R")
source("sampleAnnotation.R")
source("sampleAnnotationVisuals.R")
source("pca.R")
source("widgetSampleAnnotationImporter.R")

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
    if(input$pca.nav == "pca.samples.plot.quicklink") {
      updateNavbarPage(session, "pca.nav", selected = "pca.samples.plot")
    }
  })
}

#' Does preprocessing steps
#'
#' @param readcounts 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadCountPreProcessing <- function(readcounts, input) {
  
  return(reactive({
    
    validate(need(readcounts(), "[Read count processing] No read counts to process!"))
    
    output <- list( removed.genes = c(), readcounts = readcounts() )
    
    # Transpose read counts
    if("transpose" %in% input$pca.data.readcounts.processing) {
      output$readcounts <- transposeReadCounts(output$readcounts)
    }
    
    # Prevent too many samples
    if(ncol(output$readcounts) > 100) {
      showNotification("There are over 100 samples! I won't calculate with that! Maybe you need to transpose your data?", type = "error")
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
#' @param gene.annotation 
#' @param sample.annotation
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverReadcountNormalization <- function(readcounts, gene.annotation, sample.annotation, input) {
  
  return(reactive({
    
    validate(need(readcounts(), "[Read count processing] No readcounts to process!"))
    
    # Apply normalization
    if(input$pca.data.normalization == "tpm") {
      
      validate(need(sample.annotation(), "[Read count processing] No sample annotation available!"),
              need(gene.annotation(), "[Read count processing] No gene annotation available!"))
      
      return(applyReadcountNormalization.TPM(readcounts = readcounts(), 
                                             use.feature.exonlength = input$pca.data.normalization.tpm.exonlength,
                                             use.fragment.effectivelength = input$pca.data.normalization.tpm.effectivelength,
                                             gene.annotation = gene.annotation(),
                                             sample.annotation = sample.annotation()))
    }
    else if(input$pca.data.normalization == "deseq2") {
      
      validate(need(sample.annotation(), "No sample annotation available!"),
               need(input$pca.data.normalization.deseq2.conditions, "No conditions selected!"))
      
      return(applyReadcountNormalization.DESeq2(readcounts = readcounts(), 
                                                sample.annotation = sample.annotation(), 
                                                selected.conditions = input$pca.data.normalization.deseq2.conditions))
      
    }
    else 
    {
      return(list(readcounts = readcounts()))
    }
    
  }))
  
  
}

#' Lets the user choose a set of genes based on the features
#'
#' @param readcounts.processed 
#' @param gene.annotation 
#'
#' @return
#' @export
#'
#' @examples
serverFilteredGenes <- function(readcounts.processed, gene.annotation) {
  
  return(filterSelectionValues("pca.pca.genes.set",  reactive({
    
    validate(need(readcounts.processed(), "[Gene filtering] No readcounts to process!"),
             need(gene.annotation(), "[Gene filtering] No gene annotation available!"))
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    all.genes <- rownames(readcounts.processed())
    
    annotation <- gene.annotation()
    annotation <- geneAnnotationRestrictToGenes(annotation, all.genes) # The annotation is for the complete set of genes. But we want to filter processed readcounts
    
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
      need(genes.filtered(), "[Gene filtering] No genes selected!"),
      need(length(genes.filtered()$values) > 0, "[Gene filtering] No genes selected!"))
    
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
      need(readcounts.top.variant(), "[PCA] No data to apply PCA to!"),
      need(!scale || no.constant, "[PCA] Constant read count genes must be removed for scaling!")
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
    validate(need(gene.variances(), "No gene variances available!"))
    
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
#' @param gene.annotation 
#'
#' @return
#' @export
#'
#' @examples
serverGeneAnnotationTableData <- function(readcounts, gene.annotation) {
  
  return(reactive({
    validate(need(gene.annotation(), "No gene annotation available!"))
    
    notification.id <- progressNotification("Building table data ...")
    on.exit({
      removeNotification(notification.id)
    })
    
    return(geneAnnotationToTable(gene.annotation()))
    
  }))
}