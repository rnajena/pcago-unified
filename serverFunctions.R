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
  
  #' Start page button. User can click it to go to the "Help" section
  observeEvent(input$about.goto.help, {
    updateNavbarPage(session, "main.nav", "help")
  })
  
  # Navigation quick links
  # Offer quick links in the navigation as compromise between hierarchical layout and discoverability
  observeEvent(input$pca.nav, {
    if(input$pca.nav == "pca.samples.plot.quicklink") {
      updateNavbarPage(session, "pca.nav", selected = "pca.samples.plot")
    }
  })
}

#' Automatically navigates to content pages depending on sidebar
#'
#' @param input 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
serverAutoNavigation <- function(input, session) {
  observeEvent(input$sidebardata, {
    cid <- getOpenCollapseId(input, "sidebardata")
    
    if(is.null(cid)) {
      
    }
    else if(cid == "data.readcounts.import") {
      updateNavbarPage(session, "pca.nav", selected = "readcounts.raw")
    }
    else if(cid == "data.sample.annotation") {
      updateNavbarPage(session, "pca.nav", selected = "samples.conditions")
    }
    else if(cid == "data.gene.annotation") {
      updateNavbarPage(session, "pca.nav", selected = "genes.annotation")
    }
    else if(cid == "data.readcounts.processing") {
      updateNavbarPage(session, "pca.nav", selected = "readcounts.processed")
    }
    
  })
}

#' Automatically navigates to a content navigation based on which data is refereshed
#'
#' @param observed 
#' @param target.nav 
#'
#' @return
#' @export
#'
#' @examples
serverReactiveNavigation <- function(session, observed, target.nav) {
  observeEvent(observed(), {
    updateNavbarPage(session, "pca.nav", selected = target.nav)
  })
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
    {
      unused.genes <- setdiff(all.genes, geneFilterGenes(annotation@gene.custom))
      gene.criteria[["Custom"]] <- annotation@gene.custom@data
      
      if(length(unused.genes) > 0) {
        gene.criteria[["Custom"]][["No data"]] <- unused.genes
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