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

#' Fills PCAGO with some example data
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
serverQuickLoad <- function(xautovars, dataset.preprocessed, dataset.pca) {
  # Note: Shiny is clearly not designed for this
  # Uses the xauto variable in generic importer to tell that specific data should be loaded.
  # Observers & a variable that tracks the current stage allow data to be loaded one after another

  notification.id <- progressNotification("Please wait ... importing data")
  shinyjs::disable("quickio.load")
  
  reset.flow <- function() {
    
    shinyjs::enable("quickio.load")
    removeNotification(id = notification.id)
    
    xautovars$import.readcounts.raw <- NULL
    xautovars$import.sample.annotation <- NULL
    xautovars$import.stage <- "NULL"
  }

  # Raw read counts
  xautovars$import.readcounts.raw <- list(source = "sample",
                                   clear = T,
                                   sample = "Monocytes/readcounts_rna.csv",
                                   parameters = list())
  
  # Flow control
  xautovars$import.stage <- "readcounts.raw"
  
  observeEvent(dataset.preprocessed(), {
    
    validate(need(dataset.preprocessed(), "Waiting for preprocessing"))
    validate(need(dataset.preprocessed()$readcounts.preprocessed, "Waiting for preprocessing"))
    
    if(xautovars$import.stage == "readcounts.raw") {
      xautovars$import.sample.annotation <- list(source = "sample",
                                       clear = T,
                                       sample = "Monocytes/sample_annotation_conditions.csv",
                                       parameters = list(imported_data = c("conditions"),
                                                         collapse_conditions = F)
      )
      xautovars$import.stage <- "sample.annotation"
    }
  })
  
  observeEvent(dataset.pca(), {
    
    validate(need(dataset.pca(), "Waiting for processing"))
    validate(need(dataset.pca()$readcounts.processed, "Waiting for processing"))
    
    if(xautovars$import.stage == "sample.annotation") {
      reset.flow()
    }
    
  })
}

#' Saves dataset into a zip
#'
#' @param filename
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
serverQuickSave <- function(input, output, dataset.pca, xautovars, export.targets) {

  if(is.null(dataset.pca())) {
    showNotification("Currently no data loaded!", type = "error")
    return()
  }

  notification.id <- progressNotification("Please wait ... exporting data")

  shinyjs::disable("quickio.save")

  export.directory <- tempfile("pcago-export")
  export.zip <- paste0(tempfile("pcago-export"), ".zip")
  dir.create(export.directory, recursive = T)

  # Processing report (we can export this without flow control)
  readcountProcessing.at.pca.save(paste0(export.directory, "/processing_report.html"), dataset.pca )
  
  # README
  file.copy("pcago_export_readme.md", paste0(export.directory, "/README.md"))
  
  # Data tables
  xautovars$export.readcounts.raw <- list(filename = paste0(export.directory, "/readcounts_raw.csv"), format = "csv")
  xautovars$export.readcounts.processed <- list(filename = paste0(export.directory, "/readcounts_processed.csv"), format = "csv")
  xautovars$export.readcounts.filtered <- list(filename = paste0(export.directory, "/readcounts_filtered.csv"), format = "csv")
  xautovars$export.readcounts.top.variant <- list(filename = paste0(export.directory, "/readcounts_top_variant.csv"), format = "csv")
  xautovars$export.readcounts.pca.transformed <- list(filename = paste0(export.directory, "/readcounts_pca_transformed.csv"), format = "csv")
  xautovars$export.sample.annotation.conditions <- list(filename = paste0(export.directory, "/sample_conditions.csv"), format = "csv")
  xautovars$export.sample.annotation.sampleinfo <- list(filename = paste0(export.directory, "/sample_annotation.csv"), format = "csv")
  xautovars$export.gene.annotation <- list(filename = paste0(export.directory, "/gene_annotation.csv"), format = "csv")
  xautovars$export.gene.variances.processed <- list(filename = paste0(export.directory, "/gene_variances_processed.csv"), format = "csv")
  xautovars$export.gene.variances.filtered <- list(filename = paste0(export.directory, "/gene_variances_filtered.csv"), format = "csv")
  xautovars$export.pca.pc <- list(filename = paste0(export.directory, "/pca_principal_components.csv"), format = "csv")
  xautovars$export.pca.variances <- list(filename = paste0(export.directory, "/pca_variances.csv"), format = "csv")
  xautovars$export.plot.clustering.readcounts.processed <- list(filename.svg = paste0(export.directory, "/clustering_readcounts_processed.svg"), filename.newick = paste0(export.directory, "/clustering_readcounts_processed.newick"))
  xautovars$export.plot.clustering.readcounts.filtered <- list(filename.svg = paste0(export.directory, "/clustering_readcounts_filtered.svg"), filename.newick = paste0(export.directory, "/clustering_readcounts_filtered.newick"))
  xautovars$export.plot.clustering.readcounts.top.variant <- list(filename.svg = paste0(export.directory, "/clustering_readcounts_top_variant.svg"), filename.newick = paste0(export.directory, "/clustering_readcounts_top_variant.newick"))
  xautovars$export.plot.clustering.readcounts.pca.transformed <- list(filename.svg = paste0(export.directory, "/clustering_readcounts_pca_transformed.svg"), filename.newick = paste0(export.directory, "/clustering_readcounts_pca_transformed.newick"))
  xautovars$export.plot.venn.conditions <- list(filename = paste0(export.directory, "/sample_conditions.tiff"), format = "tiff")
  xautovars$export.plot.pca.sampleplot <- list(filename = paste0(export.directory, "/pca_plot.svg"), format = "svg")
  xautovars$export.plot.variances.readcounts.processed <- list(filename = paste0(export.directory, "/variances_readcounts_processed.svg"), format = "svg")
  xautovars$export.plot.variances.readcounts.filtered <- list(filename = paste0(export.directory, "/variances_readcounts_filtered.svg"), format = "svg")
  xautovars$export.plot.pca.variance <- list(filename = paste0(export.directory, "/pca_variances.svg"), format = "svg")
  xautovars$export.plot.pca.loadings <- list(filename = paste0(export.directory, "/pca_loadings.svg"), format = "svg")
  
  for(target in export.targets) {
    observeEvent(target(), {
      xautovars$export.count <- xautovars$export.count + 1
      updateProgressNotification(notification.id, paste0("Please wait ... exporting data (", xautovars$export.count, "/", length(export.targets), ")"))
    })
  }

  observeEvent(xautovars$export.count, {
    if(xautovars$export.count >= length(export.targets)) {
      
      updateProgressNotification(notification.id, "Please wait ... compressing")
      
      zip(zipfile = export.zip,
          files = export.directory,
          flags = "-r9Xj")
      
      # Reset the flow
      xautovars$export.count <- 0
      xautovars$export.readcounts.raw <- NULL
      xautovars$export.readcounts.processed <- NULL
      xautovars$export.readcounts.filtered <- NULL
      xautovars$export.readcounts.top.variant <- NULL
      xautovars$export.readcounts.pca.transformed <- NULL
      xautovars$export.sample.annotation.conditions <- NULL
      xautovars$export.sample.annotation.sampleinfo <- NULL
      xautovars$export.gene.annotation <- NULL
      xautovars$export.gene.variances.processed <- NULL
      xautovars$export.gene.variances.filtered <- NULL
      xautovars$export.pca.pc <- NULL
      xautovars$export.pca.variances <- NULL
      xautovars$export.plot.clustering.readcounts.processed <- NULL
      xautovars$export.plot.clustering.readcounts.filtered <- NULL
      xautovars$export.plot.clustering.readcounts.top.variant <- NULL
      xautovars$export.plot.clustering.readcounts.pca.transformed <- NULL
      xautovars$export.plot.venn.conditions <- NULL
      xautovars$export.plot.pca.sampleplot <- NULL
      xautovars$export.plot.variances.readcounts.processed <- NULL
      xautovars$export.plot.variances.readcounts.filtered <- NULL
      xautovars$export.plot.pca.variance <- NULL
      xautovars$export.plot.pca.loadings <- NULL
      
      # Reset notification
      shinyjs::enable("quickio.save")
      removeNotification(id = notification.id)
      
      # Show download modal
      showModal(modalDialog(
          "The data is ready for download!",
          footer = tagList(
            modalButton("Close"),
            downloadButton("quickio.save.download", "Download now")
          )
        ))
      
    }
  })
  
  
  output$quickio.save.download <- downloadHandler("pcago-export.zip",
                                                  content = function(filename) {
                                                    file.copy(export.zip, filename, overwrite = T)
                                                  },
                                                  contentType = "application/zip")
  

}

#' QuickIO implementation
#'
#' @param session 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
serverQuickIO <- function(input, output, session, xautovars, dataset.preprocessed, dataset.pca, export.targets) {
  
  observeEvent(input$quickio.load, {
    if(!xautovars$import.ask) {
      serverQuickLoad(xautovars, dataset.preprocessed, dataset.pca)
    }
    else {
      showModal(modalDialog(
        "Do you really want to load example data?",
        footer = tagList(
          modalButton("No"),
          actionButton("quickio.load.yes", "Yes")
        )
      ))
    }
  })
  
  observeEvent(input$quickio.load.yes, {
    removeModal()
    serverQuickLoad(xautovars, dataset.preprocessed, dataset.pca)
  })
  
  observeEvent(input$quickio.save, {
    serverQuickSave(input, output, dataset.pca, xautovars, export.targets)
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

#' Filters the read count table by only returning the rows that are in the list of genes.
#'
#' @param genes.filtered 
#' @param readcounts.processed 
#'
#' @return
#' @export
#'
#' @examples
serverFilterReadcountsByAnnotationKeywords <- function(dataset) {
  
  readcounts.processed <- reactive({ 
    validate(need(dataset(), "[Gene filtering] No readcounts to process!"))
    return(dataset()$readcounts.processed)
  })
  gene.annotation <- reactive({ 
    validate(need(dataset(), "[Gene filtering] No gene annotation available!"))
    return(dataset()$gene.annotation)
  })
  
  # Get the list of genes we want
  genes.filtered <- (geneAnnotationKeywordFilterValues("pca.pca.genes.set",  reactive({
    
    validate(need(readcounts.processed(), "[Gene filtering] No readcounts to process!"),
             need(gene.annotation(), "[Gene filtering] No gene annotation available!"))
    
    
    gene.criteria <- list() # This list contains Category -> list of [ Criterion -> Vector of genes ]
    all.genes <- rownames(readcounts.processed())
    
    annotation <- gene.annotation()
    annotation <- geneAnnotationRestrictToGenes(annotation, all.genes) # The annotation is for the complete set of genes. But we want to filter processed readcounts
    
    {
      unused.genes <- setdiff(all.genes, (annotation@gene.biotype$get_genes()))
      gene.criteria[["Biotype"]] <- annotation@gene.biotype$data
      
      if(length(unused.genes) > 0) {
        gene.criteria[["Biotype"]][["No data"]] <- unused.genes
      }
    }
    {
      unused.genes <- setdiff(all.genes, (annotation@gene.go.ids$get_genes()))
      gene.criteria[["Associated GO terms"]] <- annotation@gene.go.ids$data

      if(length(unused.genes) > 0) {
        gene.criteria[["Associated GO terms"]][["No data"]] <- unused.genes
      }
    }
    {
      unused.genes <- setdiff(all.genes, (annotation@gene.scaffold$get_genes()))
      gene.criteria[["Scaffold"]] <- annotation@gene.scaffold$data
      
      if(length(unused.genes) > 0) {
        gene.criteria[["Scaffold"]][["No data"]] <- unused.genes
      }
    }
    {
      unused.genes <- setdiff(all.genes, (annotation@gene.custom$get_genes()))
      gene.criteria[["Custom"]] <- annotation@gene.custom$data
      
      if(length(unused.genes) > 0) {
        gene.criteria[["Custom"]][["No data"]] <- unused.genes
      }
    }
    
    return(gene.criteria)
    
  })))
  
  # Return a dataset that both contains the filtered read counts and the list of filtered genes
  return(reactive({
    
    validate(need(dataset(), "[Gene filtering] No readcounts to process!"))
    validate(
      need(genes.filtered(), "[Gene filtering] No genes selected!"),
      need(length(genes.filtered()$values) > 0, "[Gene filtering] No genes selected!"))
    
    keep.genes <- rownames(readcounts.processed())
    keep.genes <- intersect(keep.genes, genes.filtered()$values)
    
    keep.readcounts <- readcounts.processed()[keep.genes,]
    
    dataset <- dataset()
    dataset$readcounts.filtered.keywords.parameters.genes <- genes.filtered()
    dataset$readcounts.filtered.keywords <- keep.readcounts
    
    return(dataset)
  }))
}

#' Filters read counts by selecting only the top variant genes
#'
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
serverFilterReadCountsByVariance <- function(dataset, input, animation.top.variant) {
  
  readcounts.filtered <- reactive({
    validate(need(dataset(), "No filtered read counts available!"))
    validate(need(dataset()$readcounts.filtered, "No filtered read counts available!"))
    return(dataset()$readcounts.filtered)
  })
  
  readcounts.top.variant <- reactive({  })
  
  pca.pca.genes.set.count.minimal <- relevantGenesValue("pca.pca.genes.count.findminimal", 
                                                        readcounts = readcounts.filtered,
                                                        pca.center = reactive(input$pca.pca.settings.center),
                                                        pca.scale = reactive(input$pca.pca.settings.scale)) # Minimal set of genes that clusters the same
  observeEvent(pca.pca.genes.set.count.minimal(), {
    updateExtendedSliderInput("pca.genes.count", value = pca.pca.genes.set.count.minimal())
  })
  
  return(reactive({
    validate(need(dataset(), "No filtered read counts available!"))
    validate(need(dataset()$readcounts.filtered, "No filtered read counts available!"))
    validate(need(dataset()$variances.filtered, "No filtered read counts available!"))
    
    dataset <- dataset()
    dataset$readcounts.top.variant <- selectTopVariantGeneReadcounts(dataset$readcounts.filtered, dataset$variances.filtered, animation.top.variant()$value)
    dataset$variances.top.variant <- buildGeneVarianceTable(dataset$readcounts.top.variant)
    dataset$readcounts.top.variant.parameters.count <- animation.top.variant()$value
    
    return(dataset)
    
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