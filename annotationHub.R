
library(shiny)
library(AnnotationHub)

annotationHub.hub <- reactive(AnnotationHub())

annotationHub.speciesChoices <- function(datatype) {
  
  ah <- annotationHub.hub()
  
  if(!is.character(datatype) || datatype == "") {
    return(c())
  }
  
  if(datatype == "sequence.info") {
    return(unique(ah$species[ah$rdataclass == "GRanges"]))
  }
  else {
    stop("Unknown data type!")
  }
  
}

annotationHub.datasetChoices <- function(datatype, species) {
  
  ah <- annotationHub.hub()
  
  if(!is.character(datatype) || !is.character(species) || datatype == "" || species == "") {
    return(c())
  }
  
  if(datatype == "sequence.info") {
    query.results <- AnnotationHub::query(ah, c("GRanges", "Ensembl", species))
    available.datasets <- mcols(query.results)
    
    choices <- rownames(available.datasets)
    names(choices) <- sapply(choices, function(x) { sprintf("%s (%s)", available.datasets[x, "title"], available.datasets[x, "genome"]) })
    
    return(choices)
  }
  else {
    stop("Unknown data type!")
  }
  
}

annotationHub.importerEntry <- ImporterEntry(name = "annotation_hub",
                                       label = "Annotation Hub",
                                       parameters = list(
                                         ImporterParameter(name = "datatype",
                                                           label = "Extract information",
                                                           type = "select",
                                                           select.values = c("Sequence info" = "sequence.info",
                                                                             "GO terms" = "go.terms")),
                                         ImporterParameter(name = "species", 
                                                           label = "Species", 
                                                           type = "select", 
                                                           select.values = annotationHub.speciesChoices),
                                         ImporterParameter(name = "dataset",
                                                           label = "Dataset",
                                                           type = "select",
                                                           select.values = annotationHub.datasetChoices)
                                       ))

generateGeneInformation.AnnotationHub <- function(datatype, species, dataset, readcounts) {
  
  if(!is.character(species) || species == "") {
    stop("Invalid species!")
  }
  if(!is.character(dataset) || dataset == "") {
    stop("Invalid data set!")
  }
  
  genes <- rownames(readcounts)
  
  if(datatype == "sequence.info") {
    
    gr <- annotationHub.hub()[[dataset]]
    gff <- mcols(gr)
    meta.indices <- match(genes, gff$gene_id) # F: Index of gene in readcounts -> Index of gene in gff annotation
    
    if(any(is.na(meta.indices))) {
      
      # Remove genes that are NA. They will not appear in the annotation
      na.genes <- genes[is.na(meta.indices)]
      genes <- genes[!is.na(meta.indices)]
      meta.indices <- na.omit(meta.indices)
      
      showNotification(type = "warning", 
                       duration = NULL,
                       paste("Could not find sequence information for all genes. Following genes are affected:", 
                             strJoinLimited(na.genes, limit = 10)))
      
    }
    
    sequence.info <- data.frame(row.names = genes,
                                scaffold = as.vector(seqnames(gr)[meta.indices]),
                                start_position = start(gr)[meta.indices],
                                end_position = end(gr)[meta.indices],
                                length = width(gr)[meta.indices],
                                stringsAsFactors = F)
    
    # As we have sequence info, extract scaffold filter
    scaffolds <- list()
    for(scaffold in unique(sequence.info$scaffold)) {
      scaffolds[[scaffold]] <- rownames(sequence.info)[sequence.info$scaffold == scaffold]
    }
    
    return(Annotation(sequence.info = sequence.info, 
                      gene.scaffold = GeneFilter(data = scaffolds)))
    
  }
  else {
    stop("Unsupported data type!")
  }
  
}