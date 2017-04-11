
library(shiny)
library(AnnotationHub)
source("annotationGRanges.R")

annotationHub.hub <- reactive(AnnotationHub())

annotationHub.speciesChoices <- function(datatype) {
  
  ah <- annotationHub.hub()
  
  if(!is.character(datatype) || datatype == "") {
    return(c())
  }
  
  if(datatype == "sequence.info" || datatype == "associated.features") {
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
  
  if(datatype == "sequence.info" || datatype == "associated.features") {
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
                                                                             "Associated features" = "associated.features",
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
    return(GRanges.extractSequenceInfoAnnotation(gr, genes))
  }
  else if(datatype == "associated.features") {
    gr <- annotationHub.hub()[[dataset]]
    return(GRanges.extractAssociatedFeaturesAnnotation(gr, genes))
  }
  else {
    stop("Unsupported data type!")
  }
  
}