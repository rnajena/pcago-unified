
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