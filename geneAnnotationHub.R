
library(shiny)
library(AnnotationHub)
source("geneAnnotationGRanges.R")

annotationHub.hub <- reactive(AnnotationHub())

annotationHub.databaseChoices <- function() {
  return(c("", unique(ah$dataprovider[ah$rdataclass == "GRanges"])))
}

annotationHub.speciesChoices <- function(database) {
  
  ah <- annotationHub.hub()
 
  if(!is.character(database) || database == "") {
    return(c())
  }
 
  choices <- unique(ah$species[ah$rdataclass == "GRanges" & ah$dataprovider == database])
  choices <- na.omit(choices)
  
  return(c("", choices))
}

annotationHub.datasetChoices <- function(database, species) {
  
  ah <- annotationHub.hub()
  
  if(!is.character(species) || !is.character(database) || database == "" || species == "") {
    return(c())
  }
  
  if(datatype == "sequence.info" || datatype == "biotype") {
    query.results <- AnnotationHub::query(ah, c("GRanges", database, species))
    available.datasets <- mcols(query.results)
    
    choices <- rownames(available.datasets)
    names(choices) <- sapply(choices, function(x) { sprintf("%s (%s)", available.datasets[x, "title"], available.datasets[x, "genome"]) })
    
    return(c("", choices))
  }
  else {
    stop("Unknown data type!")
  }
  
}

annotationHub.importerEntry <- ImporterEntry(name = "annotation_hub",
                                       label = "Annotation Hub",
                                       parameters = list(
                                         ImporterParameter(name = "database", 
                                                           label = "Database", 
                                                           type = "select", 
                                                           select.values = annotationHub.databaseChoices),
                                         ImporterParameter(name = "species", 
                                                           label = "Species", 
                                                           type = "select", 
                                                           select.values = annotationHub.speciesChoices),
                                         ImporterParameter(name = "dataset",
                                                           label = "Dataset",
                                                           type = "select",
                                                           select.values = annotationHub.datasetChoices),
                                         ImporterParameter(name = "imported_data",
                                                           label = "Imported data",
                                                           type = "checkboxes",
                                                           checkboxes.options = GeneAnnotationEntryNames[!(GeneAnnotationEntryNames %in% c("go_terms"))],
                                                           checkboxes.selected = GeneAnnotationEntryNames[!(GeneAnnotationEntryNames %in% c("go_terms"))])
                                       ))

generateGeneInformation.AnnotationHub <- function(database, species, dataset, imported_data, readcounts) {
  
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
  else if(datatype == "biotype") {
    gr <- annotationHub.hub()[[dataset]]
    return(GRanges.extractBiotypeAnnotation(gr, genes))
  }
  else {
    stop("Unsupported data type!")
  }
  
}