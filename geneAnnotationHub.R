
library(shiny)
library(AnnotationHub)
source("geneAnnotationGRanges.R")

annotationHub.hub <- reactive(AnnotationHub())

annotationHub.databaseChoices <- function() {
  ah <- annotationHub.hub()
  return(c("", unique(ah$dataprovider[ah$rdataclass == "GRanges"])))
}

annotationHub.speciesChoices <- function(database) {
  
  ah <- annotationHub.hub()
 
  if(!is.character(database) || length(database) != 1 || database == "") {
    return(c())
  }
 
  choices <- unique(ah$species[ah$rdataclass == "GRanges" & ah$dataprovider == database])
  choices <- na.omit(choices)
  
  return(c("", choices))
}

annotationHub.datasetChoices <- function(database, species) {
  
  ah <- annotationHub.hub()
  
  if(!is.character(species) || !is.character(database) || length(database) != 1 || length(species) != 1 || database == "" || species == "") {
    return(c())
  }
  
  query.results <- AnnotationHub::query(ah, c("GRanges", database, species))
  available.datasets <- mcols(query.results)
  
  choices <- rownames(available.datasets)
  names(choices) <- sapply(choices, function(x) { sprintf("%s (%s)", available.datasets[x, "title"], available.datasets[x, "genome"]) })
  
  return(c("", choices))
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
  
  if(!is.character(database) || database == "" || length(database) != 1) {
    stop("Invalid database!")
  }
  if(!is.character(species) || species == "" || length(species) != 1) {
    stop("Invalid species!")
  }
  if(!is.character(dataset) || dataset == "" || length(dataset) != 1) {
    stop("Invalid data set!")
  }
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  genes <- rownames(readcounts)
  
  output <- GeneAnnotation()
  gr <- annotationHub.hub()[[dataset]]
  
  if("biotype" %in% imported_data) {
    output <- mergeGeneAnnotation(output, GRanges.extractBiotypeAnnotation(gr, genes))
  }
  if("scaffold" %in% imported_data) {
    output <- mergeGeneAnnotation(output, GRanges.extractScaffoldAnnotation(gr, genes))
  }
  if(length(intersect(GeneAnnotationEntryNames.sequence.info, imported_data)) > 0) {
    output <- mergeGeneAnnotation(output, GRanges.extractSequenceInfoAnnotation(gr, genes, imported_data))
  }
  
  output <- geneAnnotationRestrictContentTypes(output, imported_data)
  
  return(output)
}