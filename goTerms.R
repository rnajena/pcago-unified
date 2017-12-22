#'
#' Functions regarding GO terms
#' 

library(shiny)
library(GO.db)
library(AnnotationDbi)

# Importers for condition visuals
supportedSelectedGOTermsImporters <- list(
  ImporterEntry(name = "csv", label = "PCAGO Selected GO Terms CSV (*.csv)", parameters = list(ImporterParameter.csv))
)
supportedSelectedGOTermsGenerators <- list()
availableSelectedGOTermsSamples <- list()

importSelectedGOTerms.CSV <- function(filehandle, parameters, available.terms) {
  
  data <- read.csv(filehandle, sep = parameters$separator, row.names = NULL, stringsAsFactors = F, check.names = F)
  return(data)
}

#' Imports visual definitions from filehandle with importer defined by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedReadcountDataTypes
#' @param conditions Vector of condition names
#'
#' @return Data frame containing the visual parameters for each condition
#' @export
#'
#' @examples
importSelectedGOTerms <- function(filehandle, importer, parameters, available.terms) {
  
  if(length(available.terms) == 0) {
    stop("There are no available GO terms!")
  }
  
  data <- NULL
  
  if(importer == "csv") {
    data <- importSelectedGOTerms.CSV(filehandle, parameters, available.terms)
  }
  else {
    stop(paste("Unknown importer", importer))
  }
  
  if(nrow(data) == 0) {
    stop("Imported data has no rows!")
  }
  if(ncol(data) == 0) {
    stop("Imported data has no columns!")
  }
  if(!("goid" %in% names(data)) ) {
    stop("Imported data must contain a column 'goid'!")
  }
  
  matching.indices <- match(data$goid, available.terms)
  
  if(length(matching.indices) == 0) {
    stop("The file does not contain any of the available GO IDs!")
  }
  
  return(unique(data$goid[matching.indices]))
}

#' Imports sample with given sample id
#'
#' @param sample 
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importSelectedGOTermsSample <- function(sample, parameters, available.terms) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  parameters$separator <- ","
  
  data <- importSelectedGOTerms(filehandle = con, 
                                importer = "csv", 
                                parameters = parameters, 
                                available.terms = available.terms)
  
  return(data)
  
}

exportSelectedGOTerms.CSV <- function(filename, goids) {
  
  if(length(goids) > 0) {
    frame <- AnnotationDbi::select(GO.db, goids, columns = c("TERM", "DEFINITION"))
    names(frame) <- c("goid", "goterm", "definition")
    
    write.csv(frame, file = filename, sep = ",", row.names = F)
  }
  
}

#' Builds UI that describes GO terms of given IDs
#'
#' @param goids 
#'
#' @return
#' @export
#'
#' @examples
goTermsBuildInfoUI <- function(goids) {
  
  output <- tagList()
  
  for(goid in goids) {
    
    info <- GO.db::GOTERM[[goid]]
    
    if(is.null(info)) {
      output <- tagAppendChild(output, tags$div(
        class = "go-term-details",
        tags$h2("Unknown GO ID"),
        vSkip(5),
        tags$a(href=paste0("http://amigo.geneontology.org/amigo/medial_search?q=", info@GOID), target = "_blank", "Search on AmiGO 2"),
        
        hDivider()
      ))
    }
    else {
      ontology <- "Unknown"
      
      if(info@Ontology == "BP") {
        ontology <- "Biological Process (BP)"
      }
      else if(info@Ontology == "CC") {
        ontology <- "Cellular Component (CC)"
      }
      else if(info@Ontology == "MF") {
        ontology <- "Molecular Function (MF)"
      }
      
      alternative.ids <- if(length(info@Secondary) > 0) paste(info@Secondary, collapse = ", ") else "None"
      synonyms <- if(length(info@Synonym) > 0) paste(info@Synonym, collapse = ", ") else "None"
      
      output <- tagAppendChild(output, tags$div(
        class = "go-term-details",
        tags$h2(info@GOID),
        tags$h3(info@Term),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Ontology"), tags$span(class = "value", ontology)),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Alternate IDs"), tags$span(class = "value", alternative.ids)),
        tags$div(class = "goterm-info-entry", tags$span(class = "key", "Synonyms"), tags$span(class = "value", synonyms)),
        vSkip(10),
        tags$div(class = "definition", info@Definition),
        vSkip(5),
        tags$a(href=paste0("http://amigo.geneontology.org/amigo/term/", info@GOID), target = "_blank", "View on AmiGO 2"),
        
        hDivider()
      ))
    }
    
  }
  
  return(output)
  
}

#' Selects the GO ids that are the root of the drilldown menu
#'
#' @param goids 
#'
#' @return
#' @export
#'
#' @examples
goTermsFindRootGOIds <- function(goids) {
  
  # We assume that all parent GO terms are also existing
  # Just search for the GO ids that have parent is_a "all"
  roots <- c()
  
  info <- AnnotationDbi::select(GO.db, goids, columns = c("ONTOLOGY"))
  
  # Biological Process (BP)
  goids.bp <- goids[info$ONTOLOGY == "BP"]
  if(length(goids.bp) > 0) {
    roots <- c(roots, goids.bp[sapply(as.list(GOBPPARENTS[goids.bp]), function(x) "is_a" %in% names(x) && x["is_a"] == "all")])
  }
  
  # Cellular Component (CC)
  goids.cc <- goids[info$ONTOLOGY == "CC"]
  if(length(goids.cc) > 0) {
    roots <- c(roots, goids.cc[sapply(as.list(GOCCPARENTS[goids.cc]), function(x) "is_a" %in% names(x) && x["is_a"] == "all")])
  }
  
  # Molecular Function (MF)
  goids.mf <- goids[info$ONTOLOGY == "MF"]
  if(length(goids.mf) > 0) {
    roots <- c(roots, goids.mf[sapply(as.list(GOMFPARENTS[goids.mf]), function(x) "is_a" %in% names(x) && x["is_a"] == "all")])
  }
  
  return(roots)
  
}

#' Finds the child GO ids of selection
#'
#' @param goids 
#'
#' @return
#' @export
#'
#' @examples
goTermsFindChildGOIds <- function(goids, selection) {
  children <- c()
  goids.full <- goids
  
  goids <- goids[goids == selection]
  
  info <- AnnotationDbi::select(GO.db, selection, columns = c("ONTOLOGY"))
  
  # Biological Process (BP)
  goids.bp <- goids[info$ONTOLOGY == "BP"]
  if(length(goids.bp) > 0) {
    children <- c(children, sapply(as.list(GOBPCHILDREN[goids.bp]), function(x) x[names(x) == "is_a"]))
  }
  
  # Cellular Component (CC)
  goids.cc <- goids[info$ONTOLOGY == "CC"]
  if(length(goids.cc) > 0) {
    children <- c(children, sapply(as.list(GOCCCHILDREN[goids.cc]), function(x) x[names(x) == "is_a"]))
  }
  
  # Molecular Function (MF)
  goids.mf <- goids[info$ONTOLOGY == "MF"]
  if(length(goids.mf) > 0) {
    children <- c(children, sapply(as.list(GOMFCHILDREN[goids.mf]), function(x) x[names(x) == "is_a"]))
  }
  
  found.terms <- as.vector(na.omit(match(children, goids.full)))
  
  if(length(found.terms) == 0) {
    return(c("No terms found" = "none"))
  }
  else {
    return(goids.full[found.terms])
  }
}