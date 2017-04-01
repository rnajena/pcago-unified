#'
#' Contains functions that are used for plot visualization
#' 

library(shiny)
source("helpers.R")

# Default condition for later use
condition.default <- "{default}"

# Importers for cell condition mappings
supportedCellConditionImporters <- list(
  ImporterEntry(name = "csv", label = "CSV"),
  ImporterEntry(name = "tsv", label = "TSV")
)
availableCellConditionSamples <- list()
supportedCellConditionGenerators <- list()

# Importers for condition visuals
supportedConditionVisualsImporters <- list(
  ImporterEntry(name = "csv", label = "CSV"),
  ImporterEntry(name = "tsv", label = "TSV")
)
supportedConditionVisualsGenerators <- list()
availableConditionVisualSamples <- list(
  ImporterEntry(name = "visuals.vitamins.small.csv", label = "Vitamins (Small)"),
  ImporterEntry(name = "visuals.vitamins.large.csv", label = "Vitamins (Large)"))

#' Imports cell condition assignments from filehandle with importer definded by datatype
#'
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedCellConditionFileTypes
#' @param cells Vector of cell names that have to be explained by the table
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importCellConditions <- function(filehandle, datatype, cells) {
  
  if(missing(filehandle) || !is.character(datatype) || !is.character(cells)) {
    stop("Invalid arguments!")
  }
  
  sep <- ","
  
  if(datatype == "tsv") {
    sep <- ""
  }
  else if(datatype == "csv") {
    sep <- ","
  }
  else {
    stop(paste("Unsupported format", datatype))
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Cell condition table is empty!")
  }
  if(!all(apply(data, 1, function(x) { is.logical(x) }))) {
    stop("Cell condition table is not entirely boolean!")
  }
  if(!setequal(rownames(data), cells)) {
    stop("Data does not assign conditions to all cells!")
  }
  
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
importConditionVisuals <- function(filehandle, datatype, conditions, has.color = T, has.shape = T) {
  
  sep <- ","
  
  if(datatype == "tsv") {
    sep <- ""
  }
  else if(datatype == "csv") {
    sep <- ","
  }
  else {
    stop(paste("Unsupported format", datatype))
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F, check.names = F)
  expected.columns <- c("name")
  
  if(has.color) { expected.columns <- c(expected.columns, "color") }
  if(has.shape) { expected.columns <- c(expected.columns, "shape") }
  
  # Handle errors
  if(!setequal(conditions, rownames(data))) {
    stop("Imported visual definition has different set of conditions!")
  }
  if(!setequal(expected.columns, colnames(data))) {
    stop("Imported visual definition doesn't have all columns!")
  }
  if(!is.character(data$name)) {
    stop("Imported visual definition has invalid names!")
  }
  if(has.color && !is.character(data$color)) {
    stop("Imported visual definition has invalid colors!")
  }
  if(has.color && !all(isColor(data$color[data$color != ""]))) {
    stop("Imported visual definition has invalid colors!")
  }
  if(has.shape && !is.numeric(data$shape)) {
    stop("Imported visual definition has invalid shapes!")
  }
  
  return(data)
}

#' Imports sample with given sample id
#'
#' @param sample 
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importConditionVisualsSample <- function(sample, conditions) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  data <- importConditionVisuals(con, "csv", conditions)
  close(con)
  return(data)
  
}


#' Generates a condition table by separating the condition names 
#'
#' @param readcounts 
#' @param sep Separator that is applied to column names to find conditions. Set to empty string to generate a condition table only based on colum names
#'
#' @return Data table with first column
#' @export
#'
#' @examples
generateConditionTable <- function(readcounts, sep = "_") {
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  cells <- names(readcounts)
  result <- data.frame(row.names = cells, stringsAsFactors = F)
  
  # Go through all cells and determine which conditions apply to it
  # if the condition is not known, yet -> Create a new column
  # set the value for the corresponding cell/condition pair to true
  for(i in 1:nrow(result)) {
    
    conditions <- c()
    
    if(sep == "" || !grepl(sep, cells[i], fixed = T)) {
      conditions <- c(cells[i])
    }
    else {
      conditions <- unlist(strsplit(cells[i], sep))
    }
    
    for(cond in conditions) {
      
      if( ncol(result) == 1 || !(cond %in% names(result))) {
        
        result[[cond]] <- rep(F, nrow(result))
        
      }
      
      result[[cond]][i] <- T
    }
    
  }
  
  # Order condition by variance
  result <- result[,order(colVars(data.matrix(result)), decreasing = T)]
  
  return(result)
  
}

#' Builds a condition visuals table that contains a color and a shape for each condition.
#'
#' @param input 
#' @param conditions List of condition names
#'
#' @return Data frame with columns for color ("color") and shape ("shape"), name ("name") where rows are the conditions
#' @export
#'
#' @examples
generateDefaultConditionVisualsTable <- function(conditions, has.color = T, has.shape = T) {
  
  validate(need(conditions, "Need list of conditions to build visual table!"))
  
  data <- (data.frame(
    row.names = conditions,
    name = rep("", length(conditions)),
    stringsAsFactors = F
  ))
  
  if(has.color) { data$color = colorRampPalette(brewer.pal(9, "Set1"))(length(conditions)) }
  if(has.shape) { data$shape = rep(-1, length(conditions)) }
  
  return(data)
  
}

#' Gets the
#'
#' @param visuals.conditions Table of all condition visual parameters
#' @param cond Vector of conditions
#'
#' @return Vector of names for the input conditions
#' @export
#'
#' @examples
conditionName <- function(visuals.conditions, conditions) {
  
  return(sapply(conditions, function(cond) {
    
    if(cond == condition.default) {
      return("Default")
    }
    else if(visuals.conditions[cond, "name"] != "") {
      return(visuals.conditions[cond, "name"])
    }
    else {
      return(cond)
    }
    
  }))
  
}