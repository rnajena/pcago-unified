#' 
#' Functions for handling cell annotation visuals
#' 

library(shiny)
source("helpers.R")
source("cellAnnotation.R")

# Importers for condition visuals
supportedConditionVisualsImporters <- list(
  ImporterEntry(name = "csv", label = "CSV"),
  ImporterEntry(name = "tsv", label = "TSV")
)
supportedConditionVisualsGenerators <- list()
availableConditionVisualSamples <- list(
  ImporterEntry(name = "visuals.vitamins.small.csv", label = "Vitamins (Small)"),
  ImporterEntry(name = "visuals.vitamins.large.csv", label = "Vitamins (Large)"))

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
importConditionVisuals <- function(filehandle, datatype, conditions, has.color = T, has.shape = T, has.name = T) {
  
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
  
  expected.columns <- c()
  
  if(has.name) { expected.columns <- c(expected.columns, "name") }
  if(has.color) { expected.columns <- c(expected.columns, "color") }
  if(has.shape) { expected.columns <- c(expected.columns, "shape") }
  
  if(length(expected.columns) == 0) {
    stop("No data stored!")
  }
  
  # Handle errors
  if(!setequal(conditions, rownames(data))) {
    stop("Imported visual definition has different set of conditions!")
  }
  if(!setequal(expected.columns, colnames(data))) {
    stop("Imported visual definition doesn't have all columns!")
  }
  if(has.name && !is.character(data$name)) {
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
  on.exit({ close(con) })
  
  data <- importConditionVisuals(con, "csv", conditions)
  
  return(data)
  
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
generateDefaultConditionVisualsTable <- function(conditions, has.color = T, has.shape = T, has.name = T) {
  
  validate(need(conditions, "Need list of conditions to build visual table!"))
  
  data <- (data.frame(
    row.names = conditions,
    stringsAsFactors = F
  ))
  
  if(has.name) { data$name = rep("", length(conditions)) }
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

#' 
#'
#' @param cells List of cells that is used to create visual parameters for
#' @param conditions Maps a cell to a list of conditions
#' @param visuals.conditions Maps a condtition to visual parameters
#'
#' @return
#' @export
#'
#' @examples
calculateCellVisuals <- function(cells, conditions, condition.visuals) {
  
  if(is.null(cells) || is.null(conditions) || is.null(condition.visuals)) {
    return(NULL)
  }
  
  # Setup output
  factors <- data.frame(row.names = cells,
                        color = rep("#000000", length(cells)),
                        shape = rep(16, length(cells)),
                        stringsAsFactors = F)
  
  palette.colors <- c()
  palette.colors.conditions <- c()
  palette.shapes <- c()
  palette.shapes.conditions <- c()
  
  # Go through each cell and select the color & shape based on the first condition providing it
  for(cell in cells) {
    
    color <- ""
    color.condition <- ""
    shape <- -1
    shape.condition <- ""
    
    for(condition in rownames(condition.visuals)) {
      
      # Check if the cell has the condition. Otherwise skip
      if(!is.logical(conditions[cell, condition]) || !conditions[cell, condition]) {
        next()
      }
      
      if(color == "") {
        mapping.color <- condition.visuals[condition, "color"]
        
        color <- mapping.color
        color.condition <- condition
      }
      
      if(shape == -1) {
        shape <- condition.visuals[condition, "shape"]
        shape.condition <- condition
      }
      
    }
    
    if(color == "") {
      color = "#000000"
      color.condition <- condition.default
    }
    if(shape == -1) {
      shape = 16
      shape.condition <- condition.default
    }
    
    factors[cell, "color"] <- color.condition # todo: user name for condition
    factors[cell, "shape"] <- shape.condition
    
    if(!(color.condition %in% palette.colors.conditions)) { 
      
      palette.colors <- c(palette.colors, color) 
      palette.colors.conditions <- c(palette.colors.conditions, color.condition)
    }
    if(!(shape.condition %in% palette.shapes.conditions)) {
      
      palette.shapes <- c(palette.shapes, shape) 
      palette.shapes.conditions <- c(palette.shapes.conditions, shape.condition)
    }
    
  }
  
  #Convert to factors
  factors$color <- factor(factors$color, levels = palette.colors.conditions)
  factors$shape <- factor(factors$shape, levels = palette.shapes.conditions)
  
  return(list("factors" = factors, 
              "palette.colors" = palette.colors, 
              "palette.shapes" = palette.shapes))
  
}