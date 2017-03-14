#'
#' Contains functions that are used for plot visualization
#' 

library(shiny)
source("helpers.R")

# Default condition for later use
condition.default <- "{default}"

# Importers for condition visuals
supportedConditionVisualsImporters <- c("CSV" = "csv_comma",
                                 "TSV" = "csv_whitespace")
supportedConditionVisualsFileTypes <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

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
importConditionVisuals <- function(filehandle, datatype, conditions) {
  
  sep = ","
  
  if(datatype == "csv_whitespace") {
    sep = ""
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  # Handle errors
  if(!setequal(conditions, rownames(data))) {
    stop("Imported visual definition has different set of conditions!")
  }
  if(!setequal(c("color", "shape"), colnames(data))) {
    stop("Imported visual definition doesn't have all columns!")
  }
  if(!is.character(data$color)) {
    stop("Imported visual definition has invalid colors!")
  }
  if(!all(isColor(data$color[data$color != ""]))) {
    stop("Imported visual definition has invalid colors!")
  }
  if(!is.numeric(data$shape)) {
    stop("Imported visual definition has invalid shapes!")
  }
  
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
#' @return Data frame with columns for color ("color") and shape ("shape") where rows are the conditions
#' @export
#'
#' @examples
generateDefaultConditionVisualsTable <- function(conditions) {
  
  validate(need(conditions, "Need list of conditions to build visual table!"))
  
  return(data.frame(
    row.names = conditions,
    color = colorRampPalette(brewer.pal(9, "Set1"))(length(conditions)),
    shape = rep(-1, length(conditions)),
    fill = rep("", length(conditions)),
    name = rep("", length(conditions)),
    stringsAsFactors = F
  ))
  
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

#' Builds cell visual table that finds the conditions applying to a cell and generates the
#' visual parameters for it
#'
#' @param input 
#' @param readcounts.processed 
#' @param conditions 
#' @param visuals.conditions 
#'
#' @return
#' @export
#'
#' @examples
serverGetCellVisualsTable <- function(input, readcounts.processed, conditions, visuals.conditions) {
  
  validate(
    need(readcounts.processed(), "No data to build visual parameter table from!"),
    need(conditions(), "No conditions for visual mapping!"),
    need(visuals.conditions(), "No condition visual mapping!")
  )
  
  cells <- colnames(readcounts.processed())
  cells.conditions <- conditions()
  visuals.conditions <- visuals.conditions()
  
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
    
    for(condition in rownames(visuals.conditions)) {
      
      if(!cells.conditions[cell, condition]) {
        next()
      }
      
      if(color == "") {
        mapping.color <- visuals.conditions[condition, "color"]
        
        color <- mapping.color
        color.condition <- condition
      }
      
      if(shape == -1) {
        shape <- visuals.conditions[condition, "shape"]
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