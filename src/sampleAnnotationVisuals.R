#' 
#' Functions for handling sample annotation visuals
#' 

library(shiny)
source("helpers.R")
source("sampleAnnotation.R")

# Importers for condition visuals
supportedConditionVisualsImporters <- list(
  ImporterEntry(name = "csv", label = "PCAGO visuals CSV (*.csv)", parameters = list(ImporterParameter.csv))
)
supportedConditionVisualsGenerators <- list()
availableConditionVisualSamples <- list(
  ImporterEntry(name = "Monocytes/pcago_visuals.csv", label = "Visuals for Monocytes"),
  ImporterEntry(name = "Mouse/pcago_visuals.csv", label = "Visuals for Mouse"),
  ImporterEntry(name = "Myotis RNA/pcago_visuals.csv", label = "Visuals for Myotis RNA"),
  ImporterEntry(name = "Myotis smallRNA/pcago_visuals.csv", label = "Visuals for Myotis smallRNA"))

importConditionVisuals.CSV <- function(filehandle, parameters, conditions) {
  
  data <- read.csv(filehandle, sep = parameters$separator, row.names = 1, stringsAsFactors = F, check.names = F)
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
importConditionVisuals <- function(filehandle, importer, parameters, conditions, expected.columns = c("name", "color", "shape")) {
  
  if(!all(expected.columns %in% c("name", "color", "shape"))) {
    stop("Requested existance of unexpected columns!")
  }
  
  data <- NULL
  
  if(importer == "csv") {
    data <- importConditionVisuals.CSV(filehandle, parameters, conditions)
  }
  else {
    stop(paste("Unknown importer", importer))
  }
  
  if(length(expected.columns) == 0) {
    stop("No data stored!")
  }
  
  # Handle errors
  has.name <- "name" %in% expected.columns
  has.color <- "color" %in% expected.columns
  has.shape <- "shape" %in% expected.columns
  
  if(!setequal(conditions, rownames(data))) {
    stop("Imported visual definition has different set of conditions!")
  }
  if(!setequal(expected.columns, colnames(data))) {
    stop("Imported visual definition doesn't have all columns!")
  }
  if(has.name) {
    data$name <- as.character(data$name)
  }
  if(has.color) {
    data$color <- as.character(data$color)
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
importConditionVisualsSample <- function(sample, parameters, conditions, expected.columns = c("name", "color", "shape")) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  parameters$separator <- ","
  
  data <- importConditionVisuals(filehandle = con, 
                                 importer = "csv", 
                                 parameters = parameters, 
                                 conditions = conditions, 
                                 expected.columns = expected.columns)
  
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
generateDefaultConditionVisualsTable <- function(conditions, expected.columns = c("name", "color", "shape")) {
  
  validate(need(conditions, "Need list of conditions to build visual table!"))
  
  data <- (data.frame(
    row.names = conditions,
    stringsAsFactors = F
  ))
  
  if("name" %in% expected.columns) { data$name = rep("", length(conditions)) }
  if("color" %in% expected.columns) { data$color = colorRampPalette(brewer.pal(9, "Set1"))(length(conditions)) }
  if("shape" %in% expected.columns) { data$shape = rep(-1, length(conditions)) }
  
  return(data)
  
}

#' Given a set of conditions, return the name from condition visuals if avialable
#' otherwise return the conditions
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

#' Builds the color & shape palettes (for plots) from a boolean conditions table and condition visuals
#'
#' @param samples List of samples that is used to create visual parameters for
#' @param conditions Maps a sample to a list of conditions
#' @param visuals.conditions Maps a condtition to visual parameters
#'
#' @return
#' @export
#'
#' @examples
calculateSampleVisuals <- function(samples, conditions, condition.visuals) {
  
  if(is.null(samples) || is.null(conditions) || is.null(condition.visuals)) {
    return(NULL)
  }
  
  # Setup output
  factors <- data.frame(row.names = samples,
                        color = rep("#000000", length(samples)),
                        shape = rep(16, length(samples)),
                        stringsAsFactors = F)
  
  palette.colors <- c()
  palette.colors.conditions <- c()
  palette.shapes <- c()
  palette.shapes.conditions <- c()
  
  # Go through each sample and select the color & shape based on the first condition providing it
  for(sample in samples) {
    
    color <- ""
    color.condition <- ""
    shape <- -1
    shape.condition <- ""
    
    for(condition in rownames(condition.visuals)) {
      
      # Check if the sample has the condition. Otherwise skip
      if(!is.logical(conditions[sample, condition]) || !conditions[sample, condition]) {
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
    
    factors[sample, "color"] <- color.condition # todo: user name for condition
    factors[sample, "shape"] <- shape.condition
    
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