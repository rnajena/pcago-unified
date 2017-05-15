#'
#' Contains functions that are used for plot visualization
#' 

library(shiny)
source("helpers.R")
source("classImporterEntry.R")
source("classCellAnnotation.R")

# Default condition for later use
condition.default <- "{default}"

# Importers for cell condition mappings
supportedCellAnnotationImporters <- list(
  ImporterEntry(name = "conditions_factor_csv", label = "Conditions treatments CSV"),
  ImporterEntry(name = "conditions_factor_tsv", label = "Conditions treatments TSV"),
  ImporterEntry(name = "conditions_boolean_csv", label = "Conditions boolean CSV"),
  ImporterEntry(name = "conditions_boolean_tsv", label = "Conditions boolean TSV"),
  ImporterEntry(name = "cell_info_csv", label = "Cell info CSV"),
  ImporterEntry(name = "cell_info_tsv", label = "Cell info TSV")
)
availableCellAnnotationSamples <- list(
  ImporterEntry(name = "conditions.vitamins.large.csv", label = "Conditions for Vitamins (Large)"),
  ImporterEntry(name = "cell.annotation.vitamins.csv", label = "Cell info for Vitamins")
)
supportedCellAnnotationGenerators <- list(
  ImporterEntry(name = "conditions_split", label = "Conditions from cell names", parameters = list(
    ImporterParameter(name = "separator", label = "Separator", type = "select", select.values = c("_","#","|",",",";"," ",""))
  ))
)



#' Imports cell condition assignments from filehandle
#' This imports Boolean condition assignments (which assigns True/False to whether a cell has given conditions)
#' 
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedCellConditionFileTypes
#' @param cells Vector of cell names that have to be explained by the table
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importCellAnnotation.Conditions.Boolean <- function(filehandle, sep, cells) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(cells)) {
    stop("Invalid arguments!")
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
  
  return(CellAnnotation(conditions = data))
}

#' Imports cell condition assignments from filehandle
#' This imports the data from a Factor table.
#' The imported table contains strings that represent the condition
#' according to the column (e.g. Column says "Applied Vitamin"; Rows contain "Vitamin A", ...)
#' 
#' This function will convert the data into the boolean representation used by the
#' other functions
#'
#' @param filehandle 
#' @param sep 
#' @param cells 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotation.Conditions.Factor <- function(filehandle, sep, cells) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(cells)) {
    stop("Invalid arguments!")
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Cell condition table is empty!")
  }
  if(!setequal(rownames(data), cells)) {
    stop("Table annotates a different set of cells!")
  }
  
  output <- data.frame(row.names = rownames(data))
  
  for(treatment in colnames(data)) {
    
    for(i in seq_len(nrow(data))) {
      
      # The condition that is built from the factor
      condition <- paste0(treatment, "_", data[i, treatment])
      
      if(ncol(output) == 0 || !(condition %in% colnames(output))) {
        output[[condition]] <- rep(F, nrow(data))
      }
      
      output[i, condition] <- T
      
    }
    
  }
  
  return(CellAnnotation(conditions = output))
  
}

#' Imports cell info annotation (e.g. mean fragment length) from CSV
#'
#' @param filehandle 
#' @param sep 
#' @param cells 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotation.CellInfo <- function(filehandle, sep, cells) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(cells)) {
    stop("Invalid arguments!")
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Cell info table is empty!")
  }
  if(length(intersect(rownames(data), cells)) == 0) {
    stop("Cell info table does not annotate even one cell!")
  }
  if(!setequal(colnames(data), c("meanfragmentlength"))) {
    stop("Cell info table is missing columns!")
  }
  
  # Restrict to set of cells by parameter
  data <- data[cells,,drop=F]
  
  return(CellAnnotation(cell.info = data))
  
}

#' Imports cell condition assignments from filehandle with importer definded by datatype
#'
#' @param filehandle 
#' @param datatype 
#' @param cells 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotation <- function(filehandle, datatype, cells) {
  
  if(datatype == "conditions_boolean_csv") {
    return(importCellAnnotation.Conditions.Boolean(filehandle, ",", cells))
  }
  else if(datatype == "conditions_boolean_tsv") {
    return(importCellAnnotation.Conditions.Boolean(filehandle, "", cells))
  }
  else if(datatype == "conditions_factor_csv") {
    return(importCellAnnotation.Conditions.Factor(filehandle, ",", cells))
  }
  else if(datatype == "conditions_factor_tsv") {
    return(importCellAnnotation.Conditions.Factor(filehandle, "", cells))
  }
  else if(datatype == "cell_info_csv") {
    return(importCellAnnotation.CellInfo(filehandle, ",", cells))
  }
  else if(datatype == "cell_info_tsv") {
    return(importCellAnnotation.CellInfo(filehandle, "", cells))
  }
  else {
    stop(paste("Unknown importer", datatype))
  }
  
}

#' Imports cell condition assignments from sample
#'
#' @param sample 
#' @param cells 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotationSample <- function(sample, cells) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  if(sample == "cell.annotation.vitamins.csv") {
    data <- importCellAnnotation(con, "cell_info_csv", cells)
  }
  else if(sample == "conditions.vitamins.large.csv") {
    data <- importCellAnnotation(con, "conditions_factor_csv", cells)
  }
  else {
    stop(paste("Unknown sample", sample))
  }
  
  
  return(data)
  
}

#' Generates cell condonditions assignment by splitting the cell names
#'
#' @param cells 
#' @param sep 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotationFromGenerator.Conditions.SplitCellNames <- function(cells, sep) {
  
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
      
      if( ncol(result) == 0 || !(cond %in% colnames(result))) {
        result[[cond]] <- rep(F, nrow(result))
      }
      
      result[[cond]][i] <- T
    }
    
  }
  
  # Order condition by variance
  result <- result[,order(colVars(data.matrix(result)), decreasing = T)]
  
  return(CellAnnotation(conditions = result))
}

#' Imports cell condition assignments from generator
#'
#' @param sample 
#' @param cells 
#'
#' @return
#' @export
#'
#' @examples
importCellAnnotationFromGenerator <- function(generator, cells, parameters) {
  
  if(!is.character(generator)) {
    stop("Invalid arguments!")
  }
  
  if(generator == "conditions_split") {
    return(importCellAnnotationFromGenerator.Conditions.SplitCellNames(cells, parameters$separator))
  }
  else {
    stop(paste("Unknown generator", datatype))
  }
  
}

#' Collapses the conditions for each cell in the condition table
#' into a string.
#'
#' @param condition.table Table that determines if a cell has a condition
#' @param conditions Vector of condition names that should be considered
#'
#' @return Vector of condition names
#' @export
#'
#' @examples
collapseConditions <- function(condition.table, conditions) {
  
  if(length(setdiff(conditions, colnames(condition.table))) > 0) {
    stop("Conditions do not match condition table!")
  }
  
  return(sapply(rownames(condition.table), function(cell) {
    return(paste(na.omit(sapply(conditions, function(c) { if(condition.table[cell, c]) c else "" })), collapse = "_"))
  }))
}

