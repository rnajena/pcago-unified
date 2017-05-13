#'
#' Contains functions that are used for plot visualization
#' 

library(shiny)
source("helpers.R")

# Default condition for later use
condition.default <- "{default}"

# Importers for cell condition mappings
supportedCellConditionImporters <- list(
  ImporterEntry(name = "conditions_factor_csv", label = "Condititions treatments CSV"),
  ImporterEntry(name = "conditions_factor_tsv", label = "Conditions treatments TSV"),
  ImporterEntry(name = "conditions_boolean_csv", label = "Conditions boolean CSV"),
  ImporterEntry(name = "conditions_boolean_tsv", label = "Conditions boolean TSV")
)
availableCellConditionSamples <- list(
  ImporterEntry(name = "conditions.vitamins.large.csv", label = "Conditions for Vitamins (Large)")
)
supportedCellConditionGenerators <- list()



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
importCellConditions.Boolean <- function(filehandle, sep, cells) {
  
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
  
  return(data)
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
importCellConditions.Factor <- function(filehandle, sep, cells) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(cells)) {
    stop("Invalid arguments!")
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Cell condition table is empty!")
  }
  if(!setequal(rownames(data), cells)) {
    stop("Data does not assign conditions to all cells!")
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
  
  return(output)
  
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
importCellConditions <- function(filehandle, datatype, cells) {
  
  if(datatype == "boolean_csv") {
    return(importCellConditions.Boolean(filehandle, ",", cells))
  }
  else if(datatype == "boolean_tsv") {
    return(importCellConditions.Boolean(filehandle, "", cells))
  }
  else if(datatype == "factor_csv") {
    return(importCellConditions.Factor(filehandle, ",", cells))
  }
  else if(datatype == "factor_tsv") {
    return(importCellConditions.Factor(filehandle, "", cells))
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
importCellConditionsSample <- function(sample, cells) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  data <- importCellConditions(con, "factor_csv", cells)
  
  return(data)
  
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