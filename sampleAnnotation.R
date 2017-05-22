#'
#' Contains functions that are used for plot visualization
#' 

library(shiny)
source("helpers.R")
source("classImporterEntry.R")
source("classSampleAnnotation.R")

# Default condition for later use
condition.default <- "{default}"

# Importers for sample condition mappings

supportedSampleAnnotationImporters.imported_data.conditions <- ImporterParameter(name = "imported_data",
                                                                                 label = "Imported data",
                                                                                 type = "checkboxes",
                                                                                 checkboxes.options = c("Conditions" = "conditions"),
                                                                                 checkboxes.selected = c("Conditions" = "conditions"))
supportedSampleAnnotationImporters.imported_data.sample_info <- ImporterParameter(name = "imported_data",
                                                                                 label = "Imported data",
                                                                                 type = "checkboxes",
                                                                                 checkboxes.options = c("Mean fragment lengths" = "meanfragmentlength"),
                                                                                 checkboxes.selected = c("Mean fragment lengths" = "meanfragmentlength"))

supportedSampleAnnotationImporters <- list(
  ImporterEntry(name = "conditions_factor_csv", 
                label = "Sample conditions factors table (*.csv)", 
                parameters = list(ImporterParameter.csv, supportedSampleAnnotationImporters.imported_data.conditions)),
  ImporterEntry(name = "conditions_boolean_csv", 
                label = "Sample conditions boolean table (*.csv)",
                parameters = list(ImporterParameter.csv, supportedSampleAnnotationImporters.imported_data.conditions)),
  ImporterEntry(name = "sample_info_csv", 
                label = "Sample info table (*.csv)",
                parameters = list(ImporterParameter.csv, supportedSampleAnnotationImporters.imported_data.sample_info))
)
availableSampleAnnotationSamples <- list(
  ImporterEntry(name = "conditions.vitamins.large.csv", 
                label = "Conditions for Vitamins (Large)",
                parameters = list(supportedSampleAnnotationImporters.imported_data.conditions)),
  ImporterEntry(name = "sample.annotation.vitamins.csv", 
                label = "Sample info for Vitamins",
                parameters = list(supportedSampleAnnotationImporters.imported_data.sample_info))
)
supportedSampleAnnotationGenerators <- list(
  ImporterEntry(name = "conditions_split", label = "Conditions from sample names", parameters = list(
    supportedSampleAnnotationImporters.imported_data.conditions,
    ImporterParameter(name = "separator", label = "Separator", type = "lineedit", lineedit.default = "_")
  ))
)



#' Imports sample condition assignments from filehandle
#' This imports Boolean condition assignments (which assigns True/False to whether a sample has given conditions)
#' 
#' @param filehandle Either a filename or a connection
#' @param datatype One value in supportedSampleConditionFileTypes
#' @param samples Vector of sample names that have to be explained by the table
#'
#' @return Data frame containing the read data
#' @export
#'
#' @examples
importSampleAnnotation.Conditions.Boolean <- function(filehandle, sep, samples, imported_data) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(samples)) {
    stop("Invalid arguments!")
  }
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }

  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Sample condition table is empty!")
  }
  if(!all(apply(data, 1, function(x) { is.logical(x) }))) {
    stop("Sample condition table is not entirely boolean!")
  }
  if(length(setdiff(samples, rownames(data))) > 0) {
    stop("Data does not assign conditions to all samples!")
  }
  if(any(grepl("^", colnames(data), fixed = T))) {
    stop("Special character ^ in conditions! This is not allowed!")
  }
    
  data <- data[samples,,drop=F]
  
  return(SampleAnnotation(conditions = data))
}

#' Imports sample condition assignments from filehandle
#' This imports the data from a Factor table.
#' The imported table contains strings that represent the condition
#' according to the column (e.g. Column says "Applied Vitamin"; Rows contain "Vitamin A", ...)
#' 
#' This function will convert the data into the boolean representation used by the
#' other functions
#'
#' @param filehandle 
#' @param sep 
#' @param samples 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotation.Conditions.Factor <- function(filehandle, sep, samples, imported_data) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(samples)) {
    stop("Invalid arguments!")
  }
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Sample condition table is empty!")
  }
  if(length(setdiff(samples, rownames(data))) > 0) {
    stop("Data does not assign conditions to all samples!")
  }
  if(any(grepl("^", colnames(data), fixed = T))) {
    stop("Special character ^ in conditions! This is not allowed!")
  }
  
  data <- data[samples,,drop=F]
  
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
  
  return(SampleAnnotation(conditions = output))
  
}

#' Imports sample info annotation (e.g. mean fragment length) from CSV
#'
#' @param filehandle 
#' @param sep 
#' @param samples 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotation.SampleInfo <- function(filehandle, sep, samples, imported_data) {
  
  if(missing(filehandle) || !is.character(sep) || !is.character(samples)) {
    stop("Invalid arguments!")
  }
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  data <- read.csv(filehandle, sep = sep, row.names = 1, stringsAsFactors = F)
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    stop("Sample info table is empty!")
  }
  if(length(intersect(rownames(data), samples)) == 0) {
    stop("Sample info table does not annotate even one sample!")
  }
  if(!setequal(colnames(data), c("meanfragmentlength"))) {
    stop("Sample info table is missing columns!")
  }
  
  # Restrict to set of samples by parameter
  data <- data[samples,,drop=F]
  
  return(SampleAnnotation(sample.info = data))
  
}

#' Imports sample condition assignments from filehandle with importer definded by datatype
#'
#' @param filehandle 
#' @param datatype 
#' @param samples 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotation <- function(filehandle, datatype, samples, parameters) {
  
  if(datatype == "conditions_boolean_csv") {
    return(importSampleAnnotation.Conditions.Boolean(filehandle, parameters$separator, samples, parameters$imported_data))
  }
  else if(datatype == "conditions_factor_csv") {
    return(importSampleAnnotation.Conditions.Factor(filehandle, parameters$separator, samples, parameters$imported_data))
  }
  else if(datatype == "sample_info_csv") {
    return(importSampleAnnotation.SampleInfo(filehandle, parameters$separator, samples, parameters$imported_data))
  }
  else {
    stop(paste("Unknown importer", datatype))
  }
  
}

#' Imports sample condition assignments from sample
#'
#' @param sample 
#' @param samples 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotationSample <- function(sample, samples, parameters) {
  
  if(!is.character(sample)) {
    stop("Invalid arguments!")
  }
  
  con <- file(paste0("sampledata/", sample), "r")
  on.exit({ close(con) })
  
  if(sample == "sample.annotation.vitamins.csv") {
    parameters$separator <- ","
    data <- importSampleAnnotation(con, "sample_info_csv", samples, parameters)
  }
  else if(sample == "conditions.vitamins.large.csv") {
    parameters$separator <- ","
    data <- importSampleAnnotation(con, "conditions_factor_csv", samples, parameters)
  }
  else {
    stop(paste("Unknown sample", sample))
  }
  
  
  return(data)
  
}

#' Generates sample condonditions assignment by splitting the sample names
#'
#' @param samples 
#' @param sep 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotationFromGenerator.Conditions.SplitSampleNames <- function(samples, sep, imported_data) {
  
  if(length(imported_data) == 0) {
    stop("No data to be imported selected!")
  }
  
  result <- data.frame(row.names = samples, stringsAsFactors = F)
  
  # Go through all samples and determine which conditions apply to it
  # if the condition is not known, yet -> Create a new column
  # set the value for the corresponding sample/condition pair to true
  for(i in 1:nrow(result)) {
    
    conditions <- c()
    
    if(sep == "" || !grepl(sep, samples[i], fixed = T)) {
      conditions <- c(samples[i])
    }
    else {
      conditions <- unlist(strsplit(samples[i], sep))
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
  
  return(SampleAnnotation(conditions = result))
}

#' Imports sample condition assignments from generator
#'
#' @param sample 
#' @param samples 
#'
#' @return
#' @export
#'
#' @examples
importSampleAnnotationFromGenerator <- function(generator, samples, parameters) {
  
  if(!is.character(generator)) {
    stop("Invalid arguments!")
  }
  
  if(generator == "conditions_split") {
    return(importSampleAnnotationFromGenerator.Conditions.SplitSampleNames(samples, parameters$separator, parameters$imported_data))
  }
  else {
    stop(paste("Unknown generator", datatype))
  }
  
}

#' Collapses the conditions for each sample in the condition table
#' into a string.
#'
#' @param condition.table Table that determines if a sample has a condition
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
  
  return(sapply(rownames(condition.table), function(sample) {
    return(paste(na.omit(sapply(conditions, function(c) { if(condition.table[sample, c]) c else "" })), collapse = "^"))
  }))
}

