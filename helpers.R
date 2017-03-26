#'
#' Some general helpers
#' 

library(shiny)
library(reshape2)

#' Returns logical vector indicating for each input element if they are a valid color
#'
#' @param x Vector of character strings
#'
#' @return Logical vector determining if the corresponding string is a valid color
#' @export
#'
#' @examples
isColor <- function(x) {
  
  if(!is.character(x)) {
    stop("Invalid arguments!")
  }
  if(length(x) == 0) {
    return(T)
  }

  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

#' Reads a data frame and extracts labels for the columns from them.
#' Columns that have the form "colum=Custom label" are seen as labeled columns.
#' This function will reshape the data frame to only contain the actual column name (here "column")
#' and return the labels
#' If no custom label is provided, the label is the column name
#'
#' @param data 
#'
#' @return List with data frame with correct column names ($data) and named vector of column labels ($labels).
#' @export
#'
#' @examples
data.frame.labels = function(data) {
  
  col <- colsplit(colnames(data), "=", c("id", "label"))
  col.labels <- apply(col, 1, function(x) { if(is.na(x[["label"]])) x[["id"]] else x[["label"]] })
  names(col.labels) <- col$id
  
  output <- list()
  output$data <- data
  colnames(output$data) <- col$id
  output$labels <- col.labels
  
  return(output)
  
}

#' Custom implementation of withProgress
#' 
#' callback function: updateProgress(detail = NULL, value = NULL)
#' Updates the progress with description of current task (detail) and the progress (value; numeric in 0 ... 1) 
#'
#' @param expr Function that takes 1 parameter (callback function)
#' @param message Message of the progress bar
#'
#' @return
#' @export
#'
#' @examples
withProgressCustom <- function(expr, message) {
  
  progress <- shiny::Progress$new()
  on.exit({
    progress$close()
  })
  
  progress$set(message = message, value = 0)
  
  # Status callback function
  updateProgress <- function(detail = NULL, value = NULL) {
    progress$set(value = value, detail = detail)
  }
  
  return(expr(updateProgress))
  
}

#' Returns input if it isn't empty or NULL
#'
#' @param input 
#' @param default 
#'
#' @return
#' @export
#'
#' @examples
getOrDefault.character <- function(input, default) {
  return(if(is.null(input) || length(input) == 0) input else default)
}

#' Helper function that creates a list of plot settings
#'
#' @param width 
#' @param height 
#' @param dpi 
#' @param format 
#' @param title 
#' @param subtitle 
#' @param legend.color 
#' @param legend.shape 
#'
#' @return
#' @export
#'
#' @examples
plotSettings <- function(width, height, dpi, title = "", subtitle = "", legend.color = "", legend.shape = "") {
  
  if(!is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) 
     || !is.character(title) || !is.character(subtitle) || !is.character(legend.color) || !is.character(legend.shape)) {
    stop("Invalid arguments!")
  }
  
  return(list(
    width = width,
    height = height,
    dpi = dpi,
    title = title,
    subtitle = subtitle,
    legend.color = legend.color,
    legend.shape = legend.shape
  ))
  
}
