#'
#' Some general helpers
#' 

library(shiny)

#' Returns logical vector indicating for each input element if they are a valid color
#'
#' @param x Vector of character strings
#'
#' @return Logical vector determining if the corresponding string is a valid color
#' @export
#'
#' @examples
isColor <- function(x) {
  
  if(is.character(x)) {
    stop("Invalid arguments!")
  }
  
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}

#' Custom implementation of withProgress
#' 
#' callback function: updateProgress(detail = NULL, value = NULL)
#' Updates the progress with description of current task (detail) and the progress (value; numeric in 0 ... 1) 
#'
#' @param expr Function that takes 1 parameter (callback function)
#' @param message 
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