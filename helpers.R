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
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}