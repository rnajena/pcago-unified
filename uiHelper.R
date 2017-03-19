#'
#' Contains some UI helper functions
#'

#' Creates a string "input['<input>'] == <equals>"
#' 
#' @param input UI input name
#' @param equals Value to compare to. Note that you need to put strings into inverted commas
#' 
conditionalPanel.equals <- function(input, equals) {
  return(paste0("input['", input, "'] == ", equals))
}

#' Creates a header panel that is not collapsable
#' Meant for static grouping
#'
#' @param ... 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
headerPanel <- function( ... ,header = "") {
  
  return(tags$div(class="panel panel-default header-panel",
                  tags$div(class="panel-heading", header),
                  tags$div(class="panel-body", ...)))
  
}

#' Creates an UI element that has text next to an icon
#'
#' @param icon A taglist containing the icon
#' @param text The text
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
iconText <- function(icon, text) {
  
  return(tags$span(icon, tags$span(text)))
  
}

#' Creates an UI element that has text next to an icon provided by icon() command
#'
#' @param faicon The name of the icon.
#' @param text The text
#' @param lib Icon provider
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
faIconText <- function(faicon, text, lib = "font-awesome") {
  
  return(iconText(icon(faicon, lib = lib), text))
  
}

#' Creates a horizontal divider UI element
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
hDivider <- function() {
  return(tags$div(class = "hdivider"))
}

