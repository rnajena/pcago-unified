#'
#' Provides a widget that a help tooltip on hovering
#' Also provides additional methods like creating a text element with help
#'

library(shiny)
library(shinyBS)

#' Creates a text element that shows a help tooltip if hovered
#'
#' @param text Text to display
#' @param helptext The content of the help text
#' @param title The content of the help text
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
helpText <- function(text, helptext, title = "Info") {
  
  return(tags$a(href = "#", 
                "data-toggle"="popover", 
                "data-trigger" = "hover",
                title = title,
                "data-content" = helptext,
                text))
  
}

#' Creates an UI element that shows a help text on hovering the icon
#'
#' @param helptext The content of the help text
#' @param title The title of the help text
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
helpIcon <- function(helptext, title = "Info") {
  
  return(helpText(icon("info-circle"),
                  helptext,
                  title))
 
}

#' Creates an UI element consisting of text and and a help icon next to it
#'
#' @param text Text left to the help icon
#' @param helptext The content of the help text
#' @param title The title of the help text
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
helpIconText <- function(text, helptext, title = "Info") {
  
  return(tags$span( tags$span(text), helpIcon(helptext, title) ))
  
}


