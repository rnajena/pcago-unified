#'
#' Provides a widget that a help tooltip on hovering
#' Also provides additional methods like creating a text element with help
#'

library(shiny)
library(shinyBS)

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
  
  return(tags$a(href = "#", 
                "data-toggle"="popover", 
                "data-trigger" = "hover",
                title = title,
                "data-content" = helptext,
                icon("info-circle")))
 
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