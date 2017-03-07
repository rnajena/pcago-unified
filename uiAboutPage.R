#
# Contains the about page (start page)
#

library(shiny)

#' Creates the about page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiAboutPage <- function() {
  return(fluidPage(
    titlePanel("About PCAGo"),
    
    verticalLayout(
      p("PCAGo is a tool to cluster cells based on RNASeq read counts."),
      actionButton("aboutPageGotoPCA", "Go!")
    )
  ))
}