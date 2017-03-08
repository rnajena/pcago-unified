#'
#' Contains the ui definition of the about page (start page)
#'

library(shiny)
library(shinyBS)

#' Creates the about page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiAboutPage <- function() {
  return(fluidPage(
    titlePanel("About PCAGO"),
    
    verticalLayout(
      p("PCAGO is a tool to cluster cells based on RNASeq read counts."),
      fluidRow(style = "text-align: center;", tags$div(class = "col-lg-1 col-centered", style = "display: inline-block;", bsButton("about.goto.analyze", "Start  analysis!", style = "primary", size = "large")))
    )
  ))
}