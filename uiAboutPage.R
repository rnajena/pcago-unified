#'
#' Contains the ui definition of the about page (start page)
#'

library(shiny)
library(shinyBS)
source("uiHelper.R")

#' Creates the about page for the UI
#'
#' @return Object of shiny page type
#' @export
#'
#' @examples
uiAboutPage <- function() {
  return(tags$div(class = "about-page",
    tags$div(class="landing-header",
             tags$img(class = "landing-header-logo", src = "logo.svg"),
             tags$div(class = "container-fluid landing-header-action",
                      fluidRow(column(width = 8, offset = 2, includeMarkdown("aboutpage/landing.md"))),
                      fluidRow(column(width = 1, offset = 5, bsButton("about.goto.analyze", "Start  analysis!", style = "primary", size = "large"))))
    ))
  )
}