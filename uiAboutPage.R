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
                      fluidRow(column(width = 8, offset = 2, bsButton("about.goto.analyze", "Start  analysis!", style = "primary", size = "large"))),
                      tags$div(class="row top5", column(width = 8, offset = 2, actionLink("about.goto.help", "or learn how to use it!")))
                                      )
    ),
    tags$div(class="features", 
             fluidRow(
               column(offset = 3, width = 2, tags$div(class = "feature-column", 
                                                      tags$img(src = "aboutpage/quickfeatures_readcounts.svg"),
                                                      includeMarkdown("aboutpage/quickfeatures_readcounts.md"))),
               column(width = 2, tags$div(class = "feature-column", 
                                          tags$img(src = "aboutpage/quickfeatures_filter.svg"),
                                          includeMarkdown("aboutpage/quickfeatures_filter.md"))),
               column(width = 2, tags$div(class = "feature-column", 
                                          tags$img(src = "aboutpage/quickfeatures_pca.svg"),
                                          includeMarkdown("aboutpage/quickfeatures_pca.md")))
             )))
  )
}