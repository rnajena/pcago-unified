#'
#' Contains the ui definition of the about page (start page)
#'

library(shiny)
library(shinyBS)
source("uiHelper.R")
source("environment.R")

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
             tags$div(class = "landing-header-action",
                      tags$div(style="display: inline-block; width: 80%;", includeMarkdown("aboutpage/landing.md")),
                      tags$div(style="display: block;",bsButton("about.goto.analyze", "Start  analysis!", style = "primary", size = "large")),
                      tags$div(style="display: block; margin-top: 5px;", actionLink("about.goto.help", "or learn how to use it!"))
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
             )),
    tags$div(class = "footer",
             paste("PCAGO version", pcago.version)))
  )
}