
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(markdown)
library(htmltools)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(colourpicker)

source("uiAboutPage.R")
source("uiPCAPage.R")
source("uiHelpPage.R")
source("uiLegalPage.R")

shinyUI(tags$div(useShinyjs(),
                  list(tags$head(HTML('<link rel="icon", href="icon.png",
                                   type="image/png" />'))),
                  div(style="padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title="", windowTitle="PCAGO"
                      )
                  ),
                 navbarPage(img(src = "logo.svg", class = "header-logo"),
                   tabPanel("About", value = "about", uiAboutPage()),
                   tabPanel("Analyze", value = "analyze", uiPCAPage()),
                   tabPanel("Help", value = "help", uiHelpPage()),
                   tabPanel("Impressum", value = "legal", uiLegalPage()),
                   id = "main.nav",
                   theme = "style.css"),
                 tags$script(I(includeText("scripts/globalBody.js")))))

