
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(ggplot2)
library(plotly)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
source("uiAboutPage.R")
source("uiPCAPage.R")
source("uiHelpPage.R")

shinyUI(fluidPage(useShinyjs(),
                  list(tags$head(HTML('<link rel="icon", href="icon.png",
                                   type="image/png" />'))),
                  div(style="padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title="", windowTitle="PCAGO"
                      )
                  ),
                 navbarPage(img(src = "logo.svg", class = "header-logo"),
                   tabPanel("About", uiAboutPage()),
                   tabPanel("Analyze", uiPCAPage()),
                   tabPanel("Help", uiHelpPage()),
                   theme = "style.css")))

