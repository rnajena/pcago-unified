
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
source("uiAboutPage.R")
source("uiPCAPage.R")
source("uiHelpPage.R")

#' Script that enables all popovers (needed for help texts)
#' Attach to bottom of body.
script.enable.popovers <- I("$(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover({
      container: 'body'
    });   
});")

shinyUI(fluidPage(useShinyjs(),
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
                   id = "navigation",
                   theme = "style.css"),
                 tags$script(script.enable.popovers)))

