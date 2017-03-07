#
# Provides a widget that shows help entries
#

library(shiny)
library(shinyBS)

inPlaceHelpUI <- function(id, helptext = "", visible = F) {
  
  ns <- NS(id)
  
  return(tags$div(class = "in-place-help",
                  wellPanel(bsButton(ns("show"),
                                     label = "",
                                     icon = icon("info-circle"),
                                     style = "help",
                                     size = "small",
                                     type = "toggle",
                                     value = visible),
                            conditionalPanel(paste0("input['", ns("show"), "'] == true"),
                                             helpText(helptext)))))
 
}