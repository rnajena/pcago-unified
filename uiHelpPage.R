library(shiny)

#' Creates the help page for the UI
#'
#' @return
#' @export Object of shiny page type
#'
#' @examples
uiHelpPage <- function() {
  return(fluidPage(
    sidebarLayout(
      sidebarPanel(),
      mainPanel()
    )
  ))
}