library(shiny)

#' Creates the impressum page for the UI
#'
#' @return
#' @export Object of shiny page type
#'
#' @examples
uiLegalPage <- function() {
  return(navlistPanel(
    uiHelpPageEntry("Impressum", "legal/impressum.md"),
    uiHelpPageEntry("Data Privacy Policy", "legal/data_policy.md"),
    uiHelpPageEntry("Licenses", "legal/licenses.md")
  ))
}