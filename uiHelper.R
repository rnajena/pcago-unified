#'
#' Contains some UI helper functions
#'

#' Creates a string "input['<input>'] == <equals>"
#' 
#' @param input UI input name
#' @param equals Value to compare to. Note that you need to put strings into inverted commas
#' 
conditionalPanel.equals <- function(input, equals) {
  return(paste0("input['", input, "'] == ", equals))
}

headerPanel <- function( ... ,header = "") {
  
  return(tags$div(class="panel panel-default header-panel",
                  tags$div(class="panel-heading", header),
                  tags$div(class="panel-body", ...)))
  
}