#'
#' Contains some UI helper functions
#' 

conditionalPanel.equals <- function(input, equals) {
  return(paste0("input['", input, "'] == ", equals))
}