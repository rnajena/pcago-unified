#
# Functions that provide importing/generating the condition table
# The condition table tells which conditions apply to which cell
#

#' Generates a condition table by separating the condition names 
#'
#' @param readcounts 
#' @param sep 
#'
#' @return Data table with first column
#' @export
#'
#' @examples
generateConditionTable <- function(readcounts, sep = "_") {
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  cells <- names(readcounts)
  result <- data.frame(row.names = cells, stringsAsFactors = F)
  
  for(i in 1:nrow(result)) {
    
    conditions <- unlist(strsplit(cells[i], sep))

    for(cond in conditions) {

      if( ncol(result) == 1 || !(cond %in% names(result))) {

        result[[cond]] <- rep(F, nrow(result))

      }
      
      result[[cond]][i] <- T
    }
    
  }
  
  return(result)
  
}