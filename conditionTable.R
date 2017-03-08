#'
#' Functions that provide importing/generating the condition table
#' The condition table tells which conditions apply to which cell
#' The most simple condition table is the identity matrix (if a cell condition is it's name)
#'

#' Generates a condition table by separating the condition names 
#'
#' @param readcounts 
#' @param sep Separator that is applied to column names to find conditions. Set to empty string to generate a condition table only based on colum names
#'
#' @return Data table with first column
#' @export
#'
#' @examples
generateConditionTable <- function(readcounts, sep = "_") {
  
  if(is.null(readcounts)) {
    return(NULL)
  }
  
  print(sep)
  
  cells <- names(readcounts)
  result <- data.frame(row.names = cells, stringsAsFactors = F)
  
  # Go through all cells and determine which conditions apply to it
  # if the condition is not known, yet -> Create a new column
  # set the value for the corresponding cell/condition pair to true
  for(i in 1:nrow(result)) {
    
    conditions <- c()
    
    if(sep == "" || !grepl(sep, cells[i], fixed = T)) {
      conditions <- c(cells[i])
    }
    else {
      conditions <- unlist(strsplit(cells[i], sep))
    }
    
    for(cond in conditions) {

      if( ncol(result) == 1 || !(cond %in% names(result))) {

        result[[cond]] <- rep(F, nrow(result))

      }
      
      result[[cond]][i] <- T
    }
    
  }
  
  return(result)
  
}