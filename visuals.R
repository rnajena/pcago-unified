#'
#' Contains functions that are used for plot visualization
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
  
  # Order condition by variance
  result <- result[,order(colVars(data.matrix(result)), decreasing = T)]
  
  return(result)
  
}

serverGetConditionTable <- function(input, readcounts.normalized) {
  
  validate(need(readcounts.normalized(), "Cannot get condition table without read counts!"))
  
  if(input$pca.data.conditions.mode == "column") {
    return(generateConditionTable(readcounts.normalized(), sep = ""))
  }
  else if(input$pca.data.conditions.mode == "extract") {
    return(generateConditionTable(readcounts.normalized(), sep = input$pca.data.conditions.separator))
  }
  else if(input$pca.data.conditions.mode == "upload") {
    return(NULL) #todo
  }
  else {
    return(NULL)
  }
}

#' Builds a condition visuals table that contains a color and a shape for each condition.
#'
#' @param input 
#' @param conditions List of condition names
#'
#' @return Data frame with columns for color ("color") and shape ("shape") where rows are the conditions
#' @export
#'
#' @examples
generateDefaultConditionVisualsTable <- function(conditions) {
  
  validate(need(conditions, "Need list of conditions to build visual table!"))
  
  #todo loading/saving of this table
  
  return(data.frame(
    row.names = conditions,
    color = colorRampPalette(brewer.pal(9, "Set1"))(length(conditions)),
    shape = rep(-1, length(conditions)),
    stringsAsFactors = F
  ))
  
}

serverGetCellVisualsTable <- function(input, readcounts.normalized, conditions, conditions.visuals.table) {
  
  validate(
    need(readcounts.normalized(), "No data to build visual parameter table from!"),
    need(conditions(), "No conditions for visual mapping!"),
    need(conditions.visuals.table(), "No condition visual mapping!")
  )
  
  cells <- colnames(readcounts.normalized())
  cells.conditions <- conditions()
  conditions.mapping <- conditions.visuals.table()
  
  # Setup output
  factors <- data.frame(row.names = cells,
                        color = rep("#000000", length(cells)),
                        shape = rep(16, length(cells)),
                        stringsAsFactors = F)
  
  palette.colors <- c()
  palette.colors.conditions <- c()
  palette.shapes <- c()
  palette.shapes.conditions <- c()
  
  # Go through each cell and select the color & shape based on the first condition providing it
  for(cell in cells) {
    
    color <- ""
    color.condition <- ""
    shape <- -1
    shape.condition <- ""
    
    for(condition in colnames(cells.conditions)) {
      
      if(!cells.conditions[cell, condition]) {
        next()
      }
      
      if(color == "") {
        
        mapping.color <- conditions.mapping[condition, "color"]
        
        color <- mapping.color
        color.condition <- condition
      }
      
      if(shape == -1) {
        shape <- conditions.mapping[condition, "shape"]
        shape.condition <- condition
      }
      
    }
    
    if(color == "") {
      color = "#000000"
      color.condition <- "Default"
    }
    if(shape == -1) {
      shape = 16
      shape.condition <- "Default"
    }
    
    factors[cell, "color"] <- color.condition # todo: user name for condition
    factors[cell, "shape"] <- shape.condition
    
    if(!(color.condition %in% palette.colors.conditions)) { 
      
      palette.colors <- c(palette.colors, color) 
      palette.colors.conditions <- c(palette.colors.conditions, color.condition)
    }
    if(!(shape.condition %in% palette.shapes.conditions)) {
      
      palette.shapes <- c(palette.shapes, shape) 
      palette.shapes.conditions <- c(palette.shapes.conditions, shape.condition)
    }
    
  }
  
  #Convert to factors
  factors$color <- factor(factors$color, levels = palette.colors.conditions)
  factors$shape <- factor(factors$shape, levels = palette.shapes.conditions)
  
  print(factors)
  print(palette.colors)
  print(palette.shapes)
  
  return(list("factors" = factors, "palette.colors" = palette.colors, "palette.shapes" = palette.shapes))
  
}