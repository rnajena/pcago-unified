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

#' Creates a header panel that is not collapsable
#' Meant for static grouping
#'
#' @param ... 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
headerPanel <- function( ... ,header = "") {
  
  return(tags$div(class="panel panel-default header-panel",
                  tags$div(class="panel-heading", header),
                  tags$div(class="panel-body", ...)))
  
}

#' Creates an UI element that has text next to an icon
#'
#' @param icon A taglist containing the icon
#' @param text The text
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
iconText <- function(icon, text) {
  
  return(tags$span(icon, tags$span(text)))
  
}

#' Creates an UI element that has text next to an icon provided by icon() command
#'
#' @param faicon The name of the icon.
#' @param text The text
#' @param lib Icon provider
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
faIconText <- function(faicon, text, lib = "font-awesome") {
  
  return(iconText(icon(faicon, lib = lib), text))
  
}

#' Creates a horizontal divider UI element
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
hDivider <- function() {
  return(tags$div(class = "hdivider"))
}

#' Creates a vertical skip
#'
#' @return Shiny UI element
#' @export
#'
#' @examples
vSkip <- function(h = 5) {
  return(tags$div(style = paste0("height: ", h, "px;")))
}


subSubBox <- function(...) {
  return(tags$div(class = "sub-sub-box", ...))
}

#' Joins a vector to a string with a limit
#' Use this for UI
#'
#' @param x 
#' @param sep 
#' @param limit 
#'
#' @return
#' @export
#'
#' @examples
strJoinLimited <- function(x, sep = ", ", limit = NULL) {
  
  if(is.null(limit) || length(x) <= limit) {
    return(paste(x, collapse = sep))
  }
  else {
    return(paste0(paste(x[1:limit], collapse = sep), sep, "... (", length(x), ")"))
  }
  
}

#' Creates a dropdown button for actionButtons
#'
#' @param id 
#' @param label 
#' @param content 
#' @param icon 
#' @param button.style 
#'
#' @return
#' @export
#'
#' @examples
dropdownButton <- function(id, label, content, icon = NULL, button.style = "btn-default") {
  
  wrapped <- lapply(content, function(x) { tags$li(x) })
  
  return(tags$div(class = "dropdown dropdown-button",
                  tags$button(class = paste(c("btn", button.style, "dropdown-toggle"), collapse = " "), 
                              type = "button",
                              "data-toggle" = "dropdown",
                              icon,
                              label,
                              tags$span(class = "caret")),
                  tags$ul(class = "dropdown-menu",
                          wrapped)))
}

#' Workaround for bug in shinyBS
#' Returns all collapse elements 2x (except if not) and then (as last entry) the current open one
#'
#' @param input 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
getOpenCollapseId <- function(input, id) {
  l <- input[[id]]
  t <- table(l)
  c <- l[length(l)]
  
  if(t[c] > 2 || t[c] == 1) {
    return(c)
  }
  else {
    return(NULL)
  }
  
}
