#'
#' Some general helpers
#' 

library(shiny)
library(reshape2)
library(SummarizedExperiment)
library(Cairo)
library(parallel)
library(tools)
source("classImporterParameter.R")

#' Returns TRUE if all data in x are integers
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
is.integer <- function(x) {
  return(all(x == floor(x)))
}

#' Checks if object is a SummarizedExperiment
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
is.SummarizedExperiment <- function(object) {
  return(is(object, "SummarizedExperiment"))
}

#' Checks if value is a valid choice
#'
#' @param value 
#' @param choices 
#'
#' @return
#' @export
#'
#' @examples
enum.contains <- function(value, choices) {
  return(value %in% choices)
}

#' Generates a random string
#' Src: http://stackoverflow.com/questions/42734547/generating-random-strings
#'
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
randomString <- function(n = 1, k = 5) {
  a <- do.call(paste0, replicate(k, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

#' Returns logical vector indicating for each input element if they are a valid color
#'
#' @param x Vector of character strings
#'
#' @return Logical vector determining if the corresponding string is a valid color
#' @export
#'
#' @examples
isColor <- function(x) {
  
  if(!is.character(x)) {
    stop("Invalid arguments!")
  }
  if(length(x) == 0) {
    return(T)
  }

  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

#' Reads a data frame and extracts labels for the columns from them.
#' Columns that have the form "colum=Custom label" are seen as labeled columns.
#' This function will reshape the data frame to only contain the actual column name (here "column")
#' and return the labels
#' If no custom label is provided, the label is the column name
#'
#' @param data 
#'
#' @return List with data frame with correct column names ($data) and named vector of column labels ($labels).
#' @export
#'
#' @examples
data.frame.labels = function(data) {
  
  col <- colsplit(colnames(data), "=", c("id", "label"))
  col.labels <- apply(col, 1, function(x) { if(is.na(x[["label"]])) x[["id"]] else x[["label"]] })
  names(col.labels) <- col$id
  
  output <- list()
  output$data <- data
  colnames(output$data) <- col$id
  output$labels <- col.labels
  
  return(output)
  
}

#' Custom implementation of withProgress
#' 
#' callback function: updateProgress(detail = NULL, value = NULL)
#' Updates the progress with description of current task (detail) and the progress (value; numeric in 0 ... 1) 
#'
#' @param expr Function that takes 1 parameter (callback function)
#' @param message Message of the progress bar
#'
#' @return
#' @export
#'
#' @examples
withProgressCustom <- function(expr, message) {
  
  progress <- shiny::Progress$new()
  on.exit({
    progress$close()
  })
  
  progress$set(message = message, value = 0)
  
  # Status callback function
  updateProgress <- function(detail = NULL, value = NULL) {
    progress$set(value = value, detail = detail)
  }
  
  return(expr(updateProgress))
  
}

#' Runs expr in parallel environment
#'
#' @param expr Expression to run
#' @param exprsuccess Expression to run if the task was successful (not canceled)
#' @param exprfailed Expression to run if the task was unsuccessful (canceled)
#' @param exprfinally Expression to run after parallel
#'
#' @return
#' @export
#'
#' @examples
withParallel <- function(session, input, expr, exprsuccess = function() {}, exprfailed = function() {}, exprfinally = function(){}, withProgress = F, message = "The calculation is currently running. Please wait.") {
 
  vars <- reactiveValues(status = "calculating")
  
  showModal(modalDialog(
    iconText(icon("circle-o-notch", class = "fa-spin"), message),
    footer = tagList(
      actionButton(session$ns("parallel.cancel"), "Cancel")
    )
  ))
  
  # Start the task
  process <- mcparallel(expr)
  
  # User can click cancel
  observeEvent(input$parallel.cancel, {
    
    vars$status <- "canceled"
    pskill(process$pid)
    removeModal()
    exprfailed()
    exprfinally()
    
  })
  
  # Always check every 1s
  observe({
    
    process.result <- mccollect(process, wait = F)
    
    if(!is.null(process.result)) {
      vars$status <- "finished"
      removeModal()
      exprsuccess()
      exprfinally()
    }
    else if(isolate(vars$status) == "calculating") {
      invalidateLater(1000)
    }
  })
   
}

#' Creates a notification that indicates progress without a progress bar
#' Server code must close this notification manually!
#'
#' @param id Id of the notification if NULL, an ID will be generated
#' @param message 
#'
#' @return ID of the notification
#' @export
#'
#' @examples
progressNotification <- function(message, id = NULL) {
  
  if(is.null(id)) {
    id <- stringi::stri_rand_strings(1, 16)
  }
  
  showNotification(
    ui = tags$span(icon("circle-o-notch", class = "fa-spin"), message),
    id = id,
    closeButton = F,
    type = "default",
    duration = NULL
  )
  
  return(id)
}


#' Updates an existing progress notification
#'
#' @param id 
#' @param message 
#'
#' @return
#' @export
#'
#' @examples
updateProgressNotification <- function(id, message) {
  showNotification(
    ui = tags$span(icon("circle-o-notch", class = "fa-spin"), message),
    id = id,
    closeButton = F,
    type = "default",
    duration = NULL
  )
}



#' Runs an expression with progress notification
#'
#' @param message 
#' @param expr 
#'
#' @return
#' @export
#'
#' @examples
withProgressNotification <- function(message, expr) {
  
  id <- randomString(n = 1, k = 16)
  on.exit({
    removeNotification(id = id)
  })
  
  progressNotification(id = id, message = message)
  return(expr())
  
}

#' Returns input if it isn't empty or NULL
#'
#' @param input 
#' @param default 
#'
#' @return
#' @export
#'
#' @examples
getOrDefault.character <- function(input, default) {
  return(if(is.null(input) || length(input) == 0) input else default)
}

#' Saves a plot using R default plotting system
#'
#' @param width 
#' @param height 
#' @param dpi 
#' @param format 
#' @param expr 
#'
#' @return
#' @export
#'
#' @examples
saveRPlot <- function(width, height, dpi, scale, filename, format, expr) {
  
  if(!is.numeric(width) || !is.numeric(height) || !is.numeric(dpi) || !is.numeric(scale)
     || !is.character(filename) || !is.character(format) || missing(expr)) {
    stop("Invalid arguments!")
  } 
  
  if(format == "svg") {
    CairoSVG(file = filename,
        width = width / dpi,
        height = height / dpi,
        pointsize = 12 * scale)
  }
  else if(format == "pdf") {
    CairoPDF(file = filename,
        width = width / dpi,
        height = height / dpi,
        pointsize = 12 * scale)
  }
  else if(format == "png") {
    CairoPNG(filename = filename,
        width = width,
        height = height,
        pointsize = 12 * scale,
        units = "px",
        res = dpi)
  }
  else if(format == "tiff") {
    CairoTIFF(filename = filename,
         width = width,
         height = height,
         pointsize = 12 * scale,
         units = "px",
         res = dpi)
  }
  
  expr()
  
  dev.off()
  
}

#' A importer parameter that handles CSV separators
ImporterParameter.csv <- ImporterParameter(name = "separator",
                                           label = "CSV Separator",
                                           type = "select",
                                           select.values = c(
                                             "Comma" = ",",
                                             "Semicolon" = ";",
                                             "Tab" = "\t",
                                             "Whitespace" = " "
                                           ))
