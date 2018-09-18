#'
#' Serves as starter for the server
#' 

library(shiny)

server.port <- as.numeric(Sys.getenv("SHINY_PORT"))

if(is.na(server.port) || server.port < 1) {
  stop("No valid server port!")
}

runApp(port = server.port)