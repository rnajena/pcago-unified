#'
#' Global definitions, loaded by Shiny before ui.R and server.R
#'
#' Keep Shiny validation errors intact when they pass through S4 generics.
#'
#' Bioconductor (BiocGenerics, S4Vectors, ...) turns many base functions such as colnames(),
#' nrow(), intersect() or lapply() into S4 generics. When an argument of such a generic is a
#' reactive that stops with validate(need(...)), S4 method dispatch catches the error and
#' re-raises it as a plain simpleError ("error in evaluating the argument 'x' in selecting a
#' method for function ..."). The error then no longer inherits from "shiny.silent.error",
#' and Shiny closes the whole session ("Disconnected from the server") instead of showing
#' the validation message.
#'
#' The wrappers below force all arguments *before* calling the generic, so a validation error
#' is raised outside of S4 dispatch and keeps its class. The original promises are passed on,
#' so argument names and substitute()-based behaviour of the generics are unchanged.
#' Only the app's own code (sourced into the global environment) uses these wrappers;
#' package code is not affected.
#'

pcago.generics.forced <- c(
  "%in%", "anyDuplicated", "append", "as.data.frame", "as.factor", "as.list", "as.matrix",
  "basename", "cbind", "colMeans", "colnames", "colSums", "diff", "dirname", "do.call",
  "droplevels", "duplicated", "Filter", "Find", "format", "grep", "grepl", "gsub", "intersect",
  "is.unsorted", "lapply", "levels", "Map", "mapply", "match", "mean", "merge", "nchar", "ncol",
  "NCOL", "nrow", "NROW", "order", "paste", "pmax", "pmin", "Position", "rank", "rbind", "Reduce",
  "rev", "rowMeans", "rownames", "rowSums", "sapply", "setdiff", "setequal", "sort", "split",
  "startsWith", "endsWith", "sub", "substr", "substring", "summary", "t", "table", "tapply",
  "tolower", "toupper", "union", "unique", "unname", "which", "which.max", "which.min"
)

pcago.forcing.wrapper <- function(name) {
  force(name)
  function(...) {
    list(...) # Force all arguments here, outside of S4 dispatch
    # Resolve the function behind the global environment (S4 generic from an attached package or base)
    fun <- get(name, envir = parent.env(globalenv()), mode = "function")
    fun(...)
  }
}

for (pcago.generic.name in pcago.generics.forced) {
  assign(pcago.generic.name, pcago.forcing.wrapper(pcago.generic.name), envir = globalenv())
}
rm(pcago.generic.name)

#' The app's own S4 generics (setGeneric in class*.R) have the same problem. They cannot be
#' wrapped before all setMethod() calls are done, so this is called at the start of each
#' session (server.R), when all files have been sourced. The generic objects are kept in
#' pcago.app.generics; calling it repeatedly is safe.
pcago.app.generics <- new.env()

pcago.wrap.app.generics <- function() {
  for (name in ls(globalenv())) {
    obj <- get(name, envir = globalenv())
    # genericFunction covers standard and nonstandard generics (setGeneric with valueClass etc.)
    if (is(obj, "genericFunction")) {
      assign(name, obj, envir = pcago.app.generics)
      assign(name, local({
        generic.name <- name
        function(...) {
          list(...) # Force all arguments here, outside of S4 dispatch
          get(generic.name, envir = pcago.app.generics)(...)
        }
      }), envir = globalenv())
    }
  }
}
