#'
#' Server routines handling selecting selecting top variant genes
#' Including animation
#' 

library(shiny)

#' Handles server side of selecting top variant genes
#'
#' @param input 
#' @param session 
#' @param readcounts.filtered 
#'
#' @return Reactive list with "from", "to" and "by" to indicate how animation should behave
#' @export
#'
#' @examples
serverPCATopVariantGeneCount <- function(input, session, readcounts.filtered) {
  
  observeEvent(readcounts.filtered(), {
    genes_max <- nrow(readcounts.filtered())
    updateSliderInput(session, "pca.pca.genes.count", min = 1, max = genes_max, value = genes_max)
    updateNumericInput(session, "pca.pca.genes.count.from", min = min(1, genes_max), max = genes_max, value = min(2, genes_max))
    updateNumericInput(session, "pca.pca.genes.count.to", min = min(1, genes_max), max = genes_max, value = genes_max)
  })
  
  #' 
  #' Fetch gene count parameters
  #' Usually using input$ would be sufficient,
  #' but numericInput is broken
  #' TODO: More elegant solution
  #' 
  
  pca.genes.count.from <- reactive({
    
    # Additional checks as the numeric input is broken (accepts values outside of range)
    # This bug is from 2015 (sic!) https://github.com/rstudio/shiny/issues/927
    
    genes_max <- nrow(readcounts.filtered())
    genes_min <- min(1, genes_max)
    genes_from <- input$pca.pca.genes.count.from
    genes_to <- input$pca.pca.genes.count.to
    
    validate(need(genes_max == 0 || genes_from < genes_to, "Invalid range given!"))
    
    genes_from <- max(genes_min, genes_from)
    
    return(genes_from)
  })
  
  pca.genes.count.to <- reactive({
    
    # Additional checks as the numeric input is broken (accepts values outside of range)
    # This bug is from 2015 (sic!) https://github.com/rstudio/shiny/issues/927
    
    genes_max <- nrow(readcounts.filtered())
    genes_from <- input$pca.pca.genes.count.from
    genes_to <- input$pca.pca.genes.count.to
    
    validate(need( genes_max == 0 || genes_from < genes_to, "Invalid range given!"))
    
    genes_to <- min(genes_max, genes_to)
    
    return(genes_to)
    
  })
  
  pca.genes.count.by <- reactive({
    return(max(0, input$pca.pca.genes.count.by))
  })
  
  # User clicks fine-grained controls in gene count panel
  observeEvent(input$pca.pca.genes.count.lstepdecrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current - input$pca.pca.genes.count.by)
  })
  
  observeEvent(input$pca.pca.genes.count.lstepincrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current + input$pca.pca.genes.count.by)
  })
  
  observeEvent(input$pca.pca.genes.count.stepdecrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current - 1)
  })
  
  observeEvent(input$pca.pca.genes.count.stepincrease, {
    current <- input$pca.pca.genes.count
    updateSliderInput(session, "pca.pca.genes.count", value = current + 1)
  })
  
  observeEvent(pca.genes.count.from(), {
    updateSliderInput(session, "pca.pca.genes.count", min = pca.genes.count.from())
  })
  
  observeEvent(input$pca.genes.count.to, {
    updateSliderInput(session, "pca.pca.genes.count", max = pca.genes.count.to())
  })
  
  
  
  #' Handles the animation of the gene counts
  #' This works by invalidating itself automatically if the play button is toggled
  #' 
  #' Info: There's a native animation feature in the slider. But it does not allow
  #' changing the animation parameters without renderUI; which is slow and fragile due to missing inputs
  observe({
    
    if(input$pca.pca.genes.count.animation.play) {
      # Separate the actual animation from the environment
      isolate({
        
        from <- pca.genes.count.from()
        to <- pca.genes.count.to()
        by <- input$pca.pca.genes.count.by
        current <- input$pca.pca.genes.count
        
        validate(need(from < to, "Wrong animation parameters!"))
        
        if(current == to) {
          current <- from
        }
        else if(current < to) {
          current <- min(to, current + by)
        }
        else {
          current <- from
        }
        
        updateSliderInput(session, "pca.pca.genes.count", value = current)
        
      })
      
      invalidateLater(isolate({input$pca.pca.genes.count.animation.speed}))
    }
  })
  
  return(reactive(
    return(list("count.from" = pca.genes.count.from(),
                "count.to" = pca.genes.count.to(),
                "count.by" = pca.genes.count.by()))
  ))
  
}