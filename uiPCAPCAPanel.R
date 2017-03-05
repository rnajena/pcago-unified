#
# Contains definition of PCA settings panel
#

library(shiny)
library(shinyBS)

uiPCAPCAPanel <- function() {
  return(bsCollapse(
    bsCollapsePanel("Gene set"),
    bsCollapsePanel("Gene count",
                    sliderInput("pca.pca.genes.count", "Gene count", min = 0, max = 0, value = 0, step = 1))
  ))
}