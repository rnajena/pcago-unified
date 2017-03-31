PCAGO
================

Online tool to perform PCA on RNASeq results.

Requirements
------------

PCAGO was developed on R version 3.3.3 (2017-03-06) and also requires following packages to run:

| Package       | URL                                                                                                     | Min. version |
|:--------------|:--------------------------------------------------------------------------------------------------------|:-------------|
| knitr         | <http://yihui.name/knitr/>                                                                              | 1.15.1       |
| rmarkdown     | <http://rmarkdown.rstudio.com>                                                                          | 1.4          |
| BiocInstaller |                                                                                                         | 1.24.0       |
| VennDiagram   |                                                                                                         | 1.6.17       |
| futile.logger |                                                                                                         | 1.4.3        |
| dplyr         | <https://github.com/hadley/dplyr>                                                                       | 0.5.0        |
| ggplot2       | <http://ggplot2.tidyverse.org>, <https://github.com/tidyverse/ggplot2>                                  | 2.2.1        |
| scatterplot3d |                                                                                                         | 0.3-38       |
| Cairo         | <http://www.rforge.net/Cairo/>                                                                          | 1.5-9        |
| stringi       | <http://www.gagolewski.com/software/stringi/>, <http://site.icu-project.org/> <http://www.unicode.org/> | 1.1.3        |
| reshape2      | <https://github.com/hadley/reshape>                                                                     | 1.4.2        |
| RColorBrewer  |                                                                                                         | 1.1-2        |
| colourpicker  | <https://github.com/daattali/colourpicker>                                                              | 0.3          |
| GO.db         |                                                                                                         | 3.4.0        |
| AnnotationDbi |                                                                                                         | 1.36.2       |
| Biobase       |                                                                                                         | 2.34.0       |
| rtracklayer   |                                                                                                         | 1.34.2       |
| GenomicRanges |                                                                                                         | 1.26.4       |
| GenomeInfoDb  |                                                                                                         | 1.10.3       |
| IRanges       |                                                                                                         | 2.8.2        |
| S4Vectors     |                                                                                                         | 0.12.2       |
| BiocGenerics  |                                                                                                         | 0.20.0       |
| matrixStats   | <https://github.com/HenrikBengtsson/matrixStats>                                                        | 0.51.0       |
| shinyjs       | <http://deanattali.com/shinyjs>                                                                         | 0.9          |
| shinyBS       | <https://ebailey78.github.io/shinyBS>                                                                   | 0.61         |
| DT            | <http://rstudio.github.io/DT>                                                                           | 0.2          |
| shiny         | <http://shiny.rstudio.com>                                                                              | 1.0.0        |
| biomaRt       |                                                                                                         | 2.30.0       |

Use following command to install all required packages:

    # CRAN packages
    install.packages(c("knitr", "rmarkdown", "VennDiagram", "futile.logger", "dplyr", "ggplot2", "scatterplot3d", "Cairo", "stringi", "reshape2", "RColorBrewer", "colourpicker", "matrixStats", "shinyjs", "shinyBS", "DT", "shiny"))

    # BioConductor packages
    source("https://bioconductor.org/biocLite.R")
    biocLite(c("BiocInstaller", "GO.db", "AnnotationDbi", "Biobase", "rtracklayer", "GenomicRanges", "GenomeInfoDb", "IRanges", "S4Vectors", "BiocGenerics", "biomaRt"), type = "source")

Credits
-------

Uses code from <https://github.com/daattali/advanced-shiny> by Dean Attali. Licensed under the MIT license.
