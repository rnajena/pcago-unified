# PCAGO

## Features

* Annotations, GO terms, ...
* Normalization
* Plot output settings (width, height, dpi)
* Plot title/subtitle
* PC variances in plot
* Gene information
* Annotation: Show gene count in collected data

## Code

* Improve performance
* Insert parameter checking (missing, is.xyz) in all functions

## Bugs

* Plot sizes buggy

## Additional notes

* Use notifications for importer messages
* ggplot2 has slow performance (3x slower than scatterplot3d)
* All annotation results won't necessary contain all genes and in order!

## R Quirks

* RStudio downloads don't overwrite files correctly
* R 3.3.3 compile rtracklayer etc. from source https://support.bioconductor.org/p/93347/#93373
