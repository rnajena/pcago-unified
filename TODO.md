# PCAGO

## Features

* Help pages
* About page
* Annotation overview
* SMPFPP server implementation

## Code

* Improve performance
* Better variable names

## Bugs

* Visual editor color update sometimes not triggering

## Additional notes

* Use notifications for importer messages
* ggplot2 has slow performance (3x slower than scatterplot3d)

## R Quirks

* RStudio downloads don't overwrite files correctly
* R 3.3.3 compile rtracklayer etc. from source https://support.bioconductor.org/p/93347/#93373

## Theory/practice

### Which genes are relevant?

Already known to be useful: Gene variance as main criterion
Where to set cutoff?

* PCA transformed change rate (directly related to variance? Theory behind PCA?)
* Cluster results? (problem: PCA based clustering sucks; at least if sample PCA is done)
* Theoretical approach? Distribution of read counts? -> DeSeq/EdgeR papers!!! -> Statistical value!
* Variance cutoff caused by problems with PCA? -> Look this up! Find ACTUAL cause! -> Derive better criterium!
* SNR as criterion?
