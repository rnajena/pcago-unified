# PCAGO

## Features

* Normalization ; DeSeq -> Need SummarizedExperiment!
* Help pages
* Read count normalization result plots
* Custom selectors
* Annotation overview
* Better processing view
* SMPFPP server implementation
* Use SummarizedExperiment instead of read count data frame + sequence info
* TPM normalization: Processing steps should give info!

## Theory/practice

### Which genes are relevant?

Already known to be useful: Gene variance as main criterium
Where to set cutoff?

* PCA transformed change rate (directly related to variance? Theory behind PCA?)
* Cluster results? (problem: PCA based clustering sucks; at least if cell PCA is done)
* Theoretical approach? Distribution of read counts? -> DeSeq/EdgeR papers!!! -> Statistical value!
* Variance cutoff caused by problems with PCA? -> Look this up! Find ACTUAL cause! -> Derive better criterium!


## Code

* Improve performance
* Better variable names

### SummarizedExperiment

Switch to SummarizedExperiment. Problem: How to include annotations?

## Bugs

* Visual editor color update sometimes not triggering

## Additional notes

* Use notifications for importer messages
* ggplot2 has slow performance (3x slower than scatterplot3d)
* All annotation results won't necessary contain all genes and in order!

## R Quirks

* RStudio downloads don't overwrite files correctly
* R 3.3.3 compile rtracklayer etc. from source https://support.bioconductor.org/p/93347/#93373
