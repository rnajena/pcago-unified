# PCAGO

## Features

* Help pages
* About page
* Read count normalization result plots
* Custom selectors
* Annotation overview
* SMPFPP server implementation
* BioMart: Use GRanges for seqinfo
* Sample annotation: Fragment lengths
* Integrating importer: Selectize to remove datasets
* General plot settings; disable features

## Code

* Improve performance
* Better variable names

## Bugs

* Visual editor color update sometimes not triggering
* GRanges sequence info exon info generated without any infos
* Sequence info intended behavior?

## Additional notes

* Use notifications for importer messages
* ggplot2 has slow performance (3x slower than scatterplot3d)
* All annotation results won't necessary contain all genes and in order!

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
