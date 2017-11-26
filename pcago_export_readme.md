# PCAGO export

This README file contains a short description for each exported file.

## readcounts_raw.csv

The raw read count table.

## readcounts_processed.csv

Read counts where additional processing steps such as transposition,
normalization & removal of constant count genes were applied.
See the processing report for detailed parameters.

## readcounts_filtered.csv

Read counts filtered by gene annotation. See the processing report 
for detailed parameters.

## readcounts_top_variant.csv

Read counts filtered by applying a gene variance cut-off. 
See the processing report for detailed parameters.

## readcounts_pca_transformed.csv

Read counts transformed with PCA. 
See the processing report for detailed parameters.

## sample_conditions.csv

Sample conditions as boolean factor table (see PCAGO help).

## sample_annotation.csv

Additional sample annotations such as the mean fragment length.

## gene_annotation.csv

Gene annotations in PCAGO tabular format (see PCAGO help).

## gene_variances_processed.csv

Gene variances (absolute and relative) of processed read counts.
See the processing report for detailed parameters.

## gene_variances_filtered.csv

Gene variances (absolute and relative) of gene annotation filtered
read counts. See the processing report for detailed parameters.

## pca_principal_components.csv

Principal component vectors as matrix.
See the processing report for detailed parameters.

## pca_variances.csv

Variance of each principal component.
See the processing report for detailed parameters.

## clustering_readcounts_processed.svg

Agglomerative clustering plot of processed read counts.

## clustering_readcounts_processed.newick

Agglomerative clustering dendrogram of processed read counts
as NEWICK file.

## clustering_readcounts_filtered.svg

Agglomerative clustering plot of gene variance filtered read counts.

## clustering_readcounts_filtered.newick

Agglomerative clustering dendrogram of gene variance filtered
read counts as NEWICK file.

## clustering_readcounts_top_variant.svg

Agglomerative clustering plot of top variant read counts.

## clustering_readcounts_top_variant.newick

Agglomerative clustering dendrogram of top variant
read counts as NEWICK file.

## clustering_readcounts_pca_transformed.svg

Agglomerative clustering plot of PCA transformed read counts.

## clustering_readcounts_pca_transformed.newick

Agglomerative clustering dendrogram of PCA transformed
read counts as NEWICK file.

## sample_conditions.tiff

Venn diagram showing the relation of specific sample conditions.

## pca_plot.svg

PCA plot of top variant read counts. 
See the processing report for detailed parameters.

## variances_readcounts_processed.svg

Plot of gene variances of processed read counts.

## variances_readcounts_filtered.svg

Plot of gene variances of gene annotation filtered read counts.

## pca_variances.svg

Plot of PCA variances.

## processing_report.html

Contains all relevant parameters used to generate the data.
