Using annotations of the genes provided in the read count table allows you
to apply additional processing steps and to filter the genes used for the PCA.

PCAGO differentiates between following annotation types:

* **Sequence info** provides the scaffold, start and stop positions of the gene
* **Associated features** provides a list of associated feature types like "miRNA" for each gene
* **GO terms** provides a list of associated GO terms for each gene

## Providing annotations

Annotations can be uploaded in `Sidebar > Data > Annotation`.

Annotations can come in various forms and not every format covers all information
about the genes. As a solution to this problem, the annotation uploader allows
you to (up)load *multiple* annotation sources that are combined for further processing.

You can upload an annotation file or use a *generator* to fetch the data from available
data sources like Ensembl BioMart for your currently loaded read counts.

## Supported formats

* **Ensembl GFF** An annotation file from Ensembl database. This format provides *sequence information* and *associated features*

## Supported generators

* **Ensembl BioMart GO terms** Imports *GO terms* from Ensembl BioMart
* **Ensembl BioMart sequence info** Imports *sequence info* from Ensembl BioMart
