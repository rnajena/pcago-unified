# Gene annotation

The gene annotation can be used to select only a specific set of genes. Other annotation
data is needed for read count normalization.

![Importing schema](helppages/importingOverviewGeneAnnotation.png)

A gene has following annotations:

* **Scaffold.** Scaffold/sequence the gene is located.
* **Start position.** Start location of the gene.
* **End position.** End location of the gene.
* **Length.** Length of the gene.
* **Exon length.** Non overlapping exon length. Used for TPM read count normalization.
* **Biotype.** Biotype of the gene.
* **GO terms.** List of GO terms for filtering
* **Custom.** List of additional keywords that can be filtered in a similar fashion as GO terms

You can upload multiple gene annotations per upload widget (see `Importing > Upload widget` for more information).
The data will be integrated into a final annotation. Older data will be overwritten by newer data.

## Importing from file

PCAGO supports two file formats, which are GFF files downloaded from Ensembl (**Ensembl GFF**) and
a tabular format internally used by PCAGO (**PCAGO gene annotation table**). Please note that
**Ensembl GFF files do not support GO terms**. The tables exported from `Content Navigation > Data > Genes > Annotation`
are consistent with the PCAGO gene annotation table format and can be imported.

The uploader widget allows selection of specific annotations to be imported.
Deselect a checkbox to exclude the associated annotation data from being imported.

### General format of PCAGO gene annotation table

| ID    | scaffold | start_position | end_position | length | exonlength | biotype | go_ids | custom |
|-------|----------|----------------|--------------|--------|------------|---------|----------|--------|
| Gene1 | ...      | ...            | ...          | ...    | ...        | ...     | term1&#124;term2&#124;... | custom1&#124;custom2&#124;...      |
| ...   | ...      | ...            | ...          | ...    | ...        | ...     | ...      | ... |

## Importing from online databases

PCAGO includes access to following sources of gene annotations:

1. [AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html)
2. [Ensembl BioMart](http://www.ensembl.org/biomart/)

### AnnotationHub

Following parameters are required:

1. **Database.** The database that is used as source for the annotation. *Please note that we only tested Ensembl. There may be problems if you want to use other sources.*
2. **Species.** The species your data is generated from.
3. **Dataset.** Choose one of the available data sets. *Use the documentation of the respective database to find out which dataset is the correct one for the data.*

### Ensembl BioMart

Following parameters are required:

1. **Database.** The database that is used as source for the annotation. *Please note that we only tested Ensembl Gene 88. There may be problems if you want to use other sources.*
2. **Species.** The species your data is generated from.

<div class="well help-box">
<label>Info</label>  Getting data from online resources may take a long time or even fail/freeze if the online resources are under high demand.
</div>
