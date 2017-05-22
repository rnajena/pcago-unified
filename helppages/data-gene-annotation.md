# Gene annotation

The gene annotation can be used to select only a specific set of genes. Other annotation
data is needed for read count normalization.

A gene has following annotations:

* **Scaffold.** Scaffold/sequence the gene is located.
* **Start position.** Start location of the gene.
* **End position.** End location of the gene.
* **Length.** Length of the gene.
* **Exon length.** Non overlapping exon length. Used for TPM read count normalization.
* **Biotype.** Biotype of the gene.
* **GO terms.** GO terms for filtering
* **Custom.** An external selector that can be defined by the user.


You can upload multiple gene annotations per upload widget (see `Appendix > Upload widget` for more information).
The data will be integrated into a final annotation. Older data will be overwritten by newer data.

## Importing from file

Following importers are available:

* **Ensembl GFF (\*.gff)** Imports data from a GFF annotation from Ensembl. GO terms are not supported.
* **PCAGO gene annotation table (\*.csv)** Imports data from the table exported from the *output* in `Data > Genes > Annotation`

The **Imported data** parameter is available for all importers. Deselect a checkbox to
exclude the associated annotation data from being imported.

### General format of PCAGO gene annotation table

| ID    | scaffold | start_position | end_position | length | exonlength | biotype | go_terms | custom |
|-------|----------|----------------|--------------|--------|------------|---------|----------|--------|
| Gene1 | ...      | ...            | ...          | ...    | ...        | ...     | term1|term2|... | custom1|custom2|...      |
| ...   | ...      | ...            | ...          | ...    | ...        | ...     | ...      | ... |

## Importing from online databases

You have access to two database providers:

1. [AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html)
2. [Ensembl BioMart](http://www.ensembl.org/biomart/)

### AnnotationHub

You need to input following parameters:

1. **Database.** The database that is used as source for the annotation. *Please note that we only tested Ensembl. There may be problems if you want to use other sources.*
2. **Species.** The species your data is generated from.
3. **Dataset.** Choose one of the available data sets. *Use the documentation of the respective database to find out which dataset is the correct one for your data.*
4. **Imported data.** You can restrict the loaded data by deselecting entries.

### Ensembl BioMart

You need to input following parameters:

1. **Database.** The database that is used as source for the annotation. *Please note that we only tested Ensembl Gene 88. There may be problems if you want to use other sources.*
2. **Species.** The species your data is generated from.
3. **Imported data.** You can restrict the loaded data by deselecting entries.

<div class="well help-box">
<label>Info</label>  Getting data from online resources may take a long time or even fail/freeze if the online resources are under high demand.
</div>
