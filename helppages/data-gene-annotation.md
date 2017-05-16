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


You can upload multiple gene annotations per upload widget (see `Appendix > Upload widget` for more information).
The data will be integrated into a final annotation. Older data will be overwritten by newer data.

## Importing from file

Following importers are available:

* **Ensembl GFF.** Imports data from a GFF annotation from Ensembl. GO terms are not supported.
* **Tabular CSV.** Imports data from the table exported from the *output* in `Data > Genes > Annotation`
* **Tabular TSV.** Like Tabular CSV, but with TSV format

### General format of tabular CSV/TSV

| ID    | scaffold | start_position | end_position | length | exonlength | biotype | go_terms |
|-------|----------|----------------|--------------|--------|------------|---------|----------|
| Gene1 | ...      | ...            | ...          | ...    | ...        | ...     | term1|term2|...      |
| ...   | ...      | ...            | ...          | ...    | ...        | ...     | ...      |

## Importing from online databases

You have access to two database providers:

1. [AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html)
2. [Ensembl BioMart](http://www.ensembl.org/biomart/)

### AnnotationHub

You need to input following parameters:

1. **Extract information.** The information that should be extracted. You can choose between **biotype** and **sequence info** (scaffold,
  start position, end position, length and exon length)
2. Database
3. Species
4. Dataset

### Ensembl BioMart

You need to input following parameters:

1. **Extract information.** The information that should be extracted. You can choose between **biotype** and **sequence info** (scaffold,
  start position, end position, length and exon length) or **GO terms**
2. Database
3. Species

<div class="well help-box">
<label>Info</label>  Getting data from online resources may take a long time or even fail/freeze if the online resources are under high demand.
</div>
