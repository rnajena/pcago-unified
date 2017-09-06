# Filtering read counts with gene annotation

You can filter your read counts by restricting them to a specific set of genes.
The first available filter mechanism is to use the data from the gene annotation.

![Filtering genes with annotation data](helppages/geneFilterByAnnotation.png)

## Selecting criteria

Click on the large selection field to list all available criteria. Type into the
field to search for criteria.

Currently, following criteria are available:

* Biotype (blue color)
* GO term (green color)
* Scaffold (purple color)

**Note:** Don't forget to remove the **All (*)** element from the list or all genes
are selected.

## Filter settings

You can change how the criteria are matched. You can either select all genes that
have at least one matching criterion (*OR*) or only genes where **all** criteria
in the selection field have to apply (*AND*). Additionally, you can invert the selection.

### Example

You have genes A, B, C and D.

| Gene | Scaffold | Biotype        |
|------|----------|----------------|
| A    | X        | protein_coding |
| B    | X        | miRNA          |
| C    | 1        | protein_coding |
| D    | 2        | lncRNA         |

Depending on the filter settings, different genes are selected.

| Selected criteria | Operation | Invert | Selected genes |
|-------------------|-----------|--------|----------------|
| X, protein_coding | OR        | False  | A,B,C          |
| X, protein_coding | AND       | False  | A              |
| X, protein_coding | AND       | True   | B,C,D          |
| X, protein_coding | OR        | True   | D              |


# Filtering read counts by gene variance cut-off

![Filtering genes by gene variance](helppages/geneFilterByVariance.png)
