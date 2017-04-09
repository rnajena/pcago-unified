# Read counts

To provide read counts, go to `Sidebar > Data > Readcounts > Import` and choose
a sample data set or upload a file.

## Uploading read counts

Read counts are provided in tabular form. Currently supported are CSV files with comma or whitespace/tab separation.
Choose the *importer* according to the file you want to upload.

The read count table should have following form:

| ID    | Cell1                       | Cell2                       | Cell3 | ... |
|-------|-----------------------------|-----------------------------|-------|-----|
| Gene1 | Read count of Gene1 in Cell1 | Read count of Gene1 in Cell2 | ...   | ... |
| Gene2 | Read count of Gene2 in Cell1 | ...                         | ...   | ... |
| Gene3 | ...                         | ...                         | ...   | ... |
| ...   | ...                         | ...                         | ...   | ... |

## Additional processing

You can apply additional processing to your read count table:

### Remove genes with constant read counts

If enabled, all genes with constant read counts are omitted from the table.
This is needed if you want to apply variance scaling to the data before PCA as
genes with constant read counts have a variance of zero.

### Transpose matrix

You can use this option if you provided a transposed table (where the genes are the columns
  and the cells are the rows).

## Read count normalization

If your read counts are not normalized, it is advised to apply normalization.

**TODO**

<div class="well help-box">
<label>Info</label>  We chose to not include normalization via RPKM/FPKM. See <a href="http://www.rna-seqblog.com/rpkm-fpkm-and-tpm-clearly-explained/">here</a> and <a href="http://blog.nextgenetics.net/?e=51">here</a> for great articles that give a summary about read count normalization and the issues with RPKM/FPKM.
</div>
