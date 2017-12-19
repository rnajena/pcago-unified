# Read counts

To provide read counts, go to `Sidebar > Data > Import read counts` and choose
an example data set or upload a file. You can find more information about the
upload widget in `Importing > Upload widget` help section.

Read counts are provided in tabular form where a column represents a sample
and a row represents a gene. The first row contains the **sample names** that are
refered in the sample annotation. The first column contains the name of the gene.

| ID    | Sample1                       | Sample2                       | Sample3 | ... |
|-------|-----------------------------|-----------------------------|-------|-----|
| Gene1 | Read count of Gene1 in Sample1 | Read count of Gene1 in Sample2 | ...   | ... |
| Gene2 | Read count of Gene2 in Sample1 | ...                         | ...   | ... |
| Gene3 | ...                         | ...                         | ...   | ... |
| ...   | ...                         | ...                         | ...   | ... |

PCAGO can import read count data from **CSV** files.
See `Importing > File formats` for more information about file formats.

![Importing schema](helppages/importingOverviewReadCounts.png)
