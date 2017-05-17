# Samples annotation

A read count sample has following annotations:

* Conditions
* Mean fragment length

The **conditions** annotation defines the conditions/treatments of each sample,
which are needed for DESeq2 normalization and visual representation of the data.
The **mean fragment length** is needed for TPM normalization (see `Data > Read count processing` help page).

## Conditions annotation

The condition annotation determines for each sample if it is in a condition or has a specific treatment.
It is a generalization of factorized treatment tables like the following:

| Sample | Vitamin | Infection |
|--------|---------|-----------|
| S1     | C       | E. coli   |
| S2     | C       | Control   |
| S3     | Control | E. coli   |

Instead of having factors like "Vitamin" or "Infection", the general approach of the condition table
stores only boolean values (true or false)

| Sample | Vitamin_C | Vitamin_Control | Infection_EColi | Infection_Control |
|--------|-----------|-----------------|-----------------|-------------------|
| S1     | TRUE      | FALSE           | TRUE            | FALSE             |
| S2     | TRUE      | FALSE           | FALSE           | TRUE              |
| S3     | FALSE     | TRUE            | TRUE            | FALSE             |

## Mean fragment length

The mean fragment length of a sample can be obtained from the read analysis step.

## Managing conditions

Other components like the visual editor use the condition table to determine cell properties.
The order of the conditions within the table may change the output depending on the component.
You can disable and move conditions by using the **Rearrange conditions** control at
the below the samples annotation importer.

![Condition rearrangement tool](helppages/samplesAnnotationConditionEditor.png)

## Importing data

You can upload multiple sample annotations per upload widget (see `Appendix > Upload widget` for more information).
The data will be integrated into a final sample annotation. You can upload multiple data for the same type.
The sample info annotation will be merged, while the conditions are completely overwritten. Newer data overwrites
old data.

## Conditions

Following importers are available:

* **Conditions boolean CSV.** Import from a boolean table (CSV format)
* **Conditions boolean TSV.** Import from a boolean table (TSV format)
* **Conditions treatments CSV.** Import from a *treatments* table (CSV format)
* **Conditions treatments TSV.** Import from a *treatments* table (TSV format)

See `Appendix > File formats` for more information about file formats.

Following generators are available:

* **Conditions from sample names.** Generates the conditions table by splitting the sample names by a **separator** (additional parameter)

### General format of a boolean table

|       | Condition1    | Condition2 | Condition3 | ... |
|-------|---------------|------------|------------|-----|
| Sample1 | TRUE or FALSE | ...        | ...        | ... |
| Sample2 | ...           | ...        | ...        | ... |
| Sample3 | ...           | ...        | ...        | ... |
| ...   | ...           | ...        | ...        | ... |

Sample1, Sample2, ... are the column names in your read count table.

### General format of a treatment table

| Sample  | Treatment1        | Treatment2 | ... |
|---------|-------------------|------------|-----|
| Sample1 | Treatment factors | ...        | ... |
| Sample2 | ...               | ...        | ... |
| ...     | ...               | ...        | ... |

Sample1, Sample2, ... are the column names in your read count table.

## Mean fragment length

For future development, the mean fragment length is part of the **sample info**
annotation that may contain additional information about each same similar to
gene annotations.

Following importers are available:

* **Sample info CSV.** Import from a CSV file
* **Sample info TSV.** Import from a TSV file

### General format of a sample info table

| ID      | meanfragmentlength |
|---------|--------------------|
| Sample1 | ...                |
| Sample2 | ...                |
| ...     | ...                |
