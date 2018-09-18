# Samples annotation

Read count samples are annotated with two different types of annotations,
the **sample conditions** and the *optional* **sample info annotation**.
The sample conditions assign a set of conditions to each sample
and is used for visual representation and normalization. The sample info annotation
currently only contains the **mean fragment length** that is needed by TPM normalization.

![Importing schema](helppages/importingOverviewSamplesAnnotation.png)

## Sample conditions

PCAGO represents the sample conditions as a **boolean table** that contains logical values.
The rows represent the samples, while the columns represent the available conditions.
Similar to the read count table, the first row and the first column are reserved for labeling.
All other cells determine if a *sample* of a given row has a *condition* as given by the column.

|       | Condition1    | Condition2 | Condition3 | ... |
|-------|---------------|------------|------------|-----|
| Sample1 | TRUE or FALSE | ...        | ...        | ... |
| Sample2 | ...           | ...        | ...        | ... |
| Sample3 | ...           | ...        | ...        | ... |
| ...   | ...           | ...        | ...        | ... |

**Example**

A valid sample condition table looks like following:

| Sample | Vitamin_C | Vitamin_Control | Infection_EColi | Infection_Control |
|--------|-----------|-----------------|-----------------|-------------------|
| S1     | TRUE      | FALSE           | TRUE            | FALSE             |
| S2     | TRUE      | FALSE           | FALSE           | TRUE              |
| S3     | FALSE     | TRUE            | TRUE            | FALSE             |

Sample **S1** is infected by E. coli and treated with Vitamin C. Sample **S2** is not infected, but
treated with Vitamin C. Sample **S3** is only infected with E. coli.

### Alternative representation

PCAGO is able to convert a different format to represent sample conditions to
its internal representation. This **factor table** is more easily readable by humans
and represents sample conditions as choice from specified categories of treatments. Here the columns represent
the categories.

| Sample  | Category of treatments 1        | Category of treatments 2 | ... |
|---------|-------------------|------------|-----|
| Sample1 | Specific treatment of given category | ...        | ... |
| Sample2 | ...               | ...        | ... |
| ...     | ...               | ...        | ... |

**Example**

Following factor table represents the same conditions as with the other example:

| Sample | Vitamin | Infection |
|--------|---------|-----------|
| S1     | C       | E. coli   |
| S2     | C       | Control   |
| S3     | Control | E. coli   |


### Importing sample conditions

PCAGO supports importing sample conditions from a **boolean table** or
a **factor table**. The tables must be provided in **CSV** format.
See `Importing > File formats` for more information about file formats.

The sample conditions also can be built based on the names of the
samples in the read count table. The *generator* splits the name of each sample
by a userdefined character and adds each of the resulting substrings to the set of conditions.
If for example the name of a sample is `Infection_EColi_N1` and the generator is set to split
using a `_` character, this sample has the conditions `Infection`, `EColi` and `N1`.
If the character does not occur in the sample name or the split character is empty,
the whole sample name is assumed to be a condition.

Other components like the visual editor use the condition table to determine cell properties.
The order of the conditions within the table may change the output depending on the component.
Additionally, the generator can generate unwanted conditions that would affect normalization
(an example might be `N1`).
To solve this, the **Rearrange conditions** control below the samples annotation importer
allows *rearrangement* and *disabling* of specific conditions.

![Condition rearrangement tool](helppages/samplesAnnotationConditionEditor.png)

## Sample info

The sample info currently only contains the mean fragment length used by TPM normalization
and similar to the condition annotation is stored as a table.
Again, the rows represent the sample. The columns represent the currently supported
annotation values. To allow extension of PCAGO, both the first row and the first column
are reserved for labeling of the data.

| ID      | meanfragmentlength |
|---------|--------------------|
| Sample1 | ...                |
| Sample2 | ...                |
| ...     | ...                |

The table must be provided in **CSV** format.
See `Importing > File formats` for more information about file formats.

You can upload multiple sample annotations per upload widget (see `Importing > Upload widget` for more information).
The data will be integrated into a final sample annotation. You can upload multiple data for the same type.
The sample info annotation will be merged, while the conditions are completely overwritten. Newer data overwrites
old data.
