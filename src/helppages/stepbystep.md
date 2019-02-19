# Step by step example

This page will guide you through all steps to process and visualize a data set.
It uses `Monocytes` data set you can find [here](www/example_data.zip).

## Step 0: Prerequisites

You need:

* Read counts in CSV format
* (Optional) A sample annotation that assigns conditions to a sample
* (Optional) A gene annotation

![Step 0](helppages/stepbystep/step_0_prerequisites.png)

## Step 1: Upload the read counts

Open the `Data > Import read counts` section of the sidebar and select the
option to upload a file. Select the read count file and click on `Submit`.

![Step 1](helppages/stepbystep/step_1_upload_read_counts.png)

## Step 2: Display read counts

Check if the read counts are loaded correctly. PCAGO will automatically show
the data when it is available.

![Step 2](helppages/stepbystep/step_2_read_counts.png)

## Step 3: Sample annotation

You can either upload a file or generate the sample annotation from sample
names.

![Step 3](helppages/stepbystep/step_3_sample_annotation.png)

## Step 4: Sample annotation

You can exclude any sample condition from further calculation steps by
clicking the `X` button next to the condition.

![Step 4](helppages/stepbystep/step_4_sample_conditions.png)

## Step 5: Gene annotation

You can again upload a gene annotation file or query data from online databases
like Ensembl BioMart. Please note that the resources of our server are limited, which
can cause issues with large annotations. In this case, run PCAGO from your computer.

![Step 5](helppages/stepbystep/step_5_gene_annotation.png)

## Step 6: Normalization

We offer normalization with TPM or DESeq2. Please keep in mind that this process
can take a long time for larger data sets. 

![Step 6](helppages/stepbystep/step_6_normalization.png)

## Step 7: Filter genes by annotation

You can filter genes by their gene annotation. This includes GO terms, chromosome and other properties.
Just click into the filter area to show the list of available filters.

![Step 7](helppages/stepbystep/step_7_filter_genes.png)

## Step 8: Filter genes by variance

You can also filter genes by their variance. Just drag the slider to select the top variant genes.
This filter also supports animation: Just click the play button.

![Step 8](helppages/stepbystep/step_8_filter_genes_2.png)

## Step 9: PCA

At any time, you can switch to the PCA samples plot that displays PC1 and PC2
by default.

![Step 9](helppages/stepbystep/step_9_pca.png)

## Step 10: 3D PCA

To change the displayed principal component axes or switch to a 3D PCA plot,
change the displayed axes in the `Axes` setting.

![Step 10](helppages/stepbystep/step_10_3d_pca.png)

## Step 10: Plot settings

PCAGO allows you to change many plot settings directly within the application.
You can for example assign a color and/or shape to a condition or change the name
displayed in the plot.

![Step 11](helppages/stepbystep/step_11_visualization.png)
