# Overview

Working with PCAGO can be categorized into three sections:

1. Importing read counts and annotation data
2. Processing like normalization and filtering
3. Generating the output

First, read count data, sample and gene annotations need to be imported. The absolute
minimum consists of the read count data and the sample annotation that defines the conditions
of each sample. For other tasks like filtering or normalization, additional sample annotations
or gene annotations are required.

As second step, PCAGO allows additional processing steps that remove genes with a variance of zero,
read count normalization and filtering of read count data by only including genes with
specific properties. At last, principal component analysis (PCA) is applied to the
data.

Lastly, output plots and files need to be created for further usage.

This help page will cover all required information about each step
and will use certain terms to refer to specific user interface elements.

## User interface overview

The user interface of the main application is separated into three parts:

1. The content area
2. The content navigation
3. The sidebar

The **content** area displays the currently selected output. This output consists of
plots, tables and additional information about the currently displayed data. The
**content navigation** allows access to all available outputs.
The **sidebar** contains all controls, which includes plot settings and all controls
that allow importing or processing of data. With exception to **Plot**,
the categories in the sidebar are *independent* from the currently selected content.
Each category is marked with an icon that indicates if user input is expected, recommended or required:

* <i class="fa fa-exclamation-circle" aria-hidden="true"></i> indicates required input
* <i class="fa fa-check-circle" aria-hidden="true"></i> indicates that we recommend at least checking of the parameters
* <i class="fa fa-circle" aria-hidden="true"></i> indicates that input is useful, but entirely optional




![UI of PCAGO](helppages/overview.png)
