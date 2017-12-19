# Upload widget

The general upload widget handles all importing of your data. This includes
uploading files, loading samples, generating data from online resources, as
well as integrating multiple data sources.

This widget has two modes:

1. Single data importing
2. Multi data integration

## Single data importing

The widget imports one set of data. An example are a read count table or
a visual style definition. If you choose a different data source, the existing
one will be overwritten if you click *Submit*.

![Screenshot of visuals editor in single data mode](helppages/genericImporter.png)

Depending on the data this can be imported, you have following choices for the
source of the data:

1. **uploaded file.** Imports the data from an imported file.
2. **manual input.** Imports the data by manual input or copy paste.
3. **example data.** Imports an example data set.
4. **generate data.** Generates data from online resources or from already loaded data.

Below the data source selection, there are always additional parameters that belong to
the selected data source. If *uploaded file* is selected, an upload widget will appear.
If *manual input* is selected, a text area allows insertion of the data. If you
want to load a sample data set, a selection box will appear below the data source setting.

Below the data source specific parameters, there will be at least one additional parameter called
**Importer** or **Generator**. As there are often multiple file formats or online databases,
the upload widget might offer multiple options. You can find specific information about
the supported file formats in the help page sections that correspond to the sidebar.

<div class="well help-box">
<label>Note</label> An importer or generator might require additional parameters that appear
below the generator/importer selection. Specific information about those parameters can
be found the respective help sections of each data type.
</div>

## Multi data integration

If data cannot be entirely obtained with one uploaded file or database query,
the upload widget is set to the **integration mode**. This mode allows you
to combine data from multiple files, online resources and samples that are automatically
*integrated* into the final data set.

![Screenshot of visuals editor in integration mode](helppages/integratingGenericImporter.png)

The integrating upload widget works like the widget in single data mode, but additionally
has

1. A list of all currently imported data
2. A callback from the integration function

Use the list of imported data to keep overview and individually delete data sets
from the uploader.

The integration function will return a callback that contains information about the
integrated data, i.e. how many genes have an annotation or which information is missing.
