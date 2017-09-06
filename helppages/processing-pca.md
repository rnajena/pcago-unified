# PCA parameters

There are two types of PCA parameters:

* **Data processing parameters.** Parameters that determine how the data is pre-processed before applying the transformation.
* **Output transformations.** Parameters that determine if the PCA output should be additionally transformed.

## Data processing parameters

### Center data

If this parameter is enabled, the dimensions (each row in the read count table) will be
transformed, so the mean is zero. This is a recommended option as PCA assumes zero-mean data.

### Scale data

If this parameter is enabled, the dimensions (each row in the read count table) will be transformed, so
the variance is 1.

## Output transformations

### Relative sample positions

Allows scaling of the transformed data to a relative space.

* **None.** Don't transform the data.
* **Per dimension.** The data is transformed into relative space by using minimum and maximum of the dimension. This will calculate `NEWVALUE = (VALUE - MIN_DIMENSION) / (MAX_DIMENSION - MIN_DIMENSION)`
* **Global.** The data is transformed into relative space by using global minimum and maximum. This will calculate `NEWVALUE = (VALUE - MIN_GLOBAL) / (MAX_GLOBAL - MIN_GLOBAL)`
