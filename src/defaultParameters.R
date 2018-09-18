#
# Contains default parameters for the sidebar
#

## Data

# Preprocessing (Transpose, remove zero count genes)
default.data.preprocessing.transpose <- F
default.data.preprocessing.removezero <- T

# Normalization
default.data.normalization <- "none"

# Postprocessing (Remove zero variance genes)
default.data.postprocessing.removeconstant <- T

## PCA

# PCA centering, variance scaling
default.pca.settings.centering <- T
default.pca.settings.scaling <- F

# Additional output transformations
default.pca.settings.relative <- "none"

## Plots
default.plot.width <- 1024
default.plot.height <- 768
default.plot.dpi <- 96