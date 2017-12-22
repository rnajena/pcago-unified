#
# Sets some environment variables
#

# Path of ffmpeg for creating movies. Defaults to ffmpeg on Linux
if(!exists("ffmpeg.path")) {
  ffmpeg.path <- "/usr/bin/ffmpeg"
}

# If enabled, the gene annotation importing runs in a different thread (Disable if you have problems or need debugging)
parallelized.gene.annotation.importer <- F

# How many GO terms can be viewed in detail in GO browser?
go.browser.detailed.maxterms <- 50

# How many read count samples are supported
readcounts.maxsamples <- 100