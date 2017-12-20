#
# Sets some environment variables
#

# Path of ffmpeg for creating movies. Defaults to ffmpeg on Linux
if(!exists("ffmpeg.path")) {
  ffmpeg.path <- "/usr/bin/ffmpeg"
}

# If enabled, the gene annotation importing runs in a different thread
parallelized.gene.annotation.importer <- T