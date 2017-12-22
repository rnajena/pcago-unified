#
# Sets some environment variables
#

# Default program paths (set if not set from outside)
default.ffmpeg.path <- "/usr/bin/ffmpeg"
default.git.path <- "/usr/bin/git"

# If enabled, the gene annotation importing runs in a different thread (Disable if you have problems or need debugging)
parallelized.gene.annotation.importer <- T

# How many GO terms can be viewed in detail in GO browser?
go.browser.detailed.maxterms <- 50

# How many read count samples are supported
readcounts.maxsamples <- 100

# Set to non-null value to disable reading the version from git
pcago.version <- NULL

############
# End of Config section
############

# Path of ffmpeg for creating movies. Defaults to ffmpeg on Linux
if(!exists("ffmpeg.path")) {
  assign("ffmpeg.path", default.ffmpeg.path, envir = globalenv())
}

# Path of git for version display
if(!exists("git.path")) {
  assign("git.path", default.git.path, envir = globalenv())
}

# Set version from git if enabled
if(is.null(pcago.version)) {
  git.branch <- system2(git.path, c("rev-parse", "--abbrev-ref", "HEAD"), stdout = T)
  git.commit <- system2(git.path, c("rev-parse", "HEAD"), stdout = T)
  
  pcago.version <- paste0(git.branch, "/", git.commit)
}
