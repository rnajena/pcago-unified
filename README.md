# PCAGO
 
PCAGO is an interactive web service that allows analysis of RNA-Seq read
count data with principal component analysis (PCA) and agglomerative
clustering. The tool also includes features like read count
normalization, filtering read counts by gene annotation and various
visualization options.

# Running PCAGO

You can use our server on http://pcago.bioinf.uni-jena.de/ or run
PCAGO on your Linux computer via a server or as standalone application.

If you want to run PCAGO in RStudio or plain R, you find instructions
[here](https://github.com/rumangerst/pcago-unified/blob/master/src/README.md).

# Installation

We offer an installation script designed for Ubuntu 18.04 that builds the
final server and standalone application including installation of dependency
packages.

1. Clone or [download](https://github.com/rumangerst/pcago-unified/archive/master.zip) our PCAGO repository
2. If needed, extract the \*.zip file
3. Open a terminal in the downloaded PCAGO folder
4. Run `./install.sh` and follow the instructions

When the installation is finished, you can navigate to the installation directory and
either start `pcago-electron.sh` to run the standalone application or run `./pcago-server.sh`
in a terminal to start the server.

For credits and more detailed information, see [our R-specific manual](https://github.com/rumangerst/pcago-unified/blob/master/src/README.md)
