# PCAGO

PCAGO is an interactive web service that allows analysis of RNA-Seq read
count data with principal component analysis (PCA) and agglomerative
clustering. The tool also includes features like read count
normalization, filtering read counts by gene annotation and various
visualization options.

# Running PCAGO

You can use our server on https://pcago.bioinf.uni-jena.de or run
PCAGO on your own computer or server via Docker.

If you want to run PCAGO in RStudio or plain R, you find instructions
[here](https://github.com/rnajena/pcago-unified/blob/master/src/README.md).

## Run via Docker

Big thanks go out to Lasse Faber!

When running PCAGO on a remote server, connect with port forwarding and open 127.0.0.1:8000 in your browser:
```
ssh -L 8000:127.0.0.1:8000 your@your.server.com
```

### Server 2.0

We have created a new docker container with the PCAGO app and a running shiny server.
Run the docker container with e.g.

```
docker run -it -p 8000:8000 -v "$(pwd)"/logs:/home/shiny/logs trichterhub/pcago-server:latest
```

This is the Docker container that powers our server at https://pcago.bioinf.uni-jena.de.
Processes inside docker are started by the shiny user, id 999. User id 999 should have access to the
logs directory on the host machine.
Alternatively, you may start docker with the root user with the ``--user`` flag.
Shiny Server's own log goes to the container output (`docker logs`), the app logs to `/home/shiny/logs`.

The image (`Dockerfile_server`) is based on [rocker/shiny](https://rocker-project.org/images/versioned/shiny.html)
(R 4.6.2, Ubuntu 26.04 LTS). R package versions are pinned in `src/renv.lock`.
To build it yourself (amd64 only; works under emulation on Apple Silicon, but slowly):

```
docker build --platform linux/amd64 --build-arg PCAGO_VERSION=$(git describe --always --dirty) \
  -t trichterhub/pcago-server:2.0 -t trichterhub/pcago-server:latest -f Dockerfile_server .
# Optional: run the pipeline smoke test against the image
docker run --rm --platform linux/amd64 -v "$(pwd)"/docker/smoketest.R:/tmp/smoketest.R:ro \
  --entrypoint Rscript trichterhub/pcago-server:2.0 /tmp/smoketest.R
```

# Installation

The former Ubuntu 18.04 installation script (`install.sh`, R 3.4 + packrat) has been removed.
Use the Docker image above, or install R and the packages from `src/renv.lock` as described in the
[R-specific manual](https://github.com/rnajena/pcago-unified/blob/master/src/README.md).

The Electron desktop application in `src-electron/` depended on the removed packrat setup and is
currently not supported.

For credits and more detailed information, see [our R-specific manual](https://github.com/rnajena/pcago-unified/blob/master/src/README.md)
