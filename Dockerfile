FROM ubuntu:18.04

# Avoid interaction when installing npm
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
	unzip \
	r-base \
	wget \
	ffmpeg \
	libcurl4-gnutls-dev \
	libxml2-dev \
	libssl-dev \
	libmysqlclient-dev \
	libcairo2-dev \
	libgtk2.0-dev \
	xvfb \
	xauth \
	xfonts-base \
	libxt-dev \
	libgconf2-4 \
    git
RUN apt-get install -y npm build-essential

WORKDIR /home
# RUN wget --quiet https://github.com/rumangerst/pcago-unified/archive/ubuntu-18.04.zip -O pcago.zip
#RUN wget --quiet https://github.com/lmfaber/pcago-unified/archive/0.1.zip -O pcago.zip
RUN git clone https://github.com/rumangerst/pcago-unified.git
RUN mv /home/pcago-unified /home/pcago-unified-0.1 
#RUN unzip -qq pcago.zip && rm pcago.zip

WORKDIR /home/pcago-unified-0.1
RUN wget --quiet https://github.com/rumangerst/pcago-unified/releases/download/ubuntu-18.04/packrat-Ubuntu-1804.zip -O packrat.zip
RUN unzip -o -qq packrat.zip -d /home/pcago-unified-0.1/src/packrat && rm packrat.zip


RUN mv /home/pcago-unified-0.1/src/packrat/lib/x86_64-pc-linux-gnu/3.4.3 /home/pcago-unified-0.1/src/packrat/lib/x86_64-pc-linux-gnu/3.4.4

RUN mkdir -p /home/pcago-unified-0.1/src/packrat/lib-R && mkdir -p /home/pcago-unified-0.1/src/packrat/lib-ext
WORKDIR /home/pcago-unified-0.1/src
