#!/bin/bash

app_dir=$(pwd)
pcago_dir=$1

# Initialize packrat (Will install everything)
echo "[1/3] Bootstrapping packrat package management system ..."
cd $pcago_dir
R --vanilla -f packrat/init.R --args --bootstrap-packrat &

pid=$!
trap "kill $pid" TERM
wait

cd $app_dir

# Force packrat to REALLY restore all libraries (because sometimes it does NOT)
echo "[2/3] Forcing packrat to restore all libraries ..."
cd $pcago_dir

echo "
source('packrat/init.R')
packrat::restore()
" | R --vanilla &

pid=$!
trap "kill $pid" TERM
wait


cd $app_dir

# Start PCAGO
echo "[3/3] Starting PCAGO ..."
cd $pcago_dir

echo "
# Starter script for PCAGO

source('packrat/init.R')

# Set the path of ffmpeg
ffmpeg.path <- '/usr/bin/ffmpeg'
if(!file.exists(ffmpeg.path)) {
    print('Using static build of ffmpeg.')
    ffmpeg.path <- '../ffmpeg-linux-x64/ffmpeg'
}
if(!file.exists(ffmpeg.path)) {
    print('ffmpeg does not exist!')
    q()
}


print(paste('Path of ffmpeg is', ffmpeg.path))

shiny::runApp()

" | R --vanilla &

pid=$!
trap "kill $pid" TERM
wait

cd $app_dir