#!/bin/bash

# This script extracts the license files for each packrat package
cd packrat/src/
current_dir=$(pwd)

for d in $(ls -1d */); do

    echo $d

    cd $d
    
    tar -xzf *.tar.gz
    
    target=$(ls -1d */)
    
    if [ -e "$target/LICENSE" ]; then
    
        echo "Found a license for $target"
        cp $target/LICENSE LICENSE
    
    fi
    
    rm -r $target
    
    cd $current_dir

done 
