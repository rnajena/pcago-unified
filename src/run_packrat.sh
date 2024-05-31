#!/bin/bash

echo -e "source('packrat/init.R')\nshiny::runApp(host='0.0.0.0',port=8000)" | R --vanilla
