#!/bin/bash

echo -e "source('packrat/init.R')\nshiny::runApp(port=8000)" | R --vanilla
