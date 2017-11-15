#!/bin/bash

echo -e "source('packrat/init.R')\nshiny::runApp()" | R --vanilla
