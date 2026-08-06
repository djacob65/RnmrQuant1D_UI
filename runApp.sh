#!/bin/bash

RSCRIPT=$(which Rscript)

PORT=8080
(
   cd ./app
   $RSCRIPT -e "shiny::runApp(appDir=file.path(getwd(),'app'), port=$PORT, launch.browser=FALSE, host='127.0.0.1')"
)

