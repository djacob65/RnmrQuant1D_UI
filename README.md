# RnmrQuant1D User Interface

## Purpose

<div style="text-align: justify">

**Application dedicated to 1D proton NMR quantification, including peak fitting and based on external calibration using standard spectra**

This application was initially developed as part of a project on wine authenticity. However, it is generic enough to be used on other biological and/or food matrices. This involves the implementation of an analytical protocol allowing quantification from an external standard (see references).

This <a href="https://shiny.posit.co/r/getstarted/" target="_blank">R Shiny</a> application is designed around the <a href="https://github.com/djacob65/RnmrQuant1D" target="_blank">RnmrQuant1D</a> package, which forms its core. However, it is primarily designed for processing small batches of spectra (<100) more easily than in script mode. For larger batches, it is strongly recommended to switch to script mode using <a href="https://docs.posit.co/ide/user/" target="_blank">Rstudio</a> or <a href="https://jupyter.org/" target="_blank">JupyterLab</a>.

</div>


## Installation

* Requirements:

	* R version >= 4.3
	* RnmrQuant1D >= 1.2.6 (see https://github.com/djacob65/RnmrQuant1D)
	```r
	if (!require("devtools"))
		install.packages("devtools", repos="https://cran.rstudio.com")
	devtools::install_github("djacob65/RnmrQuant1D", dependencies = TRUE)
	```

	* Other R packages : shiny, shinyjs, shinyBS, shinyWidgets, shinycssloaders, markdown, colourpicker
	```r
	install.packages(c('shiny','shinyjs','shinyBS','shinyWidgets','shinycssloaders',
					'markdown','colourpicker'), repos = 'https://cran.rstudio.com')
	```

* Clone this repository, then `cd` to your clone path.

		git clone git@github.com:djacob65/RnmrQuant1D_UI.git
		cd RnmrQuant1D_UI

* Otherwise, you can download the ZIP file (see <a href="https://github.com/djacob65/RnmrQuant1D_UI/releases" target="_blank">Releases</a>), then unzip it.

<br>

## Usage

Either you open a R terminal, then you enter the following command:

           setwd('/path_to_the_RnmrQuant1D_UI_directory/app')
           shiny::runApp(launch.browser=TRUE)

or from a shell/batch console (cmd or bash), enter the following command:

           cd /path_to_the_RnmrQuant1D_UI_directory/app
           Rscript -e 'shiny::runApp(launch.browser=TRUE)'

or for _Windows_ users, click on the _runApp.bat_ file into the explorer - Don't forget to set the execution rights in the properties,

or for _Linux_ users, run the _runApp.sh_ file.

<br>

## Usage with Docker

### Dockerfile

A _Dockerfile_ is provided for Linux and macOS operating systems, provided that _Docker_ is installed. To perform the build, run the following command - it can take about a quarter of an hour :

           docker build -t rnmrquant1d .

To run the image as a container, execute the following command:

           docker run -d -p 80:3838 -v /tmp:/tmp --name rq1d rnmrquant1d

Then, in your web navigator, the application is accessible to the URL : http://127.0.0.1

### Docker Hub

It is also possible to pull the _Docker_ image directly from _Docker Hub_ and then create an instance using the following commands:

           docker pull nmrprocflow/rnmrquant1d:latest
           docker run -d -p 80:3838 -v /tmp:/tmp --name rq1d nmrprocflow/rnmrquant1d

Obviously, in this latter case, you do not need to upload the source code,  nor do you even need to have R installed. :-)

<br>

## Funded by:

* Agence Nationale de la Recherche - [ANR-21-CE21-0014](https://anr.fr/Project-ANR-21-CE21-0014)
* [INRAE, UR BIA, plate-forme BIBS](https://www.bibs.inrae.fr/eng)

<br>

## License

Copyright (C) 2026  Daniel Jacob - INRAE

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
