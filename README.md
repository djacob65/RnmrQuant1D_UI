# RnmrQuant1D User Interface

## Purpose

<div style="text-align: justify">

**Application dedicated to 1D proton NMR quantification, including peak fitting and based on external calibration using standard spectra**

This application was initially developed as part of a project on wine authenticity. However, it is generic enough to be used on other biological and/or food matrices. This involves the implementation of an analytical protocol allowing quantification from an external standard (see references).

This application is designed around the <a href="https://github.com/djacob65/RnmrQuant1D" target="_blank">RnmrQuant1D</a> package, which forms its core. However, it is primarily designed for processing small batches of spectra (<100) more easily than in script mode. For larger batches, it is strongly recommended to switch to script mode (<a href="https://docs.posit.co/ide/user/" target="_blank">Rstudio</a> or <a href="https://jupyter.org/" target="_blank">JupyterLab</a>)

</div>


## Installation

* Requirements:

	* R version >= 4.3
	* RnmrQuant1D >= 1.2.6 (see https://github.com/djacob65/RnmrQuant1D)
	

* Clone this repository, then `cd` to your clone path.

            git clone git@github.com:djacob65/RnmrQuant1D_UI.git
            cd RnmrQuant1D_UI

<br>

## Usage

Either you open a R terminal, then you enter the following command:

           shiny::runApp(launch.browser=TRUE)

or from a shell/batch console (cmd or bash), enter the following command:

           Rscript -e 'shiny::runApp(launch.browser=TRUE)'

or for Windows users, click on the 'runApp.bat' file into the explorer

<br>


### Funded by:

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
