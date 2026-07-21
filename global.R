#------------------------------------------------
# ID global.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


suppressMessages({
	library(shiny)
	library(shinyjs)
	library(shinyBS)
	library(shinyWidgets)
	library(shinycssloaders)
	library(markdown)
	library(plotly)
	library(DT)
	library(htmlwidgets)
	library(htmltools)
	library(openxlsx)
	library(RnmrQuant1D)
})

options(width=128)
options(warn=-1)
options(stringsAsFactors=FALSE)

# Instrument/Vendor/Format
optionVendor <- c('bruker', 'varian','jeol')
names(optionVendor) <- c('Bruker', 'Varian/Agilent', 'Jeol JDF format')
selectVendor <- c("-- Select the input format --"="sinput", optionVendor)

Sys.setenv(LANGUAGE = "en")

# Operating System (windows, linux, macos, ...)
OS <- .Platform$OS.type

# Load general routines
source("Rsrc/Tools.R", local=TRUE)

# Read the configuration settings file
conffile <- "conf/global.ini"
conf <- Parse.INI(conffile, section="GLOBAL")

# Software Metadata
TITLE <- conf$TITLE
CPRGHT <- conf$CPRGHT
VERSION <- conf$VERSION

# Max size for the ZIP file
MAXZIPSIZE <- 500
if (is.numeric(conf$MAXZIPSIZE) && as.numeric(conf$MAXZIPSIZE)>0) {
    MAXZIPSIZE <- as.numeric(conf$MAXZIPSIZE)
}
options(shiny.maxRequestSize=MAXZIPSIZE*1024^2)
options(shiny.sanitize.errors = FALSE)

# NB MAX CORES (0 means Auto)
CORES <- ifelse(!is.null(conf$CORES), conf$CORES, 0)

# Rscript
RSCRIPT <- ifelse(!is.null(conf$RSCRIPT), conf$RSCRIPT, '')
if (nchar(RSCRIPT)==0) {
	if (OS == "windows") {
		V <- sessionInfo()
		RSCRIPT <-paste0("C:/Program Files/R/R-",V$R.version$major, ".", V$R.version$minor,"/bin/Rscript.exe")
	} else {
		RSCRIPT <- '/usr/bin/Rscript'
	}
}

# 7zip
ZIP7 <- ifelse(!is.null(conf$ZIP7), conf$ZIP7, '')
if (OS == "windows") {
	path <- tryCatch(
		readRegistry("SOFTWARE\\R-core", maxdepth = 3),
		error=function(e){NULL}
	)
	if (!is.null(path))
		RSCRIPT <- paste0(path$R$InstallPath,"\\bin\\Rscript.exe")
	if (nchar(RSCRIPT)) RSCRIPT <- paste0("\"",RSCRIPT,"\"")
	path <- tryCatch(
		readRegistry("SOFTWARE\\7-Zip", maxdepth = 3),
		error=function(e){NULL}
	)
	if (!is.null(path))
		ZIP7 <- paste0(path$Path64,"7z.exe")
	if (nchar(ZIP7)) ZIP7 <- paste0("\"",ZIP7,"\"")
}

# See https://shiny.rstudio.com/reference/shiny/1.4.0/upgrade.html
#options(shiny.jquery.version = 1)

# Dilution factor by default
fac_dilution <- ifelse(!is.null(conf$DILUTION_FAC), as.numeric(conf$DILUTION_FAC), 0.8)

if (nchar(ZIP7))
	zipext <- c('zip', '7z')
else
	zipext <- c('zip')

# Type names for Samples, QC, QS
QCQS <-c( ifelse(!is.null(conf$QC), conf$QC, 'QC'), ifelse(!is.null(conf$QS), conf$QS, 'QS') )
sampleTypes <- c( ifelse(!is.null(conf$SAMPLE), conf$SAMPLE, 'Sample'), QCQS )

# Spectra colors : original, model, residus
COLSPEC <- c('gray70','#86c1db','deeppink4')

# Compound colors
COLCPMDS <- c('#5ba8c9','dodgerblue1','#5b75c9','slateblue2','#8334b8','steelblue1')
