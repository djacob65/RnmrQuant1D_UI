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
	library(colourpicker)
	library(htmlwidgets)
	library(htmltools)
	library(markdown)
	library(plotly)
	library(DT)
	library(openxlsx)
	library(processx)
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

# DEV mode
DEV <- conf$DEV

# Max size for the ZIP file
MAXZIPSIZE <- 100
if (is.numeric(conf$MAXZIPSIZE) && as.numeric(conf$MAXZIPSIZE)>0) {
    MAXZIPSIZE <- as.numeric(conf$MAXZIPSIZE)
}
options(shiny.maxRequestSize=MAXZIPSIZE*1024^2)
options(shiny.sanitize.errors = FALSE)

# NB MAX CORES (0 means Auto)
CORES <- ifelse(!is.null(conf$CORES), conf$CORES, 0)

# Set affinity
AFFINITY <- ifelse(!is.null(conf$AFFINITY), conf$AFFINITY, 0)

# Rscript
RSCRIPT <- ifelse(!is.null(conf$RSCRIPT), conf$RSCRIPT, '')

# 7zip
ZIP7 <- ifelse(!is.null(conf$ZIP7), conf$ZIP7, '')

if (OS == "windows") {
	path <- tryCatch(
		readRegistry("SOFTWARE\\R-core", maxdepth = 3),
		error=function(e){NULL}
	)
	if (!is.null(path))
		RSCRIPT <- paste0(path$R$InstallPath,"\\bin\\Rscript.exe")
	path <- tryCatch(
		readRegistry("SOFTWARE\\7-Zip", maxdepth = 3),
		error=function(e){NULL}
	)
	if (!is.null(path))
		ZIP7 <- paste0(path$Path64,"7z.exe")
}

# Dilution factor by default
DIL_FAC <- ifelse(!is.null(conf$DILUTION_FAC), as.numeric(conf$DILUTION_FAC), 0.8)

if (nchar(ZIP7))
	zipext <- c('zip', '7z')
else
	zipext <- c('zip')

# Type names for Samples, QC, QS
QCtype <- ifelse(!is.null(conf$QC), conf$QC, 'QC')
QStype <- ifelse(!is.null(conf$QS), conf$QS, 'QS')
SAMPLEtype <- ifelse(!is.null(conf$SAMPLE), conf$SAMPLE, 'Sample')
QCQS <-c( QCtype, QStype )
sampleTypes <- c( SAMPLEtype, QCQS )

# Online documentation
urls_doc <- list(
	CALIBDOC = conf$CALIBDOC,
	QUANTDOC = conf$QUANTDOC
)

# Spectra colors : original, model, residus
COLSPEC <- c('gray70','#86c1db','deeppink4')

# Compound colors
COLCPMDS <- c('#5ba8c9','dodgerblue1','#5b75c9','slateblue2','#8334b8','#A6B03A')

# Message Log file of the core process
OUTLOG <- 'rq1d.out'

# Message file serving also as a semaphore
ENDFILE <- 'ended.out'
