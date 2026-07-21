#------------------------------------------------
# ID Upload.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


##---------------
## Alert Box 1
##---------------
dispAlert1 <- function(msg, title='', style='danger') {
	if (nchar(msg)>0) {
		createAlert(session, "AlertUpLoad", "AlertUpLoadId", title = title, content = msg, append = FALSE, style=style)
	}
}


##---------------
## Instrument/Vendor/Format selected
##---------------
output$FormatSelected <- reactive({
	if (input$vendor=="sinput") return(0)
	return(1)
})
outputOptions(output, 'FormatSelected', suspendWhenHidden=FALSE)
outputOptions(output, 'FormatSelected', priority=20)


##---------------
## Uploading Zip file
##---------------
output$ZipUploaded <- reactive({
	Sys.sleep(1)
	if(is.null(input$zipfile)) return(0)
	if (! is.null(input$zipfile) && is.null(input$samplefile)) {
		shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'block';" )
		ErrMsg <- ''
		closeAlert(session, "AlertUpLoadId")
		# Get & Rename the ZIP file
		gv$Vendor <<- input$vendor
		zipfile <- input$zipfile
		gv$NameZip <<- zipfile$name
		gv$outDir <<- tempdir()
		ext <- tolower(gsub("^.*\\.", "", gv$NameZip))
		gv$RawZip <<- file.path(gv$outDir,paste0('raw.',ext))
		file.rename( zipfile$datapath, gv$RawZip )
		unlink(dirname(zipfile$datapath), recursive=TRUE)
		repeat {
			# Check if is an archive file
			if ( !(ext %in% zipext)) {
				ErrMsg <- paste0("ERROR: The ZIP file must have an appropriate extension (",
					paste(zipext, collapse=","),").")
				break
			}
			# Unzip RawZip
			tryCatch({
				RAWDIR <- dirname(gv$RawZip)
				if (ext=='7z') {
					system(paste0("cd ",RAWDIR,"; ",conf$ZIP7," x -y ",gv$RawZip))
				} else {
					unzip(gv$RawZip, files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = RAWDIR, unzip = "internal", setTimes = FALSE)
				}
			}, error=function(e) {
				ErrMsg <- "ERROR: Extraction failed!"
			})
			break
		}
		shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'none';" )
		unlink(gv$RawZip)
		if (nchar(ErrMsg)>0) {
			dispAlert1(ErrMsg)
			return(0)
		} else {
			shinyjs::disable('vendor')
			shinyjs::disable('zipfile')
		}
	}
	return( ifelse( ! is.null(input$zipfile) , 1, 0 ) )
})
outputOptions(output, 'ZipUploaded', suspendWhenHidden=FALSE)
outputOptions(output, 'ZipUploaded', priority=20)


##---------------
## Check if all files are uploaded
##---------------
output$allUploaded <- reactive({
	Sys.sleep(1)
	ret <- is.null(input$zipfile) || is.null(input$samplefile)
	return( ifelse( ret, 0, 1 ) )
})
outputOptions(output, 'allUploaded', suspendWhenHidden=FALSE)
outputOptions(output, 'allUploaded', priority=20)


##---------------
## Check if the 'only integration' is required
##---------------
observeEvent(input$samplefile, {
	if (! is.null(input$zipfile) && ! is.null(input$samplefile)) {
		samples <- open_samples_file(input$samplefile$datapath)
		if (! "character" %in% class(samples))
			if (! 'Type' %in% colnames(samples) || sum(c('QS','QS') %in% samples$Type)==0)
				updateCheckboxInput(session, "onlyintg", "Only integration", value = TRUE)
	}
})


##---------------
## After loading all files
##---------------
output$fileUploaded <- reactive({
	input$goButton
	rv$loadbtn
	if (input$goButton<rv$loadbtn) return(0)
	if ( is.null(input$zipfile) || (!input$onlyintg && is.null(input$samplefile)) ) return (0)
	closeAlert(session, "AlertUpLoadId")
	isolate({
		if (input$onlyintg && is.null(input$samplefile)) {
			msg <- ''
			samples <- tryCatch({
				get_samples_metadata(gv$outDir, input$vendor, input$onlyintg)
			}, error=function(cond) {
				msg <- paste(cond, collapse="\n")
			})
			if (nchar(msg)) {
				dispAlert1(msg)
				rv$loadbtn <- rv$loadbtn + 1
				rv$load <- 0
				return(0)
			}
			gv$SampleFilename <<- 'samples.txt'
			gv$SampleFile <<- file.path(gv$outDir,'samples.txt')
			write.table(samples, gv$SampleFile, sep="\t", dec='.', row.names=FALSE)
		} else {
			# Get & Rename the sample file
			samplefile <- input$samplefile
			gv$SampleFilename <<- samplefile$name
			ext <- ifelse(is_xlsx(samplefile$datapath), 'xlsx', 'txt')
			gv$SampleFile <<- file.path(gv$outDir,paste0('samples.',ext))
			file.rename( samplefile$datapath, gv$SampleFile )
			unlink(dirname(samplefile$datapath), recursive=TRUE)
		}
	})
	rv$load <- input$goButton==rv$loadbtn
	return(rv$load)
})
outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
outputOptions(output, 'fileUploaded', priority=1)


##---------------
## Samples checking & Spectra preparation
##---------------
output$fileProcessed <- reactive({
	if (rv$load==0) return (0)
	if ( is.null(gv$RawZip) ) return (0)
	updateButton(session, "goButton", label = "Launch", style = "warning", disabled = TRUE)
	shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'block';" )
	rv$unzip <- ret <- 0
	closeAlert(session, "AlertUpLoadId")

	repeat
	{
		# Create a new instance and get sample list
		# and in this way, initialize internally the sample list in RAWDIR directory
		rq1d <<- RnmrQuant1D$new(gv$Vendor)
		priv <<- rq1d$.__enclos_env__$private
		rq1d$RAWDIR <<- gv$outDir
		gv$hasQCQS  <<- FALSE

		# Check sample list provided by the user
		gv$samples <<- open_samples_file(gv$SampleFile)
		if ("character" %in% class(gv$samples)) {
			msg <- gv$samples
		} else {
			msg <- check_samples_metadata(rq1d, gv$samples, gv$outDir, gv$Vendor, input$onlyintg)
		}
		if (nchar(msg)) {
			dispAlert1(msg)
			rv$loadbtn <- rv$loadbtn + 1
			rv$load <- 0
			break
		}

		# If quantification, separate spectra directories from QC-QS directories
		if (! input$onlyintg) {
			# Split the samples according to their type
			dir.create(file.path(gv$outDir, 'samples'), showWarnings = FALSE)
			dir.create(file.path(gv$outDir, 'QC-QS'), showWarnings = FALSE)
			df <- priv$get_list_spectrum(gv$outDir, priv$get_list_samples(gv$outDir))
			for(k in 1:nrow(gv$samples)) {
				spectrum_path <- df[ which(df[,1] == gv$samples[k,1])[1], 5 ]
				if (gv$Vendor == 'bruker')
					spectrum_path <- dirname(spectrum_path)
				if (gv$Vendor != 'jeol' && dir.exists(spectrum_path)) {
					path_type <- ifelse (as.character(gv$samples[k,]$Type) %in% QCQS, 'QC-QS', 'samples')
					path_new <- file.path(gv$outDir, path_type, gv$samples[k,1])
					dir.create(path_new, showWarnings = FALSE)
					copy_folder(spectrum_path, path_new)
					unlink(spectrum_path, recursive=TRUE)
				}
				if (gv$Vendor == 'jeol' && file.exists(spectrum_path)) {
					path_type <- ifelse (as.character(gv$samples[k,]$Type) %in% QCQS, 'QC-QS', 'samples')
					path_new <- file.path(gv$outDir, path_type, gv$samples[k,1])
					file.copy(spectrum_path, path_new)
					unlink(spectrum_path, recursive=TRUE)
				}
			}
			# Populate RnmrQuant1D object
			rq1d$QCtype <<- QCQS[1]
			rq1d$QStype <<- QCQS[2]
			rq1d$QSDIR  <<- file.path(gv$outDir, 'QC-QS')
			rq1d$RAWDIR <<- file.path(gv$outDir, 'samples')
			gv$hasQCQS  <<- sum(gv$samples$Type %in% QCQS)>0
			rq1d$RAWDIR_SLIST <<- NULL # Reset the internal sample list in RAWDIR directory
		}

		# Profile directory
		dir.create(file.path(gv$outDir, 'profiles'), showWarnings = FALSE)
		copy_folder(file.path(getwd(),'www/profiles'), file.path(gv$outDir, 'profiles'))

		# Temporary directory for log files
		rq1d$TMPDIR <<- file.path(gv$outDir,'tmp/log')
		if (! dir.exists(rq1d$TMPDIR) )
			dir.create(rq1d$TMPDIR,  recursive = TRUE, showWarnings = FALSE)

		# Temporary directory for RData files
		rq1d$RDATADIR <<- file.path(gv$outDir, 'tmp/RData')
		if (! dir.exists(rq1d$RDATADIR) )
			dir.create(rq1d$RDATADIR,  recursive = TRUE, showWarnings = FALSE)

		# Temporary directory for image files
		gp$IMGDIR <<- file.path(gv$outDir,'tmp/img')
		if (! dir.exists(gp$IMGDIR) )
			dir.create(gp$IMGDIR,  recursive = TRUE, showWarnings = FALSE)

		# Ready to move on to the next step
		rv$unzip <- ret <- 1
		break
	}

	updateButton(session, "goButton", label = "Launch", style = "info", disabled = FALSE)
	shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'none';" )
	return(ret)
})
outputOptions(output, 'fileProcessed', suspendWhenHidden=FALSE)
outputOptions(output, 'fileProcessed', priority=10)


##---------------
## Export the template file
##---------------
output$exportTMPL <- downloadHandler(
	filename = function() { paste0('samples_',gsub("\\.\\S+$","",basename(gv$NameZip)), '.txt' ) },
	content = function(file) {
		shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'block';" )
		samples <- get_samples_metadata(gv$outDir, input$vendor, input$onlyintg)
		write.table(samples, file, quote = FALSE, sep = "\t",  dec = ".", row.names = FALSE)
		shinyjs::runjs( "document.getElementById('waitbox1').style.display = 'none';" )
	}
)
