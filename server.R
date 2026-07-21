#------------------------------------------------
# ID server.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


#---------------------
# Server
#---------------------

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session)
{
	set.seed(123)

	# Gobal variables
	gv <- list(
		sessid = '',                # Session Identifier
		Vendor = NULL,              # Instrument/Vendor/Format
		outDir = NULL,              # directory to output the resulting files
		RawZip = NULL,              # the full path name of the uploaded ZIP file (raw.zip)
		NameZip = NULL,             # the name of the uploaded ZIP file
		SampleFile = NULL,          # the full path name of the uploaded Sample file
		SampleFilename = '',        # the name of the uploaded Sample file
		samples = NULL,             # the sample matrix
		hasQCQS = FALSE,            # TRUE if QC/QS spectra are provided
		PROFILE = '',               # file name of the quantification profile
		STDS_FILE ='',              # file name of the calibration profile
		max_ncpu = CORES            # Number of cores for parallel computing
	)

	# Reactive values
	rv <- reactiveValues()
	rv$load <- 0
	rv$unzip <- 0
	rv$loadbtn <- 1
	rv$samples <- 0
	rv$endproc <- FALSE
	rv$reset <- FALSE

	lstbtn <- list(intg=1, quant=1)

	rq1d <- NULL
	res <- NULL

	source("Rsrc/Upload.R", local=TRUE)           # Upload files
	source("Rsrc/Samples.R", local=TRUE)          # Samples tab
	source("Rsrc/Calibration.R", local=TRUE)      # Calibration tab
	source("Rsrc/Integration.R", local=TRUE)      # Integration tab
	source("Rsrc/Quantification.R", local=TRUE)   # Quantification tab
	source("Rsrc/Viewer.R", local=TRUE)           # Spectra Viewer tab


	# --------------------------
	# Handle application reload/stop events
	# --------------------------

	observeEvent(c(input$resetBtn1, input$resetBtn2), {
		if(! is.null(input$zipfile)) {
			rv$reset <- TRUE
			message(paste(date(),": Reload Session ..."))
			session$reload()
		}
	})

	session$onSessionEnded(function() {
		if (!isolate(rv$reset)) {
			stopApp()
		} else {
			empty_directory(gv$outDir)
		}
	})

	# --------------------------
	# Gets Session Idenfier => SID
	# --------------------------
	observe({
		cdata <- session$clientData
		lparams <- unlist(strsplit(gsub("\\?", "", cdata[['url_search']]),  '&'))
		if (length(lparams)>0) {
			gv$sessid <<- lparams[1]
		}
		if (nchar(gv$sessid)==0) {
			gv$sessid <<- paste0('_',paste(sample(c(0:9, letters[1:6]),30, replace=TRUE),collapse=""))
		}
		shinyjs::runjs( paste0("window.history.replaceState(null,'RnmrQuant1D', '?", gv$sessid, "');") )
	})

	# --------------------------
	# Manage Tabs
	# --------------------------
	observe({
		c( input$onlyintg )
		hideTab(inputId = "outtabs", target = "intg")
		hideTab(inputId = "outtabs", target = "calib")
		hideTab(inputId = "outtabs", target = "quant")
		hideTab(inputId = "outtabs", target = "viewer")
		if (input$onlyintg) {
			showTab(inputId = "outtabs", target = "intg")
		} else {
			showTab(inputId = "outtabs", target = "calib")
		}
	})

}
