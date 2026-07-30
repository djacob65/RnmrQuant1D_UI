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
		PROFILE = NULL,             # file name of the quantification profile
		STDS_FILE =NULL,              # file name of the calibration profile
		max_ncpu = CORES            # Number of cores for parallel computing
	)

	# Reactive values
	rv <- reactiveValues(
		load = 0,                   # Files are loaded
		okws = 0,                   # Workspace is OK
		samples = 0,                # Samples Table is OK
		endproc = FALSE,            # End of processing
		reset = FALSE,              # Reset all
		intgreset = FALSE,          # Reset Integration
		calibreset = FALSE,         # Reset Calibration & Quantification
		quantreset = FALSE,         # Reset Quantification
		process_job = NULL          # processx object
	)

	# Pressure counters on the "Import / Launch" buttons
	lstbtn <- list(load=1, calib=1, intg=1, quant=1)

	# RnmrQuant1D instance
	rq1d <- NULL
	res <- NULL

	# List of widgets by category – useful for enabling or disabling them during reset or running
	intg_widgets <- c('sequence','externalIntg','intgprofile','externIntgFile','listcmpds','intgInvBtn','intgpattern','listsamples')
	calib_widgets <- c('sequence2', 'deconv', 'optphc1', 'thresfP', 'qbl', 'externalCalib', 'calibprofile', 'externCalibFile')
	quant_widgets <- c('externalQuant','quantprofile','externQuantFile','quantInvBtn','quantgpattern','listsamples2','listcmpds2')

	# Load source code
	source("Rsrc/Upload.R", local=TRUE)           # Upload files
	source("Rsrc/Samples.R", local=TRUE)          # Samples tab
	source("Rsrc/Calibration.R", local=TRUE)      # Calibration tab
	source("Rsrc/Integration.R", local=TRUE)      # Integration tab
	source("Rsrc/Quantification.R", local=TRUE)   # Quantification tab
	source("Rsrc/Viewer.R", local=TRUE)           # Spectra Viewer tab


	# --------------------------
	# Handle application reload events
	# --------------------------

	observeEvent(input$resetBtn1, {
		if(! is.null(input$zipfile)) {
			rv$reset <- TRUE
			message(paste(date(),": Reload Session ..."))
			session$reload()
		}
	})

	session$onSessionEnded(function() {
		if (isolate(rv$reset))
			empty_directory(gv$outDir)
	})


	# --------------------------
	# Handle application closure
	# --------------------------
	observeEvent(rv$endproc, {
		session$sendCustomMessage("proc_status", rv$endproc)
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
