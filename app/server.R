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
	#set.seed(123)
	session$allowReconnect("force")

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
		fac_dilution = DIL_FAC,     # Default dilution factor value
		hasQCQS = FALSE,            # TRUE if QC/QS spectra are provided
		PROFILE = NULL,             # file name of the quantification profile
		STDS_FILE =NULL,            # file name of the calibration profile
		affinity = AFFINITY,        # CPU affinity
		max_ncpu = CORES,           # Max number of cores for parallel computing
		ncpu = 0                    # Number of cores used for parallel computing
	)

	# Reactive values
	rv <- reactiveValues(
		Logged = USER_LOGGED,       # Is User logged ? 
		load = 0,                   # Files are loaded
		okws = 0,                   # Workspace is OK
		samples = 0,                # Samples Table is OK
		endproc = FALSE,            # End of processing
		reset = FALSE,              # Reset all
		intgreset = FALSE,          # Reset Integration
		calibreset = FALSE,         # Reset Calibration
		quantreset = FALSE,         # Reset Quantification
		process_job = NULL,         # processx object
		running = FALSE,            # Job state
		job_output = NULL,          # Job output
		n_logs = 0                  # Number of processed samples 
	)

	# Pressure counters on the "Import / Launch" buttons
	lstbtn <- list(load=1, calib=1, intg=1, quant=1)

	# RnmrQuant1D instance
	rq1d <- NULL

	# Timestamping to measure the task execution time.
	start.time <- 0

	# List of widgets by category – useful for enabling or disabling them during reset or running
	intg_widgets <- c('sequence','externalIntg','intgprofile','externIntgFile','listcmpds','intgInvBtn','intgpattern','listsamples')
	calib_widgets <- c('sequence2', 'deconv', 'optphc1', 'thresfP', 'qbl', 'externalCalib', 'calibprofile', 'externCalibFile')
	quant_widgets <- c('externalQuant','quantprofile','externQuantFile','quantInvBtn','quantpattern','listsamples2','listcmpds2')

	# Load source code
	source("Rsrc/Login.R", local=TRUE)            # Log in module
	source("Rsrc/Upload.R", local=TRUE)           # Upload files
	source("Rsrc/Samples.R", local=TRUE)          # Samples tab
	source("Rsrc/Calibration.R", local=TRUE)      # Calibration tab
	source("Rsrc/Integration.R", local=TRUE)      # Integration tab
	source("Rsrc/Quantification.R", local=TRUE)   # Quantification tab
	source("Rsrc/Process.R", local=TRUE)          # Process tracking
	source("Rsrc/Viewer.R", local=TRUE)           # Spectra Viewer tab
	source("Rsrc/Params.R", local=TRUE)           # Set parameters


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
		if (!isolate(rv$reset)) {
			unlink(gv$outDir, recursive = TRUE)
			unlink(tempdir(), recursive = TRUE)
			if (!isShinyServer()) stopApp()
		} else {
			empty_directory(gv$outDir)
		}
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
		if (length(lparams)>0)
			gv$sessid <<- lparams[1]

		if (nchar(gv$sessid)==0)
			gv$sessid <<- paste0('_',paste(sample(c(0:9, letters[1:6]),30, replace=TRUE),collapse=""))

		gv$outDir <<- file.path(dirname(tempdir()), gv$sessid)

		if (dir.exists(gv$outDir))
			empty_directory(gv$outDir)
		else
			dir.create(gv$outDir, showWarnings = FALSE)

		#shinyjs::runjs( paste0("window.history.replaceState(null,'', '?", gv$sessid, "');") )
	})


	##---------------
	## SessInit: Is User logged ? 
	##---------------
	output$SessInit <- reactive({
		sessinit <- rv$Logged == TRUE
		return(sessinit)
	})
	outputOptions(output, 'SessInit', suspendWhenHidden=FALSE)
	outputOptions(output, 'SessInit', priority=1)

}
