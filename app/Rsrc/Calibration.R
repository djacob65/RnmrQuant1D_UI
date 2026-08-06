#------------------------------------------------
# ID Calibration.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------

##---------------
## Alert Box 2
##---------------
dispAlert2 <- function(msg, title='', style='danger') {
	if (nchar(msg)>0) {
		createAlert(session, "AlertCalib", "AlertCalibId", title = title, content = msg, append = FALSE, style=style)
	}
}


##---------------
## QCQS condition
##---------------
output$fileQCQS <- reactive({
	input$outtabs
	return(gv$hasQCQS)
})
outputOptions(output, 'fileQCQS', suspendWhenHidden=FALSE)
outputOptions(output, 'fileQCQS', priority=10)


##---------------
## Obtain the calibration profile based on the selected source.
##---------------
calibprofile <- reactive ({
	if (isTRUE(input$externalCalib)) {
		namefile <- input$externCalibFile$name
		file.rename( input$externCalibFile$datapath, file.path(gv$outDir, 'profiles', namefile) )
		ret <- namefile
	} else {
		ret <- input$calibprofile
	}
	if (nchar(ret)==0) {
		ret <- NULL
	}
	return(ret)
})

##---------------
# Get QCname & QSname
##---------------
calibObj <- eventReactive(input$calibButton, {
	if (input$calibButton) {
		rq1d$SEQUENCE <<- input$sequence2
		calibfile <- calibprofile()
		if (!is.null(calibfile)) {
			gv$STDS_FILE <<- file.path(gv$outDir, 'profiles', calibfile)
			rq1d$CALIBRATION <<- data.frame(read.table(gv$STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
		} else {
			gv$STDS_FILE <<- NULL
		}
	}
})


##---------------
# Executes an expr, intercepting —where applicable— the message and interruption
##---------------
calib.exe.catch  <- function(expr) {
	out <- exe.catch({ expr })
	if (out$error_occurred) dispAlert2(out$message)
	out$result
}


##---------------
# Update Quantification profile list when choosing sequence
##---------------
observeEvent(input$sequence2, {
	if (gv$hasQCQS && rv$samples) {
		hideTab(inputId = "outtabs", target = "quant")
		hideTab(inputId = "outtabs", target = "viewer")
		lstfiles <- list.files( path = file.path(gv$outDir, 'profiles'), pattern = "^profile-", full.names = FALSE)
		lstfiles <- lstfiles[grep(input$sequence2, lstfiles)]
		lstfiles <- lstfiles[grep(rq1d$FIELD, lstfiles)]
		updateSelectInput(session, "quantprofile", label = "Quantification profile", choices = lstfiles)
	}
})


##---------------
# Reset management
##---------------
observeEvent(input$calibReset, {
	showModal(modalDialog(
		title = "Warning",
		"Changing this setting will clear the current results. Continue?",
		footer = tagList(
			actionButton("cancel_calib", "Cancel"),
			actionButton("confirm_calib", "Continue", class = "btn-danger")
		),
		easyClose = FALSE
	))
})

observeEvent(input$confirm_calib, {
	removeModal()
	rv$calibreset <- TRUE
	rv$quantreset <- TRUE
	shinyjs::disable("logButton")
	shinyjs::disable("calibReset")
	shinyjs::enable("calibButton")
	hideTab(inputId = "outtabs", target = "quant")
	hideTab(inputId = "outtabs", target = "viewer")
	for (widget in calib_widgets)
		shinyjs::enable(widget)
})

observeEvent(input$cancel_calib, {
	removeModal()
	rv$calibreset <- FALSE
	optionPulse <- unique(sort(gv$samples$Pulse))
	names(optionPulse) <- toupper(optionPulse)
	updateSelectInput(session, inputId='sequence2', label = 'Sequence (PULSE)', choices = optionPulse, selected=rq1d$SEQUENCE)
})

observeEvent(input$calibButton, {
	rv$calibreset <- FALSE
})


##---------------
# Compute PULCON factor for QC & QS
##---------------
calibResults <- eventReactive(input$calibButton, {
	calibObj()
	if (input$calibButton && ! is.null(gv$STDS_FILE)) {
		session$sendCustomMessage("proc_status", TRUE)
		shinyjs::disable("samplesReset")
		for (widget in calib_widgets)
			shinyjs::disable(widget)
		updateButton(session, "calibButton", label = "Launch Calibration", style = "warning", disabled = TRUE)

		OPTPHC0 <- rq1d$procParams$OPTPHC0
		OPTPHC1 <- rq1d$procParams$OPTPHC1
		rq1d$procParams$OPTPHC0 <<- !input$optphc1;
		rq1d$procParams$OPTPHC1 <<- input$optphc1;
		quantProfile <- rq1d$PROFILE

		repeat {
			t <- system.time({
				QSlist <- unique(gv$samples[ gv$samples$Type == QCQS[2] & gv$samples$Pulse==rq1d$SEQUENCE, 1])
				QS <- get_response_factors(rq1d, rq1d$QStype, QSlist, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, append=FALSE, verbose=1)
				if (is.null(QS)) break

				QClist <- unique(gv$samples[ gv$samples$Type == QCQS[1] & gv$samples$Pulse==rq1d$SEQUENCE, 1])
				QC <- get_response_factors(rq1d, rq1d$QCtype, QClist, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, append=TRUE, verbose=1)
				if (is.null(QC)) break	
			})
	
			shinyjs::runjs(paste0("document.getElementById('calibmsg').textContent = 'Waiting : Response factor for QC estimation ...';"))
			QS_df  <- calib.exe.catch({ rq1d$get_factor_table(QS) })
			QC_df  <- calib.exe.catch({ rq1d$get_factor_table(QC) })
			QC_tab <- calib.exe.catch({ rq1d$get_QC_estimation(QC, QS) })
			if (sum(is.na(QC_tab[,2]))==0)
				Yest <- lm(data=as.data.frame(QC_tab), Estimated~Real)
			else
				Yest <- NULL
	
			rq1d$fP <<- list(QSname=QSlist, Mat=QS_df, values=QS$fP, CV=QS$fPUL$CV, mean=QS$fPUL$mean, fK=QS$fK, elapsed=round(as.numeric(t[3]),2))
			shinyjs::runjs(paste0("document.getElementById('calibmsg').textContent = '';"))
			shinyjs::enable("quantButton")
			for (widget in quant_widgets)
				shinyjs::enable(widget)
			break
		}

		rq1d$procParams$OPTPHC0 <<- OPTPHC0;
		rq1d$procParams$OPTPHC1 <<- OPTPHC1;
		rq1d$PROFILE <<- quantProfile

		updateButton(session, "calibButton", label = "Launch Calibration", style = "info", disabled = TRUE)
		shinyjs::enable("logButton")
		shinyjs::enable("samplesReset")
		shinyjs::enable("calibReset")

		if (!is.null(QC) && !is.null(QS))
			list(QS=QS, QC=QC, QS_df=QS_df, QC_df=QC_df, QC_tab=QC_tab, Yest=Yest)
		else
			NULL
	}
})


##---------------
# Show Calibration profile in a Modal Dialog Box
##---------------
observeEvent(input$viewCalibBtn, {
	if (is.null(gv$STDS_FILE)) {
		calibfile <- calibprofile()
		if (!is.null(calibfile))
			gv$STDS_FILE <<- file.path(gv$outDir, 'profiles', calibfile)
	}
	if (!is.null(gv$STDS_FILE)) {
		output$calibTable <- renderDT({
			STDS <- data.frame(read.table(gv$STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
			datatable(STDS)
		})
		showModal(modalDialog(
			title = "Calibration profile",
			tags$br(),
			DTOutput("calibTable"),
			tags$br(),tags$br(),
			HTML("See "),
			tags$a("Calibration profile", target = "_blank", href = urls_doc$CALIBDOC), 
			HTML(" in the online documentation"),
			tags$br(),tags$br(),
			easyClose = TRUE,
			footer = modalButton("Close"),
			size = "l"
		))
	}
})


##---------------
# Show Calibration logfile
##---------------
observeEvent(input$logButton, {
	calibObj()
	#stds_file <- list.files(file.path(gv$outDir,'tmp/log'), pattern='stds_.+\\.txt')[1]
	stds_file <- 'stds_QC-QS.txt'
	LogFile <- file.path(rq1d$TMPDIR,stds_file)
	if (file.exists(LogFile)) {
		content <- readLines(LogFile, warn = FALSE)
		showModal(modalDialog(
			title = "Calibration log",
			tags$pre(paste(content, collapse = "\n")),
			easyClose = TRUE,
			footer = modalButton("Close"),
			size = "l"
		))
	}
})


##---------------
# Check Calibration 
##---------------
output$outCalib <- renderPrint({
	if (gv$hasQCQS) {
		if (input$calibButton && ! rv$calibreset) {
			closeAlert(session, "AlertCalibId")
			obj <- calibObj()
			if (!is.null(gv$STDS_FILE))
				out <- calib.exe.catch({
					rq1d$check_calibration(verbose=TRUE)
				})
		}
	} else {
		"No QC/QS-labeled spectra in the sample file, So no calibration or quantification, only integration is possible."
	}
})


##---------------
# Show Calibration details 
##---------------
output$outPulcon <- renderUI({
	ret <- FALSE
	repeat {
		if (!gv$hasQCQS || ! input$calibButton || rv$calibreset)
			break

		if (is.null(gv$STDS_FILE))
			break

		closeAlert(session, "AlertIntgId")

		obj <- calibObj()
		if (is.null(gv$STDS_FILE)) {
			dispAlert2("Error: No calibration profile provided")
			break
		}

		#  Compute PULCON factors for QC & QS
		res <- calibResults()
		if (is.null(res)) {
			dispAlert2("Error: something went wrong")
			break
		}

		if (sum(is.na(res$QC_tab[,2]))>0) {
			dispAlert2("Error: CV threshold seems to low !")
			hideTab(inputId = "outtabs", target = "quant")
			tagList(
				tags$span(style = "color: red; font-weight: bold; font-size: 120%;", "An error occured !")
			)
			break
		}
		if ( nrow(gv$samples[ ! gv$samples$Type %in% QCQS, ])>0 )
			showTab(inputId = "outtabs", target = "quant")
		ret <- TRUE
		break
	}

	if (ret) {
		tagList(
			tags$span(style = "color: #2c7be5; font-weight: bold; font-size: 120%;", "PULCON Factor for QS"),
			tags$pre(paste(capture.output(res$QS_df), collapse = "\n")),
			tags$pre(paste(capture.output(res$QS$fPUL), collapse = "\n")),
			tags$br(), tags$br(),
			tags$span(style = "color: #2c7be5; font-weight: bold; font-size: 120%;", "PULCON Factor for QC"),
			tags$pre(paste(capture.output(res$QC_df), collapse = "\n")),
			tags$pre(paste(capture.output(res$QC$fPUL), collapse = "\n")),
			tags$br(), tags$br(),
			tags$span(style = "color: #2c7be5; font-weight: bold; font-size: 120%;", "QC estimation"),
			tags$pre(paste(capture.output(res$QC_tab), collapse = "\n")),
			tags$pre(paste('R2 =', round( cor(res$QC_tab[,1], res$QC_tab[,2]), 5))),
			tags$pre(paste('Rate =', round(coef(res$Yest)[2],4), ', Intercept =', round(coef(res$Yest)[1],4))),
			tags$br()
		)
	}
})


##---------------
# PLot QC estimation
##---------------
output$QC_estimation <- renderPlotly({
	if (! gv$hasQCQS || ! input$calibButton || rv$calibreset) return(NULL)
	res <- calibResults()
	if (!is.null(res) && !is.null(gv$STDS_FILE))
		rq1d$plot_QC_estimation(res$QC_tab)
})


