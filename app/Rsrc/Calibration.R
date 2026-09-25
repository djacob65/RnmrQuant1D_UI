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
	rv$calibreset <- rv$quantreset <-TRUE
	rv$endcalib <- rv$endproc <- FALSE
	shinyjs::disable("logButton")
	shinyjs::disable("calibReset")
	shinyjs::enable("calibButton")
	hideTab(inputId = "outtabs", target = "quant")
	hideTab(inputId = "outtabs", target = "viewer")
	for (widget in calib_widgets)
		shinyjs::enable(widget)
	LogFile <- file.path(rq1d$TMPDIR,'stds_QC-QS.txt')
	if (file.exists(LogFile)) unlink(LogFile)

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
# Show Calibration profile in a Modal Dialog Box
##---------------
observeEvent(input$viewCalibBtn, {
	calibfile <- calibprofile()
	if (!is.null(calibfile))
		gv$STDS_FILE <<- file.path(gv$outDir, 'profiles', calibfile)
	if (!is.null(gv$STDS_FILE)) {
		output$calibTable <- renderDT({
			STDS <- data.frame(read.table(gv$STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
			datatable(STDS)
		})
		showModal(modalDialog(
			title = "Calibration profile",
			tags$br(),
			DTOutput("calibTable"),
			downloadButton("bCprofile", "Download"),
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
# Export the current calculation profile
##---------------
output$bCprofile <- downloadHandler(
	filename = function() {
		calibfile <- calibprofile()
		if (!is.null(calibfile))
			gv$STDS_FILE <<- file.path(gv$outDir, 'profiles', calibfile)
		basename(gv$STDS_FILE)
	},
	content = function(file) {
		M <- data.frame(read.table(gv$STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
		write.table(M, file, sep = "\t", dec = ".", row.names = FALSE)
	}
)


##---------------
# Show Calibration logfile
##---------------
observeEvent(input$logButton, {
	calibObj()
	LogFile <- file.path(rq1d$TMPDIR,'stds_QC-QS.txt')
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
# Check Calibration &
# Launch the calculation of the PULCON factor for "QS-QC" types 
##---------------
output$outCalib <- renderPrint({
	req(input$calibButton)
	repeat {
		if (input$calibButton!=lstbtn$calib || rv$calibreset)
			break

		lstbtn$calib <<- lstbtn$calib + 1

		if (!gv$hasQCQS) {
			dispAlert2("No QC/QS-labeled spectra in the sample file, So no calibration or quantification, only integration is possible.")
			break
		}

		closeAlert(session, "AlertCalibId")

		obj <- calibObj()
		if (is.null(gv$STDS_FILE)) {
			dispAlert2("Error: No calibration profile provided")
			break
		}

		isolate({
			QSlist <- unique(gv$samples[ gv$samples$Type == QCQS[2] & gv$samples$Pulse==rq1d$SEQUENCE, 1])
			QClist <- unique(gv$samples[ gv$samples$Type == QCQS[1] & gv$samples$Pulse==rq1d$SEQUENCE, 1])
			Rscript_txt <- paste0(
				get_Rscript_for_calib('QSvar',rq1d$QStype, QSlist, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, append=FALSE, verbose=1),
				get_Rscript_for_calib('QCvar',rq1d$QCtype, QClist, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, append=TRUE, verbose=1)
			)
			rq1d$procParams$OPTPHC0 <- !input$optphc1;
			rq1d$procParams$OPTPHC1 <- input$optphc1;
		})

		out <- exe.catch({
			rq1d$check_calibration(verbose=TRUE)
		})

		if (out$error_occurred) {
			dispAlert2(out$message)
			break
		}

		shinyjs::disable("samplesReset")
		for (widget in calib_widgets)
			shinyjs::disable(widget)
		updateButton(session, "calibButton", label = " Launch Calibration", style = "warning", disabled = TRUE)

		shinyjs::runjs(paste0("document.getElementById('pb_calib').style.display = 'block';document.getElementById('calibmsg').textContent = 'Launch the calculation of the PULCON factor ...';"))
		session$sendCustomMessage("proc_status", TRUE)

		rv$running <- TRUE
		rv$endcalib <- FALSE
		rv$calib_output <- NULL

		gv$proctype <<- 'calib'
		rv$process_job <- submit_rq1d_calib(rq1d, gv, Rscript_txt)
		break
	}
})


##---------------
# Show Calibration details 
##---------------
output$outPulcon <- renderUI({
	req(rv$endcalib)
	ret <- FALSE
	repeat {
		res <- rv$calib_output
		if (is.null(res))
			break

		if (! "list" %in% class(res) || is.null(res$QC) || is.null(res$QS)) {
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
	req(rv$endcalib)
	if (! gv$hasQCQS || ! input$calibButton || rv$calibreset) return(NULL)
	if (gv$proctype !='calib' || is.null(rv$calib_output)) return(NULL)
	res <- rv$calib_output
	if (!is.null(res) && "list" %in% class(res) && !is.null(gv$STDS_FILE))
		rq1d$plot_QC_estimation(res$QC_tab)
})


