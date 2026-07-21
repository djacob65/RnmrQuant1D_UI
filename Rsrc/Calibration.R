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
	return(ret)
})

##---------------
# Get QCname & QSname
##---------------
calibObj <- eventReactive(input$calibButton, {
	if (input$calibButton) {
		gv$STDS_FILE <<- file.path(gv$outDir, 'profiles', calibprofile())
		rq1d$SEQUENCE <<- input$sequence2
		rq1d$CALIBRATION <<- data.frame(read.table(gv$STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
		QCname <- unique(sort(gv$samples[gv$samples$Type==rq1d$QCtype, 1]))[1]
		QSname <- unique(sort(gv$samples[gv$samples$Type==rq1d$QStype, 1]))[1]
		list(QCname=QCname, QSname=QSname)
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
# Compute PULCON factor for QC & QS
##---------------
calibResults <- eventReactive(input$calibButton, {
	obj <- calibObj()
	if (input$calibButton) {
		QS <- calib.exe.catch({
			rq1d$get_response_factors(rq1d$QStype, obj$QSname, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, verbose=1)
		})
		QC <- calib.exe.catch({
			rq1d$get_response_factors(rq1d$QCtype, obj$QCname, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, verbose=1)
		})
		QS_df  <- calib.exe.catch({ rq1d$get_factor_table(QS) })
		QC_df  <- calib.exe.catch({ rq1d$get_factor_table(QC) })
		QC_tab <- calib.exe.catch({ rq1d$get_QC_estimation(QC, QS) })
		if (sum(is.na(QC_tab[,2]))==0)
			Yest <- lm(data=as.data.frame(QC_tab), Estimated~Real)
		else
			Yest <- NULL
		updateButton(session, "logButton", label = "Log", style = "info", disabled = FALSE)
		list(QCname=obj$QCname, QSname=obj$QSname, QS=QS, QC=QC, QS_df=QS_df, QC_df=QC_df, QC_tab=QC_tab, Yest=Yest)
	}
})


##---------------
# Calibration profile as a DataTable
##---------------
output$calibTable <- renderDT({
	if (! gv$hasQCQS ) return(NULL)
	STDS_FILE <- file.path(gv$outDir, 'profiles', calibprofile())
	STDS <- data.frame(read.table(STDS_FILE, header=T, sep="\t", dec=".", stringsAsFactors=F))
	datatable(STDS)
})


##---------------
# Show Calibration profile in a Modal Dialog Box
##---------------
observeEvent(input$viewCalibBtn, {
	showModal(modalDialog(
		title = "Calibration profile",
		DTOutput("calibTable"),
		easyClose = TRUE,
		footer = modalButton("Close"),
		size = "l"
	))
})


##---------------
# Show Calibration logfile
##---------------
observeEvent(input$logButton, {
	obj <- calibObj()
	LogFile <- paste0(rq1d$TMPDIR,'/stds_',obj$QSname,'-',rq1d$SEQUENCE,'.txt')
	content <- readLines(LogFile, warn = FALSE)
	showModal(modalDialog(
		title = "Calibration log",
		tags$pre(paste(content, collapse = "\n")),
		easyClose = TRUE,
		footer = modalButton("Close"),
		size = "l"
	))
})


##---------------
# Check Calibration 
##---------------
output$outCalib <- renderPrint({
	if (gv$hasQCQS) {
		if (input$calibButton) {
			closeAlert(session, "AlertCalibId")
			obj <- calibObj()
			out <- calib.exe.catch({
				rq1d$check_calibration(obj$QCname, obj$QSname, verbose=TRUE)
			})
		}
	} else {
		"No QC/QS-labeled spectra in the sample file, So no calibration or quantification, only integration."
	}
})


##---------------
# Show Calibration details 
##---------------
output$outPulcon <- renderUI({
	if (gv$hasQCQS && input$calibButton) {
		res <- calibResults()
		if (sum(is.na(res$QC_tab[,2]))>0) {
			dispAlert2("Error: CV threshold seems to low !")
			hideTab(inputId = "outtabs", target = "quant")
			tagList(
				tags$span(style = "color: red; font-weight: bold; font-size: 120%;", "An error occured !")
			)
		} else {
			isolate({
				# Compute PULCON factor
				out <- exe.catch({
					rq1d$proc_fPULCON(res$QSname, thresfP=input$thresfP, deconv=input$deconv, qbl=input$qbl, verbose=2)
				})
				if (out$error_occurred) {
					dispAlert2(out$message)
					hideTab(inputId = "outtabs", target = "quant")
				} else {
					showTab(inputId = "outtabs", target = "quant")
				}
			})
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
	}
})


##---------------
# PLot QC estimation
##---------------
output$QC_estimation <- renderPlotly({
	if (! gv$hasQCQS || ! input$calibButton) return(NULL)
	res <- calibResults()
	rq1d$plot_QC_estimation(res$QC_tab)
})


