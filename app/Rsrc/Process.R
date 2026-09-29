#------------------------------------------------
# ID Process.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------

##---------------
# Export the current profile, including any potential modifications
##---------------
output$bQprofile <- downloadHandler(
	filename = function() { 
		quantfile <- quantprofile()
		if (!is.null(quantfile))
			gv$PROFILE <<- file.path(gv$outDir, 'profiles', quantfile)
		paste0(gsub("\\..*$", "",basename(gv$PROFILE)),'.tsv')
	},
	content = function(file) {
		rq1d$saveProfile(file)
	}
)

##---------------
#  Show / hide "Spectra Viewer" depending on processing status
##---------------
observeEvent(rv$endproc, {
	if (gv$proctype %in% c('intg','quant')) {
		if (rv$n_logs>0 && ((rv$endproc && !input$onlyintg) || (rv$endproc && input$onlyintg && rv$n_logs==nrow(rq1d$SAMPLES))))
			showTab(inputId = "outtabs", target = "viewer")
		else
			hideTab(inputId = "outtabs", target = "viewer")
	}
})


# --------------------------
# Reactive value of the job state for UI
# --------------------------
output$running <- reactive({
	rv$running
})
outputOptions(output, 'running', suspendWhenHidden=FALSE)
outputOptions(output, 'running', priority=20)


# --------------------------
# Process management of the unzipping uploaded file
# --------------------------
unzip_process_job <- function()
{
	p <- isolate(rv$process_job)
	if (!p$is_alive()) {
		rv$running <- FALSE
		rv$endunzip <- TRUE
		rv$process_job <- NULL 
	}
}


# --------------------------
# Process management of the calibration
# --------------------------
calib_process_job <- function()
{
	isolate({
		# Job is ended
		if (!rv$process_job$is_alive()) {
			res <- readRDS(file = file.path(gv$outDir,'calib.rds'))
			QS_df <- QC_df <- QC_tab <- Yest <- NULL
			if (!is.null(res$QS))
				QS_df <- calib.exe.catch({ rq1d$get_factor_table(res$QS) })
			if (!is.null(res$QC))
				QC_df <- calib.exe.catch({ rq1d$get_factor_table(res$QC) })
			if (!is.null(res$QC) && !is.null(res$QS)) {
				QC_tab <- calib.exe.catch({ rq1d$get_QC_estimation(res$QC, res$QS) })
				if (sum(is.na(QC_tab[,2]))==0)
					Yest <- lm(data=as.data.frame(QC_tab), Estimated~Real)
			}

			rq1d$fP <<- list(Mat=QS_df, values=res$QS$fP, CV=res$QS$fPUL$CV, mean=res$QS$fPUL$mean, 
				fK=res$QS$fK, elapsed=round(as.numeric(res$time[3]),2))

			if (!is.null(res$QC) && !is.null(res$QS))
				rv$calib_output <- list(QS=res$QS, QC=res$QC, QS_df=QS_df, QC_df=QC_df, QC_tab=QC_tab, Yest=Yest)

			shinyjs::runjs(paste0("document.getElementById('pb_calib').style.display = 'none';"))
			shinyjs::enable("quantButton")
			for (widget in quant_widgets)
				shinyjs::enable(widget)

			session$sendCustomMessage("proc_status", TRUE)
			updateButton(session, "calibButton", label = " Launch Calibration", style = "info", disabled = TRUE)
			shinyjs::enable("logButton")
			shinyjs::enable("samplesReset")
			shinyjs::enable("calibReset")

			rv$running <- FALSE
			rv$endcalib <- TRUE
		}

		# Job is running : update progress bar
		else {
			QCQSLOG <- file.path(rq1d$TMPDIR,'qc-qs_infos.txt')
			if (file.exists(QCQSLOG)) {
				shinyjs::runjs(paste0("document.getElementById('calibmsg').textContent = '",readLines(QCQSLOG)[1],"';"))
			}
		}
	})
}


# --------------------------
# Process management of the integration/quantification
# --------------------------
quant_process_job <- function()
{
	isolate({
		# Job is ended
		if (!rv$process_job$is_alive()) {
			if (rv$n_logs == nrow(rq1d$SAMPLES)) {
				res <- readRDS(file = file.path(gv$outDir,'rq1d.rds'))
				rq1d <<- res$rq1d
				if (!input$onlyintg) rq1d$get_spectra_data()
			} else {
				msg <- ifelse(rv$n_logs>0, paste(": Spectrum concerned =",rq1d$SAMPLES[rv$n_logs,1]), "" )
				if (input$onlyintg) {
					dispAlert3(paste("ERROR: Integrals failed",msg))
				} else {
					dispAlert4(paste("ERROR: Quantification failed",msg))
				}
			}

			session$sendCustomMessage("proc_status", TRUE)
			if (input$onlyintg) {
				shinyjs::enable("intgReset")
				updateButton(session, "intgButton", label = " Launch Integration", style = "info", disabled = TRUE)
			} else {
				shinyjs::enable("calibReset")
				shinyjs::enable("quantReset")
				updateButton(session, "quantButton", label = " Launch Quantification", style = "info", disabled = TRUE)
			}
			shinyjs::enable("samplesReset")
			rv$proc_output <- c( readLines(file.path(gv$outDir,OUTLOG)), readLines(file.path(rq1d$TMPDIR,ENDFILE)) )

			rv$running <- FALSE
			rv$endproc <- TRUE
		}
		
		# Job is running : update progress bar
		else {
			n_logs <- ifelse(input$onlyintg,
					length(list.files(rq1d$TMPDIR, pattern = "log-.+\\.txt$")),
					length(list.files(rq1d$TMPDIR, pattern = "output_.+\\.txt$"))
			)
			if (n_logs>rv$n_logs) {
				rv$n_logs <- n_logs
				msg <- paste('Processing running since ',
					round(as.numeric(Sys.time()-start.time, units="secs")),'secs (', rv$n_logs,'/',nrow(rq1d$SAMPLES),') ...')
				percent <- round(100*(rv$n_logs/nrow(rq1d$SAMPLES)))
				if (input$onlyintg)
					intg_pb(msg, percent)
				else
					quant_pb(msg, percent)
			}
		}
	})
}


##---------------
# Process management dispatcher
##---------------
observe({
	req(rv$running, rv$process_job)
	invalidateLater(1000, session)
	if (gv$proctype == 'unzip' )
		unzip_process_job()
	if (gv$proctype == 'calib' )
		calib_process_job()
	if (gv$proctype %in% c('intg','quant'))
		quant_process_job()
})

