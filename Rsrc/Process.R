#------------------------------------------------
# ID Process.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


##---------------
#  Show / hide "Spectra Viewer" depending on processing status
##---------------
observeEvent(rv$endproc, {
	if (rv$n_logs>0 && ((rv$endproc && !input$onlyintg) || (rv$endproc && input$onlyintg && rv$n_logs==nrow(rq1d$SAMPLES))))
		showTab(inputId = "outtabs", target = "viewer")
	else
		hideTab(inputId = "outtabs", target = "viewer")
})


# --------------------------
# Reactive value of the job state for UI
# --------------------------
output$running <- reactive({
	rv$running
})
outputOptions(output, 'running', suspendWhenHidden=FALSE)
outputOptions(output, 'running', priority=20)


##---------------
# Progress tracking
##---------------
observe({
	req(rv$running, rv$process_job)
	invalidateLater(1000, session)
	isolate({
		# Job is ended
		if (!rv$process_job$is_alive()) {
			rv$endproc <- TRUE
			if (rv$n_logs == nrow(rq1d$SAMPLES)) {
				res <<- readRDS(file = file.path(gv$outDir,'rq1d.rds'))
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
			if (input$onlyintg) {
				shinyjs::enable("intgReset")
				updateButton(session, "intgButton", label = "Launch Integration", style = "info", disabled = TRUE)
			} else {
				shinyjs::enable("calibReset")
				shinyjs::enable("quantReset")
				updateButton(session, "quantButton", label = "Launch Quantification", style = "info", disabled = TRUE)
			}
			shinyjs::enable("samplesReset")
			rv$job_output <- c( readLines(file.path(gv$outDir,OUTLOG)), readLines(file.path(rq1d$TMPDIR,ENDFILE)) )
			rv$running <- FALSE
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
})

