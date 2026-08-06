#------------------------------------------------
# ID Quantification.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------

##---------------
## Alert Box 4
##---------------
dispAlert4 <- function(msg, title='', style='danger') {
	if (nchar(msg)>0) {
		createAlert(session, "AlertQuant", "AlertQuantId", title = title, content = msg, append = FALSE, style=style)
	}
}


##---------------
## Obtain the quantification profile based on the selected source.
##---------------
quantprofile <- reactive ({
	if (isTRUE(input$externalQuant)) {
		namefile <- input$externQuantFile$name
		file.rename( input$externQuantFile$datapath, file.path(gv$outDir, 'profiles', namefile) )
		ret <- namefile
	} else {
		ret <- input$quantprofile
	}
	if (nchar(ret)==0) {
		ret <- NULL
	}
	return(ret)
})


##---------------
# Update rq1d object & both lists of samples and compounds
##---------------
updateSamples2 <- reactive({
	if (! rv$samples ) return(NULL)
	samples <- as.data.frame(gv$samples)
	samples <- samples[samples$Pulse == input$sequence2,]
	samples <- samples[samples$Type == conf$SAMPLE,]
	if (nchar(input$quantpattern) && is_regex_valide(input$quantpattern))
		samples <- samples[grep(input$quantpattern, samples[,2], value=FALSE), ]
	v_options <- samples[,2]
	names(v_options) <- v_options
	updateSelectInput(session, "listsamples2", choices = v_options, selected=v_options)
})

updateQuant <- reactive({
	if (! rv$samples) return(NULL)
	if (!is.null(input$sequence2) && nchar(input$sequence2))
		rq1d$SEQUENCE <<- input$sequence2
	gv$PROFILE <<- quantprofile()
	if (!is.null(gv$PROFILE)) {
		rq1d$PROFILE <<- rq1d$readProfile(file.path(gv$outDir, 'profiles', gv$PROFILE))
		v_options <- sort(rq1d$PROFILE$quantif$compound)
		names(v_options) <- v_options
		updateSelectInput(session, "listcmpds2", choices = v_options, selected=v_options)
	}
})

##---------------
#  Updates samples when entering in this tab
##---------------
observeEvent(input$outtabs, {
	updateSamples2()
}, ignoreInit = TRUE)


##---------------
#  Updates samples and compounds based on the profile 
##---------------
observeEvent(c(input$quantprofile,input$externQuantFile), {
	rv$endproc <- FALSE
	updateSamples2()
	updateQuant()
}, ignoreInit = TRUE)


##---------------
#  Updates samples based on a pattern
##---------------
observeEvent(input$quantpattern, {
	rv$endproc <- FALSE
	updateSamples2()
}, ignoreInit = TRUE)


##---------------
# Reset management
##---------------
observeEvent(input$quantReset, {
	showModal(modalDialog(
		title = "Warning",
		"Changing this setting will clear the current results. Continue?",
		footer = tagList(
			actionButton("cancel_quant", "Cancel"),
			actionButton("confirm_quant", "Continue", class = "btn-danger")
		),
		easyClose = FALSE
	))
}, ignoreInit = TRUE)

observeEvent(input$confirm_quant, {
	removeModal()
	rv$quantreset <- TRUE
	shinyjs::disable("quantReset")
	shinyjs::enable("quantButton")
	for (widget in quant_widgets)
		shinyjs::enable(widget)
	rv$job_output <- ''
}, ignoreInit = TRUE)

observeEvent(input$cancel_quant, {
	removeModal()
	rv$quantreset <- FALSE
}, ignoreInit = TRUE)

observeEvent(input$quantButton, {
	rv$quantreset <- FALSE
})


##---------------
#  Inverses selection of the compounds
##---------------
observeEvent(input$quantInvBtn, {
	rv$endproc <- FALSE
	gv$PROFILE <<- quantprofile()
	if (!is.null(gv$PROFILE)) {
		v_options <- sort(rq1d$PROFILE$quantif$compound)
		names(v_options) <- v_options
		cmpds <- v_options[ ! v_options %in% input$listcmpds2 ]
		updateSelectInput(session, "listcmpds2", choices = v_options, selected=cmpds)
	}
}, ignoreInit = TRUE)


##---------------
# Show/Edit Quantification profile in a Modal Dialog Box
##---------------
observeEvent(input$viewQuantBtn, {
	if (!is.null(gv$PROFILE)) {
		output$quantTable1 <- renderDT({
			if (!is.null(rq1d$PROFILE))
				datatable(rq1d$PROFILE$fitting,
					options = list(pageLength = 10),
					editable = list(target = "cell", disable = list(columns = c(9))))
		})
		output$quantTable2 <- renderDT({
			if (!is.null(rq1d$PROFILE)) {
				quantif <- rq1d$PROFILE$quantif
				quantif$P4 <- NULL
				datatable(quantif,
					options = list(pageLength = 10),
					editable = list(target = "cell", disable = list(columns = c(1,9))))
			}
		})
		observeEvent(input$quantTable1_cell_edit, {
			info <- input$quantTable1_cell_edit
			rq1d$PROFILE$fitting[info$row, info$col] <<- info$value
		})
		observeEvent(input$quantTable2_cell_edit, {
			info <- input$quantTable2_cell_edit
			quantif <- rq1d$PROFILE$quantif
			quantif$P4 <- NULL
			quantif[info$row, info$col] <- info$value
			quantif$P4 <- rq1d$PROFILE$quantif$P4
			rq1d$PROFILE$quantif <<- quantif
		})
		showModal(modalDialog(
			title = "Quantification profile",
			tags$br(),
			DTOutput("quantTable1"),
			tags$br(),tags$br(),
			DTOutput("quantTable2"),
			tags$br(),tags$br(),
			HTML("See "), 
			tags$a("Quantification profile", target = "_blank",	href = urls_doc$QUANTDOC),
			HTML(" in the online documentation"),
			tags$br(),tags$br(),
			easyClose = TRUE,
			footer = modalButton("Close"),
			size = "l"
		))
	}
}, ignoreInit = TRUE)


##---------------
## Output: conditional value for export button
##---------------
output$endQuant <- reactive({
	if (rv$endproc) return(1)
	return(0)
})
outputOptions(output, 'endQuant', suspendWhenHidden=FALSE)
outputOptions(output, 'endQuant', priority=20)


##---------------
## Show / update the progress bar
##---------------
quant_pb <- function(msg, p1p){
	p1px <- 7*p1p; p2px <- 700 - p1px;
	shinyjs::runjs( paste0("
		document.getElementById('pbtitle2').textContent = '",msg,"';
		document.getElementById('pbleft2').style.width = '",p1px,"px';
		document.getElementById('pbright2').style.width = '",p2px,"px';
		document.getElementById('pbval2').textContent = '",p1p,"';
	"))
}


##---------------
# Execute an expr, intercepting —where applicable— the message and interruption
##---------------
quant.exe.catch  <- function(expr) {
	out <- exe.catch({ expr })
	if (out$error_occurred) dispAlert4(out$message)
	out$result
}

##---------------
# Display the number of selected samples.
##---------------
output$selquant <- renderText({
	if (rv$samples==1 && length(input$listsamples2)>0) {
		samples <- gv$samples[ ! gv$samples$Type %in% QCQS & gv$samples$Pulse==input$sequence2, ]
		paste0(length(input$listsamples2),'/',nrow(samples))
	}
})


##---------------
# Launch Quantification
##---------------
output$outQuant <- renderPrint({
	req(input$quantButton)
	rv$endproc <- FALSE
	
	repeat {
		if (input$quantButton!=lstbtn$quant || rv$quantreset)
			break

		lstbtn$quant <<- lstbtn$quant + 1

		if (is.null(gv$PROFILE)) {
			dispAlert4("Error: No quantification profile provided")
			break
		}

		isolate({
			rq1d$SAMPLES <<- gv$samples[ gv$samples[,2] %in% input$listsamples2 & gv$samples$Pulse==input$sequence2, ]
			rq1d$SEQUENCE <<- input$sequence2
			gv$compounds <<- input$listcmpds2
			gv$zones <<- as.integer(unique(rq1d$PROFILE$quantif[ rq1d$PROFILE$quantif$compound %in% input$listcmpds2, ]$zone))
		})

		if (is.null(gv$zones) || length(gv$zones)==0) {
			dispAlert4("Error : No selected compounds")
			break
		}

		if (is.null(rq1d$SAMPLES) || nrow(rq1d$SAMPLES)==0) {
			dispAlert4("Error : No selected samples")
			break
		}

		closeAlert(session, "AlertQuantId")

		shinyjs::disable("samplesReset")
		shinyjs::disable("calibReset")
		for (widget in quant_widgets)
			shinyjs::disable(widget)

		out <- quant.exe.catch({
			rq1d$check_profile(verbose=TRUE)
		})

		max_ncpu <- ifelse(gv$max_ncpu>0, gv$max_ncpu, parallel::detectCores())
		gv$ncpu <<- min(length(gv$zones), max_ncpu)
		cat("\n")
		cat(paste('Selected Zones : ', paste(gv$zones, collapse=",")),"\n")
		cat(paste('Nb Cores =', gv$ncpu),"\n")
		cat(paste('Nb Samples =', nrow(rq1d$SAMPLES)),"\n")
		cat("\n")

		updateButton(session, "quantButton", label = "Launch Quantification", style = "warning", disabled = TRUE)

		# Initialize the cluster then launch the processing
		rv$running <- TRUE
		rv$n_logs <- 0
		rv$job_output <- NULL
		start.time <<- Sys.time()
		quant_pb(paste('Initialize the cluster (',gv$ncpu,' cores) ...'), 0)
		session$sendCustomMessage("proc_status", TRUE)
		rv$process_job <- submit_rq1d_proc(rq1d, gv, proc='quant')

		break
	}
})


##---------------
# Display the job output
##---------------
output$outQuant2 <- renderPrint({
	req(!rv$running)
	if (length(nchar(rv$job_output))>1) {
		rv$endproc <- TRUE
		rv$running <- FALSE
		for(l in rv$job_output) cat(l,"\n")
	}
})


##---------------
# STOP : Kills the process AND its children (cluster)
##---------------
observeEvent(input$quantStop, {
	req(rv$process_job)
	if (rv$process_job$is_alive()) {
		rv$process_job$kill_tree()
	}
	dispAlert4(paste("Warning: Processing stopped by the user at",rv$n_logs,"/",nrow(rq1d$SAMPLES)))
	shinyjs::enable("calibReset")
	shinyjs::enable("samplesReset")
	updateButton(session, "quantButton", label = "Launch Quantification", style = "info", disabled = TRUE)
	rv$job_output <- NULL
	if (file.exists(file.path(gv$outDir,OUTLOG)))
		rv$job_output <- readLines(file.path(gv$outDir,OUTLOG))
	if (file.exists(file.path(rq1d$TMPDIR,ENDFILE)))
		rv$job_output <- c( rv$job_output , readLines(file.path(rq1d$TMPDIR,ENDFILE)))
	rv$running <- FALSE
	shinyjs::enable("quantReset")
	rq1d$SAMPLES <<- rq1d$SAMPLES[1:(rv$n_logs-1), ]
	rq1d$quantpars <<- list(cmpdlist=gv$compounds, zones=gv$zones, ncpu=gv$ncpu, 
			tottime=as.numeric(Sys.time()-start.time, units="secs"))
	rq1d$res$proctype <<- 'quantification'
	rq1d$get_spectra_data()
	rv$endproc <- TRUE
})


##---------------
# Export the WorkBook
##---------------
output$exportWBquant <- downloadHandler(
	filename = function() { paste0('WB_',gsub("\\.\\S+$","",basename(gv$NameZip)), '.xlsx' ) },
	content = function(file) {
		shinyjs::runjs( "document.getElementById('waitbox3').style.display = 'block';" )
		filelist <- list(SAMPLEFILE=gv$SampleFilename, PROFILE=basename(gv$PROFILE), CALIBRATION=basename(gv$STDS_FILE))
		out <- rq1d$get_output_results()
		cmpds <-  gsub("(-| )","_",gv$compounds)
		out$quantif <- out$quantif[ , colnames(out$quantif) %in% cmpds]
		out$Int <- out$Int[ , colnames(out$Int) %in% cmpds]
		out$SNR <- out$SNR[ , colnames(out$SNR) %in% cmpds]
		rq1d$save_Results(file, results=out, filelist=filelist)
		shinyjs::runjs( "document.getElementById('waitbox3').style.display = 'none';" )
	}
)
