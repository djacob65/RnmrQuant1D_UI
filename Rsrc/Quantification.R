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
	if (! rv$samples ) return(NULL)
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
})


##---------------
#  Updates samples and compounds based on the profile 
##---------------
observeEvent(c(input$quantprofile,input$externQuantFile), {
	rv$endproc <- FALSE
	updateSamples2()
	updateQuant()
})


##---------------
#  Updates samples based on a pattern
##---------------
observeEvent(input$quantpattern, {
	rv$endproc <- FALSE
	updateSamples2()
})


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
})

observeEvent(input$confirm_quant, {
	removeModal()
	rv$quantreset <- TRUE
	shinyjs::disable("quantReset")
	shinyjs::enable("quantButton")
	for (widget in quant_widgets)
		shinyjs::enable(widget)
})

observeEvent(input$cancel_quant, {
	removeModal()
	rv$quantreset <- FALSE
})

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
		rq1d$PROFILE <<- rq1d$readProfile(file.path(gv$outDir, 'profiles', gv$PROFILE))
		v_options <- sort(rq1d$PROFILE$quantif$compound)
		names(v_options) <- v_options
		cmpds <- v_options[ ! v_options %in% input$listcmpds2 ]
		updateSelectInput(session, "listcmpds2", choices = v_options, selected=cmpds)
	}
})


##---------------
# Quantification profile as a DataTable
##---------------
output$quantTable1 <- renderDT({
	if (! rv$samples ) return(NULL)
	updateQuant()
	datatable(rq1d$PROFILE$fitting)
})

output$quantTable2 <- renderDT({
	if (! rv$samples ) return(NULL)
	updateQuant()
	quantif <- rq1d$PROFILE$quantif
	quantif$P4 <- NULL
	datatable(quantif)
})


##---------------
# Show Quantification profile in a Modal Dialog Box
##---------------
observeEvent(input$viewQuantBtn, {
	if (!is.null(gv$PROFILE))
		showModal(modalDialog(
			tags$br(),
			DTOutput("quantTable1"),
			tags$br(),tags$br(),
			DTOutput("quantTable2"),
			title = "Quantification profile",
			easyClose = TRUE,
			footer = modalButton("Close"),
			size = "l"
		))
})


##---------------
## Output: conditional value for export button
##---------------
output$endQuant <- reactive({
	if (rv$endproc) return(1)
	return(0)
})
outputOptions(output, 'endQuant', suspendWhenHidden=FALSE)
outputOptions(output, 'endQuant', priority=20)


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
# Executes an expr, intercepting —where applicable— the message and interruption
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
			rq1d$PROFILE <<- rq1d$readProfile(file.path(gv$outDir, 'profiles', gv$PROFILE))
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
		gv$ncpu <- min(length(gv$zones), max_ncpu)
		cat("\n")
		cat(paste('Selected Zones : ', paste(gv$zones, collapse=",")),"\n")
		cat(paste('Nb Cores =', gv$ncpu),"\n")
		cat(paste('Nb Samples =', nrow(rq1d$SAMPLES)),"\n")
		cat("\n")

		updateButton(session, "quantButton", label = "Launch Quantification", style = "warning", disabled = TRUE)

		# Initialize the cluster then launch the processing
		session$sendCustomMessage("proc_status", TRUE)
		quant_pb(paste('Initialize the cluster (',gv$ncpu,' cores) ...'), 0)
		rv$process_job <- submit_rq1d_proc(rq1d, gv, proc='quant')

		if (!rv$process_job$is_alive()) {
			dispAlert4("ERROR: proc_Quantification failed !")
		} else {
			start.time <- Sys.time()
			nc <- 0
			# Monitor the number of output log files.
			repeat {
				Sys.sleep(1)
				n <-length(list.files(rq1d$TMPDIR, pattern = "output_.+\\.txt$"))
				if (n<nrow(rq1d$SAMPLES) && !rv$process_job$is_alive()) {
					dispAlert4(paste("ERROR: proc_Quantification failed : Spectrum concerned =",rq1d$SAMPLES[n,1]))
					break
				}
				if (n==0 || n>nc) {
					msg <- paste('Processing since ',round(as.numeric(Sys.time()-start.time, units="secs")),'secs (',n,'/',nrow(rq1d$SAMPLES),') ...')
					quant_pb(msg, round(100*(n/nrow(rq1d$SAMPLES))))
				}
				nc <- n
				if (n==nrow(rq1d$SAMPLES)) break
			}
			# Monitor the completion of the results file writing process.
			repeat {
				if (!rv$process_job$is_alive()) break
				Sys.sleep(1)
			}
			if (n==nrow(rq1d$SAMPLES)) {
				rv$endproc <- TRUE
				res <<- readRDS(file = file.path(gv$outDir,'rq1d.rds'))
				rq1d <<- res$rq1d
				rq1d$get_spectra_data()
			}
		}

		shinyjs::enable("calibReset")
		shinyjs::enable("samplesReset")
		shinyjs::enable("quantReset")
		updateButton(session, "quantButton", label = "Launch Quantification", style = "info", disabled = TRUE)
		L <- readLines(file.path(gv$outDir,OUTLOG))
		for(l in L) cat(l,"\n")
		L <- readLines(file.path(rq1d$TMPDIR,ENDFILE))
		for(l in L) cat(l,"\n")

		break
	}
})


##---------------
# Export the WorkBook
##---------------
output$exportWBquant <- downloadHandler(
	filename = function() { paste0('WB_',gsub("\\.\\S+$","",basename(gv$NameZip)), '.xlsx' ) },
	content = function(file) {
		shinyjs::runjs( "document.getElementById('waitbox3').style.display = 'block';" )
		filelist <- list(SAMPLEFILE=gv$SampleFilename, PROFILE=gv$PROFILE, CALIBRATION=gv$STDS_FILE)
		out <- rq1d$get_output_results()
		cmpds <-  gsub("(-| )","_",gv$compounds)
		out$quantif <- out$quantif[ , colnames(out$quantif) %in% cmpds]
		out$Int <- out$Int[ , colnames(out$Int) %in% cmpds]
		out$SNR <- out$SNR[ , colnames(out$SNR) %in% cmpds]
		rq1d$save_Results(file, results=out, filelist=filelist)
		shinyjs::runjs( "document.getElementById('waitbox3').style.display = 'none';" )
	}
)
