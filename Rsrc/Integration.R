#------------------------------------------------
# ID Integration.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------

##---------------
## Alert Box 3
##---------------
dispAlert3 <- function(msg, title='', style='danger') {
	if (nchar(msg)>0) {
		createAlert(session, "AlertIntg", "AlertIntgId", title = title, content = msg, append = FALSE, style=style)
	}
}

##---------------
## Obtain the quantification profile based on the selected source.
##---------------
intgprofile <- reactive ({
	if (isTRUE(input$externalIntg)) {
		namefile <- input$externIntgFile$name
		file.rename( input$externIntgFile$datapath, file.path(gv$outDir, 'profiles', namefile) )
		ret <- namefile
	} else {
		ret <- input$intgprofile
	}
	if (nchar(ret)==0) {
		ret <- NULL
	}
	return(ret)
})

##---------------
# Update rq1d object & both lists of samples and compounds
##---------------
updateSamples <- reactive({
	if (! rv$samples ) return(NULL)
	samples <- as.data.frame(gv$samples)
	samples <- samples[samples$Pulse == input$sequence,]
	if (nchar(input$intgpattern) && is_regex_valide(input$intgpattern))
		samples <- samples[grep(input$intgpattern, samples[,2], value=FALSE), ]
	v_options <- samples[,2]
	names(v_options) <- v_options
	updateSelectInput(session, "listsamples", choices = v_options, selected=v_options)
})

updateIntg <- reactive({
	if (! rv$samples ) return(NULL)
	gv$PROFILE <<- intgprofile()
	if (!is.null(gv$PROFILE)) {
		rq1d$PROFILE <<- rq1d$readProfile(file.path(gv$outDir, 'profiles', gv$PROFILE))
		v_options <- unique(rq1d$PROFILE$quantif$compound)
		names(v_options) <- v_options
		updateSelectInput(session, "listcmpds", choices = v_options, selected=v_options)
	}
})


##---------------
#  Updates samples when entering in this tab
##---------------
observeEvent(input$outtabs, {
	updateSamples()
})


##---------------
#  Updates samples and compounds based on the profile 
##---------------
observeEvent(input$sequence, {
	if (rv$samples ) {
		rv$endproc <- FALSE
		lstfiles <- list.files(path = file.path(gv$outDir, 'profiles'), pattern = "^profile-", full.names = FALSE)
		lstfiles <- lstfiles[grep(input$sequence, lstfiles)]
		lstfiles <- lstfiles[grep(rq1d$FIELD, lstfiles)]
		updateSelectInput(session, "intgprofile", label = "Quantification profile", choices = lstfiles)
	}
})


##---------------
# Reset management
##---------------
observeEvent(input$intgReset, {
	showModal(modalDialog(
		title = "Warning",
		"Changing this setting will clear the current results. Continue?",
		footer = tagList(
			actionButton("cancel_intg", "Cancel"),
			actionButton("confirm_intg", "Continue", class = "btn-danger")
		),
		easyClose = FALSE
	))
})

observeEvent(input$confirm_intg, {
	removeModal()
	rv$intgreset <- TRUE
	shinyjs::disable("intgReset")
	shinyjs::enable("intgButton")
	for (widget in intg_widgets)
		shinyjs::enable(widget)
})

observeEvent(input$cancel_intg, {
	removeModal()
	rv$intgreset <- FALSE
	optionPulse <- unique(sort(gv$samples$Pulse))
	names(optionPulse) <- toupper(optionPulse)
	updateSelectInput(session, inputId='sequence', label = 'Sequence (PULSE)', choices = optionPulse, selected=rq1d$SEQUENCE)
})

observeEvent(input$intgButton, {
	rv$intgreset <- FALSE
})


##---------------
#  Updates samples and compounds based on the profile 
##---------------
observeEvent(c(input$intgprofile, input$externIntgFile), {
	rv$endproc <- FALSE
	updateSamples()
	updateIntg()
})

##---------------
#  Updates samples based on a pattern
##---------------
observeEvent(input$intgpattern, {
	rv$endproc <- FALSE
	updateSamples()
})

##---------------
#  Inverses selection of the compounds
##---------------
observeEvent(input$intgInvBtn, {
	rv$endproc <- FALSE
	if (!is.null(gv$PROFILE)) {
		v_options <- sort(rq1d$PROFILE$quantif$compound)
		names(v_options) <- v_options
		cmpds <- v_options[ ! v_options %in% input$listcmpds ]
		updateSelectInput(session, "listcmpds", choices = v_options, selected=cmpds)
	}
})


##---------------
#  Show / hide "Spectra Viewer" depending on processing status
##---------------
observeEvent(rv$endproc, {
	if (rv$endproc)
		showTab(inputId = "outtabs", target = "viewer")
	else
		hideTab(inputId = "outtabs", target = "viewer")
})


##---------------
# Quantification profile as a DataTable
##---------------
output$intgTable1 <- renderDT({
	if (! rv$samples ) return(NULL)
	updateIntg()
	datatable(rq1d$PROFILE$fitting)
})

output$intgTable2 <- renderDT({
	if (! rv$samples ) return(NULL)
	updateIntg()
	quantif <- rq1d$PROFILE$quantif
	quantif$P4 <- NULL
	datatable(quantif)
})


##---------------
# Show Quantification profile in a Modal Dialog Box
##---------------
observeEvent(input$viewIntgBtn, {
	if (!is.null(gv$PROFILE))
		showModal(modalDialog(
			tags$br(),
			DTOutput("intgTable1"),
			tags$br(),tags$br(),
			DTOutput("intgTable2"),
			title = "Quantification profile",
			easyClose = TRUE,
			footer = modalButton("Close"),
			size = "l"
		))
})


##---------------
## Output: conditional value for export button
##---------------
output$endIntg <- reactive({
	if (rv$endproc) return(1)
	return(0)
})
outputOptions(output, 'endIntg', suspendWhenHidden=FALSE)
outputOptions(output, 'endIntg', priority=20)


intg_pb <- function(msg, p1p){
	p1px <- 7*p1p; p2px <- 700 - p1px;
	shinyjs::runjs( paste0("
		document.getElementById('pbtitle').textContent = '",msg,"';
		document.getElementById('pbleft').style.width = '",p1px,"px';
		document.getElementById('pbright').style.width = '",p2px,"px';
		document.getElementById('pbval').textContent = '",p1p,"';
	"))
}


##---------------
# Executes an expr, intercepting —where applicable— the message and interruption
##---------------
intg.exe.catch  <- function(expr) {
	out <- exe.catch({ expr })
	if (out$error_occurred) dispAlert3(out$message)
	out$result
}


##---------------
# Display the number of selected samples.
##---------------
output$selintg <- renderText({
	if (rv$samples==1 && length(input$listsamples)>0) {
		if (input$onlyintg) {
			samples <- gv$samples[ gv$samples$Pulse==input$sequence, ]
		} else {
			samples <- gv$samples[ ! gv$samples$Type %in% QCQS & gv$samples$Pulse==input$sequence, ]
		}
		paste0(length(input$listsamples),'/',nrow(samples))
	}
})


##---------------
# Launch Integration
##---------------
output$outIntg <- renderPrint({
	req(input$intgButton)
	rv$endproc <- FALSE

	repeat {
		if (input$intgButton!=lstbtn$intg || rv$intgreset)
			break

		lstbtn$intg <<- lstbtn$intg + 1

		if (is.null(gv$PROFILE)) {
			dispAlert3("Error: No quantification profile provided")
			break
		}

		isolate({
			rq1d$SAMPLES <<- gv$samples[ gv$samples[,2] %in% input$listsamples & gv$samples$Pulse==input$sequence, ]
			rq1d$SEQUENCE <<- input$sequence
			rq1d$PROFILE <<- rq1d$readProfile(file.path(gv$outDir, 'profiles', gv$PROFILE))
			gv$compounds <<- input$listcmpds
			gv$zones <<- as.integer(unique(rq1d$PROFILE$quantif[ rq1d$PROFILE$quantif$compound %in% input$listcmpds, ]$zone))
		})

		if (is.null(gv$zones) || length(gv$zones)==0) {
			dispAlert3("Error : No selected compounds")
			break
		}

		if (is.null(rq1d$SAMPLES) || nrow(rq1d$SAMPLES)==0) {
			dispAlert3("Error : No selected samples")
			break
		}

		closeAlert(session, "AlertIntgId")

		shinyjs::disable("samplesReset")
		for (widget in intg_widgets)
			shinyjs::disable(widget)

		out <- intg.exe.catch({
			rq1d$check_profile(verbose=TRUE)
		})

		max_ncpu <- ifelse(gv$max_ncpu>0, gv$max_ncpu, parallel::detectCores())
		gv$ncpu <- min(length(gv$zones), max_ncpu)
		cat("\n")
		cat(paste('Selected Zones : ', paste(gv$zones, collapse=",")),"\n")
		cat(paste('Nb Cores =', gv$ncpu),"\n")
		cat(paste('Nb Samples =', nrow(rq1d$SAMPLES)),"\n")
		cat("\n")

		updateButton(session, "intgButton", label = "Launch Integration", style = "warning", disabled = TRUE)

		# Initialize the cluster then launch the processing
		session$sendCustomMessage("proc_status", TRUE)
		intg_pb(paste('Initialize the cluster (',gv$ncpu,' cores) ...'), 0)
		rv$process_job <- submit_rq1d_proc(rq1d, gv, proc='intg')

		if (!rv$process_job$is_alive()) {
			dispAlert3("ERROR: proc_Integrals failed !")
		} else {
			start.time <- Sys.time()
			nc <- 0
			# Monitor the number of output log files.
			repeat {
				Sys.sleep(1)
				n <-length(list.files(rq1d$TMPDIR, pattern = "\\.txt$"))
				if (n<nrow(rq1d$SAMPLES) && !rv$process_job$is_alive()) {
					dispAlert3(paste("ERROR: proc_Integrals failed : Spectrum concerned =",rq1d$SAMPLES[n,1]))
					break
				}
				if (n==0 || n>nc) {
					msg <- paste('Processing since ',round(as.numeric(Sys.time()-start.time, units="secs")),'secs (',n,'/',nrow(rq1d$SAMPLES),') ...')
					intg_pb(msg, round(100*(n/nrow(rq1d$SAMPLES))))
				}
				nc <- n
				if (n==nrow(rq1d$SAMPLES)) break
			}
			# Monitor the completion of the messages file writing process.
			repeat {
				if (!rv$process_job$is_alive()) break
				Sys.sleep(1)
			}
			if (n==nrow(rq1d$SAMPLES)) {
				rv$endproc <- TRUE
				res <<- readRDS(file = file.path(gv$outDir,'rq1d.rds'))
				rq1d <<- res$rq1d
			}
		}

		shinyjs::enable("intgReset")
		shinyjs::enable("samplesReset")
		updateButton(session, "intgButton", label = "Launch Integration", style = "info", disabled = TRUE)
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
output$exportWBintg <- downloadHandler(
	filename = function() { paste0('WB_',gsub("\\.\\S+$","",basename(gv$NameZip)), '.xlsx' ) },
	content = function(file) {
		shinyjs::runjs( "document.getElementById('waitbox2').style.display = 'block';" )
		filelist <- list(SAMPLEFILE=gv$SampleFilename, PROFILE=gv$PROFILE)
		rq1d$res$allquantifs <<- rq1d$res$allquantifs[ rq1d$res$allquantifs$Compound %in% gv$compounds, ]
		rq1d$save_Matrices(file, filelist)
		shinyjs::runjs( "document.getElementById('waitbox2').style.display = 'none';" )
	}
)
