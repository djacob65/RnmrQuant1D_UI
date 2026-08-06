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
	rq1d$SEQUENCE <<- input$sequence
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
}, ignoreInit = TRUE)


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
}, ignoreInit = TRUE)

observeEvent(input$confirm_intg, {
	removeModal()
	rv$intgreset <- TRUE
	shinyjs::disable("intgReset")
	shinyjs::enable("intgButton")
	for (widget in intg_widgets)
		shinyjs::enable(widget)
	rv$job_output <- ''
}, ignoreInit = TRUE)

observeEvent(input$cancel_intg, {
	removeModal()
	rv$intgreset <- FALSE
	optionPulse <- unique(sort(gv$samples$Pulse))
	names(optionPulse) <- toupper(optionPulse)
	updateSelectInput(session, inputId='sequence', label = 'Sequence (PULSE)', choices = optionPulse, selected=rq1d$SEQUENCE)
}, ignoreInit = TRUE)

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
}, ignoreInit = TRUE)

##---------------
#  Updates samples based on a pattern
##---------------
observeEvent(input$intgpattern, {
	rv$endproc <- FALSE
	updateSamples()
}, ignoreInit = TRUE)

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
}, ignoreInit = TRUE)


##---------------
# Show/Edit Quantification profile in a Modal Dialog Box
##---------------
observeEvent(input$viewIntgBtn, {
	if (!is.null(gv$PROFILE)) {
		output$intgTable1 <- renderDT({
			if (!is.null(rq1d$PROFILE))
				datatable(rq1d$PROFILE$fitting,
					options = list(pageLength = 10),
					editable = list(target = "cell", disable = list(columns = c(9))))
		})
		output$intgTable2 <- renderDT({
			if (!is.null(rq1d$PROFILE)) {
				quantif <- rq1d$PROFILE$quantif
				quantif$P4 <- NULL
				datatable(quantif, 
					options = list(pageLength = 10),
					editable = list(target = "cell", disable = list(columns = c(1,9))))
			}
		})
		observeEvent(input$intgTable1_cell_edit, {
			info <- input$intgTable1_cell_edit
			rq1d$PROFILE$fitting[info$row, info$col] <<- info$value
		})
		observeEvent(input$intgTable2_cell_edit, {
			info <- input$intgTable2_cell_edit
			quantif <- rq1d$PROFILE$quantif
			quantif$P4 <- NULL
			quantif[info$row, info$col] <- info$value
			quantif$P4 <- rq1d$PROFILE$quantif$P4
			rq1d$PROFILE$quantif <<- quantif
		})
		showModal(modalDialog(
			title = "Quantification profile",
			tags$br(),
			DTOutput("intgTable1"),
			tags$br(),tags$br(),
			DTOutput("intgTable2"),
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
output$endIntg <- reactive({
	if (rv$endproc) return(1)
	return(0)
})
outputOptions(output, 'endIntg', suspendWhenHidden=FALSE)
outputOptions(output, 'endIntg', priority=20)



##---------------
## Show / update the progress bar
##---------------
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
# Execute an expr, intercepting —where applicable— the message and interruption
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
# Launch Integration process
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
		rv$running <- TRUE
		rv$n_logs <- 0
		start.time <<- Sys.time()
		intg_pb(paste('Initialize the cluster (',gv$ncpu,' cores) ...'), 0)
		session$sendCustomMessage("proc_status", TRUE)
		rv$process_job <- submit_rq1d_proc(rq1d, gv, proc='intg')

		break
	}
})


##---------------
# Display the job output
##---------------
output$outIntg2 <- renderPrint({
	req(!rv$running)
	if (length(nchar(rv$job_output))>1)
		for(l in rv$job_output) cat(l,"\n")
})


##---------------
# STOP : Kills the process AND its children (cluster)
##---------------
observeEvent(input$intgStop, {
	req(rv$process_job)
	if (rv$process_job$is_alive()) {
		rv$process_job$kill_tree()
	}
	dispAlert3(paste("Warning: Processing stopped by the user at",rv$n_logs,"/",nrow(rq1d$SAMPLES)))
	shinyjs::enable("samplesReset")
	for (widget in intg_widgets)
		shinyjs::enable(widget)
	updateButton(session, "intgButton", label = "Launch Integration", style = "info", disabled = FALSE)
	rv$job_output <- NULL
	if (file.exists(file.path(gv$outDir,OUTLOG)))
		rv$job_output <- readLines(file.path(gv$outDir,OUTLOG))
	if (file.exists(file.path(rq1d$TMPDIR,ENDFILE)))
		rv$job_output <- c( rv$job_output , readLines(file.path(rq1d$TMPDIR,ENDFILE)))
	rv$running <- FALSE
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
