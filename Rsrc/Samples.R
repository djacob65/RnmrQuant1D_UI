#------------------------------------------------
# ID Samples.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


##---------------
## Output: Provides some information about loaded spectra
##---------------
output$zipLog <- renderPrint({
	if (rv$unzip==0) return (0)
	cat("----\n",
		system(paste0(RSCRIPT," --version"), intern = TRUE),"\n",
		"Session Identifier = ", gv$sessid, "\n",
		"Instrument/Vendor/Format = ", gv$Vendor, "\n",
		"The original name of the Zip file = ", gv$NameZip, "\n",
		"The original name of the Samples file = ", gv$SampleFilename, "\n",
		"Temporary directory = ",gv$outDir, "\n",
		"The number of Spectra = ", nrow(gv$samples), "\n",
		"----\n",
		sep="")
})


##---------------
## Fill in the sample table with the pulse and frequency values.
##---------------
sampleTable <- reactive({
	if (rv$unzip==0) return(NULL)
	if (gv$Vendor=='bruker') {
		samplecode <- paste(gv$samples[,1], gv$samples[,3], sep = "-")
	} else {
		samplecode <- paste(gv$samples[,1],1, sep="-")
	}
	if (! input$onlyintg) {
		DIR1 <- file.path(gv$outDir, 'samples')
		samples_1 <- priv$get_list_spectrum(DIR1, priv$get_list_samples(DIR1))
		lst1 <- match(samplecode, samples_1[,2])
		lst1 <- lst1[!is.na(lst1)]
		DIR2 <- file.path(gv$outDir, 'QC-QS')
		samples_2 <- priv$get_list_spectrum(DIR2, priv$get_list_samples(DIR2))
		lst2 <- match(samplecode, samples_2[,2])
		lst2 <- lst2[!is.na(lst2)]
		gv$samples$Pulse <<- c(samples_1$sequence[lst1] ,samples_2$sequence[lst2])
		gv$samples$Frequence <<- round(c(as.numeric(samples_1$frequence[lst1]), as.numeric(samples_2$frequence[lst2])))
		gv$samples$Size <<- c(as.numeric(samples_1$size[lst1]), as.numeric(samples_2$size[lst2]))
	} else {
		samples <- priv$get_list_spectrum(gv$outDir, priv$get_list_samples(gv$outDir))
		lst <- match(samplecode, samples[,2])
		gv$samples$Pulse <<- samples$sequence[lst]
		gv$samples$Frequence <<- round(as.numeric(samples$frequence[lst]))
		gv$samples$Size <<- as.numeric(samples$size[lst])
	}
	rv$samples <- 1
	rq1d$FIELD <<- unique(sort(gv$samples$Frequence))[1]

	# Update Sequence / PULSE for Integration / Calibration
	optionPulse <- unique(sort(gv$samples$Pulse))
	names(optionPulse) <- toupper(optionPulse)
	updateSelectInput(session, inputId='sequence', label = 'Sequence (PULSE)', choices = optionPulse)
	updateSelectInput(session, inputId='sequence2', label = 'Sequence (PULSE)', choices = optionPulse)

	# Update Quantification profile
	lstfiles <- list.files( path = file.path(gv$outDir, 'profiles'), pattern = "^profile-", full.names = FALSE)
	lstfiles <- lstfiles[grep(paste0('(',paste(optionPulse, collapse="|"),')'), lstfiles)]
	lstfiles <- lstfiles[grep(rq1d$FIELD, lstfiles)]
	updateSelectInput(session, "intgprofile", label = "Quantification profile", choices = lstfiles)
	updateSelectInput(session, "quantprofile", label = "Quantification profile", choices = lstfiles)

	if (gv$hasQCQS) {
		# Update Sequence / PULSE
		updateSelectInput(session, inputId='sequence', label = 'Sequence (PULSE)', choices = optionPulse, selected = optionPulse[1])
		# Update Calibration profile
		lstfiles <- list.files( path = file.path(gv$outDir, 'profiles'), pattern = "^standards-", full.names = FALSE)
		updateSelectInput(session, "calibprofile", label = "Calibration profile", choices = lstfiles)
	}
	gv$samples
})


##---------------
## Output: sampleTable
##---------------
output$sampleTable <- renderDataTable({
	if (rv$unzip==0) return(NULL)
	sampleTable()
}, options = list(pageLength=10, autoWidth = F))


