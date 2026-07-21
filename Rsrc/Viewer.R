#------------------------------------------------
# ID Viewer.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------

# Graphic settings
gp <- list(
	mode = 'html',  # html, png, svg
	IMGDIR = '../tmp/img',
	width = 900,
	height = 400,
	lw = 1,
	plotTrueSpec = TRUE,
	plotzones = FALSE,
	plotresidus = FALSE,
	tags = 'none',
	showgrid = TRUE,
	legendhoriz = TRUE,
	legendtop = TRUE,
	showlegend = TRUE,
	yaxis = TRUE,
	ylabel = '', # 'Intensity (a.u)'
	xlabel = '', # 'Shift (ppm)'
	verbose = FALSE,
	colspecs = COLSPEC,
	colcpmds = COLCPMDS,
	font = list(family = "Arial", size = 16),
	font2 = list(family = "Arial", size = 14)
)


##---------------
## Update rq1d object & both lists of samples and compounds
##---------------
observeEvent( c(rv$samples, rv$endproc), {
	if (! rv$samples || ! rv$endproc ) return(NULL)
	v_options <- gv$compounds
	names(v_options) <- v_options
	updateSelectInput(session, "selcmpds", choices = v_options, selected=v_options)
	samples <-rq1d$SAMPLES
	v_options <- samples[,2]
	names(v_options) <- v_options
	updateSelectInput(session, "selsamples", choices = v_options, selected=v_options)
})

##---------------
## Retrieve information regarding both the selected spectrum and the compound.
##---------------
infolist <- reactive({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	samples <-rq1d$SAMPLES
	idx <- which( unique(samples[,2])==input$selsamples )
	quantif <- rq1d$PROFILE$quantif
	zone <- as.integer(unique(quantif[ quantif$compound==input$selcmpds, ]$zone))
	cmpds <- quantif[quantif$zone==zone, ]$compound
	spec <- rq1d$specList[[idx]]
	Noise <- spec$Noise
	infos <- rq1d$res$infos
	if (! input$onlyintg) {
		samplecol <- NULL
		for(k in 1:nrow(samples))
			samplecol <- c(samplecol, rep(samples[k,2], length(rq1d$res$zones)))
		infos <- cbind(samplecol, infos)
		colnames(infos)[1] <- 'Samplename'
	}
	infos <- infos[ infos[,1]==samples[idx,2] & infos[,3]==zone, , drop=F]
	infos <- infos[, -c(7:10,12,13)]
	peaklist <- rq1d$res$peaklist
	peaklist <- peaklist[ peaklist[,2]==samples[idx,2] & peaklist[,3] %in% cmpds, 3:4, drop=F]
	peaks <- spec$fit$peaks
	peaks <- peaks[ peaks$ppm>=infos$ppm1 & peaks$ppm<=infos$ppm2, , drop=F]
	peaks <- cbind(peaks, peaks$amp/(2*Noise))
	colnames(peaks)[ncol(peaks)] <- 'SNR'
	peaks$amp <- round(peaks$amp)
	peaks$integral <- round(peaks$integral)
	peaks$asym <- round(peaks$asym)
	peaks$SNR <- round(peaks$SNR)
	peaks <- peaks[ peaks$SNR>=input$snr, ]
	list(samples=samples, idx=idx, infos=infos, peaklist=peaklist, peaks=peaks)
})


##---------------
## Show Integration logfile
##---------------
observeEvent(input$logButton2, {
	infos <- infolist()
	if (input$onlyintg)
		LogFile <- paste0(rq1d$TMPDIR,'/log-',infos$samples[infos$idx,2],'.txt')
	else
		LogFile <- paste0(rq1d$TMPDIR,'/output_',infos$samples[infos$idx,1],'_',infos$samples[infos$idx,3],'.txt')
	content <- readLines(LogFile, warn = FALSE)
	showModal(modalDialog(
		title = "Integration log",
		tags$pre(paste(content, collapse = "\n")),
		easyClose = TRUE,
		footer = modalButton("Close"),
		size = "l"
	))
})


##---------------
## Output: conditional value for peaks information
##---------------
output$infopeaks <- reactive({
	if (input$showpeaklist || input$tags=='peak') return(1)
	return(0)
})
outputOptions(output, 'infopeaks', suspendWhenHidden=FALSE)
outputOptions(output, 'infopeaks', priority=20)


##---------------
## Output: sampleInfos
##---------------
output$sampleInfos <- renderDataTable({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	infos <- infolist()
	infos$infos
}, options = list(paging = FALSE, searching = FALSE, info = FALSE))


##---------------
## Output: samplePeaks
##---------------
output$samplePeaks <- renderDT({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	infos <- infolist()
	peaklist <- infos$peaklist
	peaks <- infos$peaks
	if (input$cmpdpeaks) { # Only the peaks of compounds
		pklist <- as.numeric(strsplit(paste(peaklist[,2], collapse=","),',')[[1]])
		peaks <- peaks[rownames(peaks) %in% pklist[!is.na(pklist)], ]
	}
	peaks <- cbind(rownames(peaks), peaks)
	colnames(peaks)[1] <- 'Peak id'
	rq1d$displayTable(peaks, nbdec=4)
})


##---------------
## Output: peaks information
##---------------
output$outInfos <- renderPrint({
	if (! rv$samples || ! rv$endproc) return(NULL)
	unlink(file.path(gp$IMGDIR, paste0('*.',gp$mode)))
	infos <- infolist()
	cat("\n")
	print(infos$peaklist)
})


##---------------
## Spectra viewer 
##---------------
output$spectrum<-renderPlotly({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	unlink(file.path(gp$IMGDIR, paste0('*.',gp$mode)))
	samples <-rq1d$SAMPLES
	idx <- which( unique(samples[,2])==input$selsamples )
	gp$plotTrueSpec <- ! input$specbl
	gp$plotresidus <- input$residus
	gp$tags <- input$tags
	gp$showgrid <- input$showgrid
	gp$showlegend <- ifelse(input$showlegend=='none', FALSE, TRUE)
	gp$legendtop <- ifelse(input$showlegend=='top', TRUE, FALSE)
	gp$yaxis <- input$yaxis

	if (input$axislbl) {
		gp$ylabel = 'Intensity (a.u)'
		gp$xlabel = 'Shift (ppm)'
	} else {
		gp$ylabel = gp$xlabel = '' 
	}
	rq1d$plot_cmpds(idx, input$selcmpds, gp)
})
