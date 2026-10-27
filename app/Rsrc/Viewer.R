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

# Reactive values ​​for the x and y limits of the spectrum graph
gprv <- reactiveValues(	xmin = NULL, xmax = NULL )

# Saving selections is necessary for infolist reactivity
gpgv <- list(selsamples='', selcmpds='')


##---------------
## Update rq1d object & both lists of samples and compounds
##---------------
observeEvent( c(rv$samples, rv$endproc), {
	if (! rv$samples || ! rv$endproc ) return(NULL)
	v_options <- sort(gv$compounds)
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
	if (input$selsamples != gpgv$selsamples || input$selcmpds != gpgv$selcmpds) {
		gpgv <<- list( selsamples=input$selsamples,  selcmpds=input$selcmpds )
		gprv$xmin <- NULL
		gprv$xmax <- NULL
	}
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
	if (! input$onlyintg)
		infos <- cbind(infos, spec$TSPwidth)
	infos[, ncol(infos)] <- as.character(infos[, ncol(infos)])
	colnames(infos)[ncol(infos)] <- 'TSP width (Hz)'
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
	peaks <- peaks[ peaks$SNR>=input$snr, , drop=F]
	if (input$cmpdpeaks) { # Only the peaks of compounds
		pklist <- as.numeric(strsplit(paste(peaklist[,2], collapse=","),',')[[1]])
		peaks <- peaks[rownames(peaks) %in% pklist[!is.na(pklist)], ]
	}
	if (!is.null(gprv$xmin) && !is.null(gprv$xmax)) # if zoom 
		peaks <- peaks[ peaks$ppm>gprv$xmin & peaks$ppm<gprv$xmax, , drop=F]
	list(samples=samples, idx=idx, infos=infos, peaklist=peaklist, peaks=peaks)
})


##---------------
## Show Integration logfile
##---------------
observeEvent(input$logButton2, {
	infos <- infolist()
	if (input$onlyintg) {
		LogFile <- paste0(rq1d$TMPDIR,'/log-',infos$samples[infos$idx,1],'-',infos$samples[infos$idx,3],'.txt')
		title_log <- "Integration log"
	} else {
		LogFile <- paste0(rq1d$TMPDIR,'/output_',infos$samples[infos$idx,1],'_',infos$samples[infos$idx,3],'.txt')
		title_log <- "Quantification log"
	}
	content <- tryCatch({
		readLines(LogFile, warn = FALSE)
	}, error = function(e) {
		content <- paste("Error: open file", LogFile, "failed\n", paste(e, collapse = "\n"), "\n")
	}, warning = function(w) {
		content <- paste("Warning: open file", LogFile, "failed\n", paste(w, collapse = "\n"), "\n")
	})
	showModal(modalDialog(
		title = title_log,
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
## Output: conditional value for profile information
##---------------
output$infoprofile <- reactive({
	if (input$showprofile) return(1)
	return(0)
})
outputOptions(output, 'infoprofile', suspendWhenHidden=FALSE)
outputOptions(output, 'infoprofile', priority=20)


##---------------
## Output: sampleInfos
##---------------
output$sampleInfos <- renderDT({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	infos <- infolist()
	infos$infos
}, options = list(paging = FALSE, searching = FALSE, info = FALSE))


##---------------
## Output: samplePeaks
##---------------

get_peaks <- function() {
	peaks <- infolist()$peaks
	peaks <- cbind(rownames(peaks), peaks)
	colnames(peaks)[1] <- 'Peak id'
	peaks
}

output$samplePeaks <- renderDT({
	if (! rv$samples || ! rv$endproc ) return(NULL)
	rq1d$displayTable(get_peaks(), nbdec=4)
})

# Peak selection
output$peak_row_sel <- renderPrint({
	rowsel <- input$samplePeaks_rows_selected
	if (!is.null(rowsel) && length(rowsel)>1) {
		peaks <- get_peaks()
		M <- NULL
		idx <- which( unique(rq1d$SAMPLES[,2])==isolate(input$selsamples) )
		spec <- rq1d$specList[[idx]]
		rowsel <- rowsel[order(rowsel)]
		for( k in 2:nrow(peaks[rowsel,])) {
			PPM <- 0.5*(peaks[rowsel[k-1],3]+peaks[rowsel[k],3])
			HZ <- abs(peaks[rowsel[k],3]-peaks[rowsel[k-1],3])*spec$acq$SFO1
			RATIO <- peaks[rowsel[k],4]/peaks[rowsel[k-1],4]
			if (RATIO<1) RATIO <- 1/RATIO
			DS <- 200*abs(peaks[rowsel[k],5]-peaks[rowsel[k-1],5])/(peaks[rowsel[k],5]+peaks[rowsel[k-1],5])
			M <- rbind( M, c( paste0(peaks[rowsel[k-1],1],'-',peaks[rowsel[k],1]), 
				round(PPM,4), round(HZ,2), round(RATIO,3), round(DS,2) ) )
		}
		colnames(M) <- c('Peaks', 'Center', 'Hz', 'Ratio Amp.', 'dSigma(%)')
		M
	}
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
## Output: profile information
##---------------
output$outProfile <- renderDT({
	if (! rv$samples || ! rv$endproc) return(NULL)
	unlink(file.path(gp$IMGDIR, paste0('*.',gp$mode)))
	infos <- infolist()
	quantif <- rq1d$PROFILE$quantif
	zone <- as.integer(unique(quantif[ quantif$compound==input$selcmpds, ]$zone))
	M <- as.data.frame(quantif[ quantif$zone==zone, c(1:7), drop=F])
	M[,4] <- sapply(M[,4], function(x) gsub(',', ', ',x))
	M[,5] <- sapply(M[,5], function(x) gsub(',', ', ',x))
	M[,6] <- sapply(M[,6], function(x) gsub(',', ', ',x))
	rownames(M) <- NULL
	colnames(M) <- c('Compound','Pattern','Params1','Params2','Params3','Calibration','snrmin')
	rq1d$displayTable(M)
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
	p <- rq1d$plot_cmpds(idx, input$selcmpds, gp)
	p$x$source <- "spectrum"
	p %>% event_register("plotly_relayout")
	p
})

# Zoom detection
observeEvent(event_data("plotly_relayout", source = "spectrum"), {
	ev <- event_data("plotly_relayout", source = "spectrum")
	if (!is.null(ev[["xaxis.range[0]"]])) {
		gprv$xmin <- ev[["xaxis.range[1]"]]
		gprv$xmax <- ev[["xaxis.range[0]"]]
	} else if (!is.null(ev[["xaxis.autorange"]])) {
		gprv$xmin <- NULL
		gprv$xmax <- NULL
	}
})


##---------------
# Export the RData file
##---------------
output$exportRData <- downloadHandler(
	filename = function() { paste0('results_',gsub("\\.\\S+$","",basename(gv$NameZip)), '.RData' ) },
	content = function(file) {
		shinyjs::runjs( "document.getElementById('waitbox4').style.display = 'block';" )
		if (!input$onlyintg) {
			rq1d$get_spectra_data()
		} else {
			rq1d$quantpars$cmpdlist <- unique(sort(rq1d$PROFILE$quantif[ rq1d$PROFILE$quantif$zone %in% rq1d$res$zones, ]$compound))
			rq1d$quantpars$zones <- rq1d$res$zones
		}
		save(rq1d, file=file)
		shinyjs::runjs( "document.getElementById('waitbox4').style.display = 'none';" )
	}
)


##---------------
# Help on how to use the RData file
##---------------
observeEvent(input$help, {
	if (!file.exists("conf/Rscript.txt")) return (NULL)
	Rscript <- paste(readLines("conf/Rscript.txt"),collapse="\n")

	PATH <- '/Path of the RData file'
	S <- as.list(Sys.getenv())
	if (!is.null(S$HOME)) {
		PATH <- gsub("\\\\","/",S$HOME)
		if (OS == "windows")
			PATH <- file.path(dirname(PATH),'Downloads')
	}
	RData <- paste0("RData <- '",PATH,"/results_",gsub("\\.\\S+$","",basename(gv$NameZip)), ".RData'\n")

	txt <-paste("### Full path of the RData file", RData, Rscript, sep="\n")

	showModal(modalDialog(
		title = "R script to use the RData file",
		tags$textarea(
			id = "helpText",
			style = "width:100%;height:500px;",
			readonly = TRUE,
			txt
		),
		footer = tagList(
			actionButton("copyText", "Copy-Paste", icon = icon("copy")),
			modalButton("Close")
		),
		easyClose = TRUE,
		size = "l"
	))

})


##---------------
# Copy-paste the R script
##---------------
observeEvent(input$copyText, {
	session$sendCustomMessage("copyToClipboard", "helpText")
})