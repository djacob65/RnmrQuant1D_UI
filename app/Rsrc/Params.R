#------------------------------------------------
# ID Params.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------



# When Ctrl-Q/I/M is pressed, open the parameters modal
observeEvent(input$keyEvent, {
	if (input$keyEvent %in% c("Ctrl-Q","Ctrl-I","Ctrl-M")) showModal(gparamsModal())
	runjs('Shiny.onInputChange("keyEvent", "");')
})


#----------------------------------------------------
# Modal dialog UI for global parameters
#----------------------------------------------------
gparamsModal <- function()
{
	V <- sessionInfo();
	p <- ls(V$loadedOnly)
	packages <- NULL
	for (i in 1:length(p))
		packages <- c( packages, paste0(V$loadedOnly[[p[i]]]$Package,'_',V$loadedOnly[[p[i]]]$Version) )
	p <- ls(V$otherPkgs)
	others <- NULL
	for (i in 1:length(p))
		others <- c( others, paste0(V$otherPkgs[[p[i]]]$Package,'_',V$otherPkgs[[p[i]]]$Version) )

	modalDialog(
		mainPanel(width=12, tabsetPanel(id = "paramtabs", type="pills",

			#----------------------------------------------------
			# Global parameters
			#----------------------------------------------------
			tabPanel('Parameters', tags$div(class="tabs",
				tags$br(), tags$br(),
				tags$table(
					tags$tr(tags$td(tags$strong("Global parameters"))), tags$tr(tags$td(tags$hr())),
					tags$tr(tags$td(width="400px", tags$strong("NB MAX CORES (0 means Auto)"))),
					tags$tr(tags$td(
						numericInput("max_ncpu", NULL, gv$max_ncpu, min = 0, max = parallel::detectCores(), step=1))),
					tags$tr(tags$td(width="400px",
						checkboxInput('affinity', 'Set CPU affinity', gv$affinity))),
					tags$tr(tags$td(tags$br())),
					tags$tr(tags$td(width="400px", tags$strong("Default dilution factor value"))),
					tags$tr(tags$td(
						numericInput("fac_dilution", NULL, gv$fac_dilution, min = 0.7, max = 1, step=0.1))),
					tags$tr(tags$td(tags$br())),
					tags$tr(tags$td(textInput("QCtype", "QC type name", QCtype))),
					tags$tr(tags$td(textInput("QStype", "QS type name", QStype))),
					tags$tr(tags$td(tags$br())),
					tags$tr(tags$td(
						bsButton("okgparams", label = "Submit", style="info", icon = icon("gear"))
					))
				)
			)),
	
			#----------------------------------------------------
			# Compound colors
			#----------------------------------------------------
			tabPanel('Colors', tags$div(class="tabs",
				tags$br(), tags$br(),
				tags$strong("Compound color settings in the spectra viewer"), tags$hr(),
				fluidRow(
					column(
						width = 6,
						h4("Compound colors"),
						colorTextInput("col1", "Color 1", gp$colcpmds[1]),
						colorTextInput("col2", "Color 2", gp$colcpmds[2]),
						colorTextInput("col3", "Color 3", gp$colcpmds[3]),
						colorTextInput("col4", "Color 4", gp$colcpmds[4]),
						colorTextInput("col5", "Color 5", gp$colcpmds[5]),
						colorTextInput("col6", "Color 6", gp$colcpmds[6])
					),
					column(
						width = 6,
						h4("Color selector"),
						uiOutput("picker_ui")
					)
				)
			)),

			#----------------------------------------------------
			# Information
			#----------------------------------------------------
			tabPanel('Information', tags$div(class="tabs",
				tags$br(), tags$br(),
				tags$table(
					tags$tr(tags$td(colspan = 2, tags$strong(paste("RnmrQuant1D version",VERSION,' - ',CPRGHT)))),
					tags$tr(tags$td(colspan = 2, tags$hr())),
					tags$tr(tags$td(colspan = 2, V$R.version$version.string)),
					tags$tr(tags$td(colspan = 2, paste("Running under:",V$running))),
					tags$tr(tags$td(colspan = 2, paste("platform:",V$platform))),
					tags$tr(tags$td(colspan = 2, tags$br())),
					tags$tr(tags$td(style="vertical-align: top;", "Blas:"), tags$td(V$BLAS)),
					tags$tr(tags$td(style="vertical-align: top;", "Lapack:"), tags$td(V$LAPACK)),
					tags$tr(tags$td(colspan = 2, tags$br())),
					tags$tr(tags$td(style="vertical-align: top;", "locale:"), tags$td(gsub(';',', ', V$locale))),
					tags$tr(tags$td(colspan = 2, tags$br())),
					tags$tr(tags$td(style="vertical-align: top;", "Base Packages:"), tags$td(paste(V$basePkgs, collapse=', '))),
					tags$tr(tags$td(colspan = 2, tags$br())),
					tags$tr(tags$td(style="vertical-align: top;", "Loarded Packages:"), tags$td( paste(packages, collapse=', ') )),
					tags$tr(tags$td(colspan = 2, tags$br())),
					tags$tr(tags$td(style="vertical-align: top;", "Others Packages:"), tags$td( paste(others, collapse=', ') ))
				)
			))
		)),
		footer = modalButton("Close"), 
		size="l", easyClose = TRUE
	)
}


# When OK button is pressed, affects the global variables
observeEvent(input$okgparams, {
	gv$max_ncpu <<- min(input$max_ncpu,parallel::detectCores())
	gv$affinity <<- input$affinity
	gv$fac_dilution <<- input$fac_dilution
	QCtype <<- input$QCtype
	QStype <<- input$QStype
	QCQS <<-c( QCtype, QStype )
	sampleTypes <<- c( SAMPLEtype, QCQS )
	removeModal()
})


# ---- Helper: "standard" textInput with an onfocus event that notifies Shiny ----
# We reconstruct the HTML of a standard textInput to preserve the Bootstrap
# styling, but add an onfocus attribute to the <input> element to
# indicate which field is active (input$active_input).
colorTextInput <- function(inputId, label, value)
{
	tags$div(
	class = "form-group shiny-input-container",
	tags$label(label, `for` = inputId),
	tags$input(
		id = inputId,
		type = "text",
		class = "form-control",
		value = value,
		onfocus = sprintf("Shiny.setInputValue('active_input', '%s')", inputId)
	)
	)
}


# Responsive storage of the 4 colors
colors <- reactiveValues(
	col1 = gp$colcpmds[1],
	col2 = gp$colcpmds[2],
	col3 = gp$colcpmds[3],
	col4 = gp$colcpmds[4],
	col5 = gp$colcpmds[5],
	col6 = gp$colcpmds[6]
)

# Active field (updated by the JS onfocus above)
active <- reactiveVal(NULL)

observeEvent(input$active_input, {
	active(input$active_input)
})

# Displays the colourInput corresponding to the active field.
output$picker_ui <- renderUI({
	req(active())
	colourInput(
		inputId = "picker",
		label = paste("Choose color ", which(names(colors)==active())),
		value = colors[[active()]],
		allowTransparent = TRUE,
		closeOnClick = TRUE
	)
})

# When a color is selected in the colourInput 
# -> updates the corresponding textInput + the stored value
observeEvent(input$picker, {
	req(active())
	colors[[active()]] <- input$picker
	gp$colcpmds[which(names(colors)==active())] <<- input$picker
	updateTextInput(session, active(), value = input$picker)
}, ignoreInit = TRUE)


# If the user types a value directly into a textInput,
# keep the stored value synchronized.
observeEvent(input$col1, { colors$col1 <- gp$colcpmds[1] <<- input$col1 }, ignoreInit = TRUE)
observeEvent(input$col2, { colors$col2 <- gp$colcpmds[2] <<- input$col2 }, ignoreInit = TRUE)
observeEvent(input$col3, { colors$col3 <- gp$colcpmds[3] <<- input$col3 }, ignoreInit = TRUE)
observeEvent(input$col4, { colors$col4 <- gp$colcpmds[4] <<- input$col4 }, ignoreInit = TRUE)
observeEvent(input$col5, { colors$col5 <- gp$colcpmds[5] <<- input$col5 }, ignoreInit = TRUE)
observeEvent(input$col6, { colors$col6 <- gp$colcpmds[6] <<- input$col6 }, ignoreInit = TRUE)
