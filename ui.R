#------------------------------------------------
# ID ui.R
# Project: RnmrQuant1D
# (C) 2026 - D. JACOB - INRAE
#------------------------------------------------


#---------------------
# User Interface
#---------------------

# Define UI for dataset viewer app ----
ui <- fluidPage(
	# Custom CSS Styles
	tags$head(
		tags$link(rel="icon", href="images/favicon.ico"),
		tags$link(rel="stylesheet", type="text/css",href="style.css"),
		tags$script(HTML("<!-- window.onbeforeunload = function() { return true; };
		document.addEventListener('contextmenu', function(e) { e.preventDefault(); }); -->
		$(document).on('shiny:connected', function(event) {
			function resizePanel() {
				document.documentElement.style.setProperty('--heightPanel', (window.innerHeight-140)+'px');
				document.documentElement.style.setProperty('--heightTabs', (window.innerHeight-180)+'px');
			}
			window.addEventListener('resize', resizePanel);
			resizePanel();
			Shiny.addCustomMessageHandler('closeWindow', function(message) { window.open('', '_self').close(); });
			window.addEventListener('beforeunload', function(e) { Shiny.setInputValue('reloading', true, {priority: 'event'}); });
		}); $(document).on('shown.bs.tab', function(e) {
				setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 150);
		});"))
	),

	shinyjs::useShinyjs(debug = TRUE, html = FALSE),

	# App title
	withTags(
		table(tr(td(width = "15",""), td(img(src="images/favicon.ico", height = 40, width = 50)),
			td(width = "10",""), td(titlePanel(HTML(TITLE)))))
	),

	fluidRow ( class="glob",

	# Step 1 - File selection
		conditionalPanel(condition="output.fileUploaded==0",
			mainPanel(width=12, tabsetPanel(id = "intabs",
				tabPanel('Upload', tags$div(class="tabs",
					column(3,
						tags$br(),tags$br(),
						selectInput("vendor", "Instrument/Vendor/Format:",  selectVendor, selected = "sinput"),
						conditionalPanel(condition="output.FormatSelected==1",
							fileInput( 'zipfile', 'ZIP file', accept = c( 'application/zip', '.7z' ) )
						),
						conditionalPanel(condition="output.ZipUploaded==1",
							fileInput( 'samplefile', 'Samples file (Tabular format or XLSX)', accept = c( '.tsv', '.txt', '.xlsx' ) ),
							checkboxInput("onlyintg", "Only integration", FALSE)
						),
						conditionalPanel(condition="output.ZipUploaded==1 && output.allUploaded==0",
							conditionalPanel(condition="input.onlyintg",
								bsButton("goButton", label = "Import", style="info", icon = icon("file-import")),
								bsButton("resetBtn1", label = "Reset", style="info", disabled = FALSE),
							),
							conditionalPanel(condition="!input.onlyintg",
								bsButton("resetBtn1", label = "Reset", style="info", disabled = FALSE)
							)
						),
						conditionalPanel(condition="output.ZipUploaded==1 && output.allUploaded==0",
								tags$br(), tags$br(),
								downloadButton("exportTMPL", "Export Template", class="expbtn")
						),
						conditionalPanel(condition="output.allUploaded==1",
							bsButton("goButton", label = "Import", style="info", icon = icon("file-import")),
							bsButton("resetBtn1", label = "Reset", style="info", disabled = FALSE)
						),
						tags$br(), tags$br(),
						tags$div( id="waitbox1", class="waitbox", style="display: none;"),
						tags$br(), tags$br()
					),
					column(9,
						tags$br(), tags$a(
							href = "images/workflow.png", target = "_blank", 
							img(src="images/workflow.png", width = 650)
						)
					),
					column(12, bsAlert("AlertUpLoad"))
				)),
				tabPanel("Help", tags$div(class="tabs",
					div(class = "markdown-body", includeMarkdown("www/help.md"))
				))
			))
		),

	# Step 2 - Core of the application
		conditionalPanel(condition="output.fileUploaded==1 && output.fileProcessed==1", column(12,
			mainPanel(width=12, tabsetPanel(id = "outtabs",
		
		#-----------------------------------------------
		# Input Files
		#-----------------------------------------------
				tabPanel('Input files', value = "inpfiles", tags$div(class="tabs",
					column(12,
						verbatimTextOutput("zipLog"),
						dataTableOutput("sampleTable"),
						bsButton("resetBtn2", label = "Reset", style="info", disabled = FALSE)
					)
				)),

		#-----------------------------------------------
		# Integration
		#-----------------------------------------------
				tabPanel('Integration', value = "intg", tags$div(class="tabs",
					tags$br(), tags$br(),
					column(4,
						fluidRow( column(9,
							selectInput("sequence", "Sequence (PULSE)", choices = NULL, width = "100%")
						)),
						conditionalPanel(condition = "!input.externalIntg",
							fluidRow(
								column(9,
									selectInput("intgprofile", "Quantification profile", choices = NULL, width = "100%")),
								column(3, style = "margin-top: 25px;", 
									bsButton("viewIntgBtn", label = "View", style="info", disabled = FALSE))
							)
						),
						conditionalPanel(condition = "input.externalIntg",
							fluidRow(
								column(9,
									fileInput(inputId = "externIntgFile", label = "Quantification profile", placeholder = "No selected profile", width = "100%")),
								column(3, style = "margin-top: 25px;", 
									bsButton("viewIntgBtn", label = "View", style="info", disabled = FALSE))
							)
						),
						checkboxInput("externalIntg", "External profile", value = FALSE)
					),
					column(8,
						div( style="max-height: 200px; overflow: scroll;",
							selectInput(inputId = "listcmpds", label = "(Un)Select Compounds", width="100%",
								choices = c(), selected = NULL, multiple = TRUE),
							bsButton("intgInvBtn", label = "Invert selection", style="info", disabled = FALSE)
						)
					),
					column(12, tags$br(), tags$br()),
					column(12,
						div( style="max-height: 300px; overflow: scroll;",
							selectInput( inputId = "listsamples", width="100%", label   = "(Un)Select Samples",
								choices = c(), selected = NULL, multiple = TRUE)
						),
						div( style="margin-top: -15px;",
							textInput("intgpattern", label = "", value = "", placeholder="Regexp pattern for sample selection")
						)
					),
					column(12,
						tags$br(), tags$br(),
						fluidRow(
							conditionalPanel(condition="output.endIntg==0",
								bsButton("intgButton", label = "Launch Integration", style="info", icon = icon("rocket")),
							),
							conditionalPanel(condition="output.endIntg==1",
								bsButton("intgButton", label = "Launch Integration", style="info", icon = icon("rocket")),
								downloadButton("exportWBintg", "Export Workbook", class="expbtn"),
								tags$div( id="waitbox2", class="waitbox", style="display: none;")
							)
						)
					),
					column(12, tags$br(), bsAlert("AlertIntg")),
					column(12,
						tags$br(),
						conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(id="progressbar",
							HTML('<table border=0 CELLPADDING=0 CELLSPACING=0>
								<tr><td colspan=2 style="font-weight:bold;"><span id="pbtitle"></span></td></tr>
								<tr><td id="pbleft" style="background-color: #337ab7; width: 0px; height: 10px;"></td>
									<td id="pbright" style="background-color: #dedede; width: 700px; height: 10px;"></td>
									<td style="font-size:10pt;text-decoration:none;font-weight:bold;">&nbsp;<span id="pbval">0</span>%</td>
								</tr></table>')
						)),
						verbatimTextOutput("outIntg")
					)
				)),

		#-----------------------------------------------
		# Calibration
		#-----------------------------------------------
				tabPanel('Calibration', value = "calib", tags$div(class="tabs",
					tags$br(), tags$br(),
					conditionalPanel(condition="output.fileQCQS==1", column(12,
						column(3,
							selectInput(inputId = "sequence2", label = "Sequence (PULSE)", choices = NULL, width = "100%"),
							checkboxInput("deconv", "Peak fitting", FALSE)
						),
						column(3,
							numericInput(inputId = "thresfP", label = "CV threshold %", value = 6, width = "100%"),
							checkboxInput("qbl", "Baseline Correction", TRUE)
						),
						column(3,
							conditionalPanel(condition = "!input.externalCalib",
								fluidRow(
									column(9,
										selectInput("calibprofile", "Calibration profile", choices = NULL, width = "100%")),
									column(3, style = "margin-top: 25px;", 
										bsButton("viewCalibBtn", label = "View", style="info", disabled = FALSE))
								)
							),
							conditionalPanel(condition = "input.externalCalib",
								fluidRow(
									column(9,
										fileInput(inputId = "externCalibFile", label = "Calibration profile", placeholder = "No selected profile", width = "100%")),
									column(3, style = "margin-top: 25px;", 
										bsButton("viewCalibBtn", label = "View", style="info", disabled = FALSE))
								)
							),
							checkboxInput("externalCalib", "External profile", value = FALSE)
						),
						column(12,
							tags$br(), 
							bsButton("calibButton", label = "Launch Calibration", style="info", icon = icon("rocket")),
							bsButton("logButton", label = "Log", style="info", disabled = TRUE)
						)
					)),
					column(12, tags$br(), bsAlert("AlertCalib")),
					column(12,
						tags$br(), 
						withSpinner(verbatimTextOutput("outCalib"), type=1),
						tags$br(),
						withSpinner(uiOutput("outPulcon"), type=1),
						tags$br(),
						withSpinner(plotlyOutput("QC_estimation"), type=1)
					)
				)),

		#-----------------------------------------------
		# Quantification
		#-----------------------------------------------
				tabPanel('Quantification', value = "quant", tags$div(class="tabs",
					tags$br(), tags$br(),
					column(4,
						conditionalPanel(condition = "!input.externalQuant",
							fluidRow(
								column(9,
									selectInput("quantprofile", "Quantification profile", choices = NULL, width = "100%")),
								column(3, style = "margin-top: 25px;", 
									bsButton("viewQuantBtn", label = "View", style="info", disabled = FALSE))
							)
						),
						conditionalPanel(condition = "input.externalQuant",
							fluidRow(
								column(9,
									fileInput(inputId = "externQuantFile", label = "Quantification profile", placeholder = "No selected profile", width = "100%")),
								column(3, style = "margin-top: 25px;", 
									bsButton("viewQuantBtn", label = "View", style="info", disabled = FALSE))
							)
						),
						checkboxInput("externalQuant", "External profile", value = FALSE)
					),
					column(8,
						div( style="max-height: 200px; overflow: scroll;",
							selectInput(inputId = "listcmpds2", label = "(Un)Select Compounds", width="100%",
								choices = c(), selected = NULL, multiple = TRUE),
							bsButton("quantInvBtn", label = "Invert selection", style="info", disabled = FALSE)
						)
					),
					column(12, tags$br(), tags$br()),
					column(12,
						div( style="max-height: 300px; overflow: scroll;",
							selectInput( inputId = "listsamples2", width="100%", label   = "(Un)Select Samples",
								choices = c(), selected = NULL, multiple = TRUE)
						),
						div( style="margin-top: -15px;",
							textInput("quantpattern", label = "", value = "", placeholder="Regexp pattern for sample selection")
						)
					),
					column(12,
						tags$br(), tags$br(),
						fluidRow(
							conditionalPanel(condition="output.endQuant==0",
								bsButton("quantButton", label = "Launch Quantification", style="info", icon = icon("rocket")),
							),
							conditionalPanel(condition="output.endQuant==1",
								bsButton("quantButton", label = "Launch Quantification", style="info", icon = icon("rocket")),
								downloadButton("exportWBquant", "Export Workbook", class="expbtn"),
								tags$div( id="waitbox3", class="waitbox", style="display: none;")
							)
						)
					),
					column(12, tags$br(), bsAlert("AlertQuant")),
					column(12,
						tags$br(),
						conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(id="progressbar2",
							HTML('<table border=0 CELLPADDING=0 CELLSPACING=0>
								<tr><td colspan=2 style="font-weight:bold;"><span id="pbtitle2"></span></td></tr>
								<tr><td id="pbleft2" style="background-color: #337ab7; width: 0px; height: 10px;"></td>
									<td id="pbright2" style="background-color: #dedede; width: 700px; height: 10px;"></td>
									<td style="font-size:10pt;text-decoration:none;font-weight:bold;">&nbsp;<span id="pbval2">0</span>%</td>
								</tr></table>')
						)),
						verbatimTextOutput("outQuant")
					)
				)),

		#-----------------------------------------------
		# Spectra Viewer
		#-----------------------------------------------
				tabPanel('Spectra viewer', value = "viewer", tags$div(class="tabs",
					tags$br(),
					column(12, 
						column(1, style="width: 1%;", tags$br()),
						column(10,
							dataTableOutput("sampleInfos"),
							tags$br(), tags$br()
						),
						column(1, 
							tags$br(), tags$br(),
							bsButton("logButton2", label = "Log", style="info", disabled = FALSE)
						)
					),
					tags$br(), tags$br(),
					fluidRow(
						column(1, style="width: 1%;", tags$br()),
						column(2,
							selectInput( inputId = "selsamples", label   = "Select a sample", width="100%", 
								choices = c(), selected = NULL, multiple = FALSE, selectize=FALSE, size=30)
						),
						column(6, style="width: 58%;",
							column(12,
								column(2,
									radioButtons("tags", "Tags:", c("None" = "none", "Id" = "peak", "Name" = "name"),
										inline = FALSE)
								),
								column(2,
									radioButtons("showlegend", "Legend:", c("None" = "none", "Top" = "top", "Bottom" = "bottom"),
										selected='bottom', inline = FALSE)
								),
								column(2,
									checkboxInput("showgrid", "Show gridlines", TRUE),
									checkboxInput("axislbl", "Show axis labels", FALSE),
									checkboxInput("yaxis", "Show Y axis", TRUE)
								),
								column(3,
									checkboxInput("showpeaklist", "Show peaklist", FALSE),
									checkboxInput("residus", "Plot with residus", FALSE),
									checkboxInput("specbl", "Spectrum with BL correction", FALSE)
								)
							),
							column(12, 
								withSpinner(plotlyOutput("spectrum"), type=7),
								conditionalPanel(condition="output.infopeaks==1",
									verbatimTextOutput("outInfos")
								),
								conditionalPanel(condition="input.showpeaklist==1",
									column(12,
										column(6,
											checkboxInput("cmpdpeaks", "Only the peaks of compounds", TRUE)
										),
										column(6,
											numericInput(inputId = "snr", label = "SNR Threshold", value = 4)
										)
									),									
									DTOutput("samplePeaks")
								)
							)
						),
						column(2,
							selectInput(inputId = "selcmpds", label = "Select a compound", width="100%",
								choices = c(), selected = NULL, multiple = FALSE, selectize=FALSE, size=30)
						)
					)
				)),
				
				tabPanel("Help", tags$div(class="tabs",
					div(class = "markdown-body", includeMarkdown("www/help.md"))
				))

			))
		)),

	# Footer
		column(12, HTML(paste0('<hr><br>
  <div class="footer"><span style="font-size:12px;">Version ',VERSION,' - ',CPRGHT,' - </span>
  <a href="https://www.bibs.inrae.fr/" target="_blank"><img style="vertical-align:middle;height:10px; width:28px;" src="images/INRAE_logo.png"/></a></div>'
		)))
	)
)
