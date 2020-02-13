#' FAS module

fasAppUI <- function(id) {
	ns <- NS(id)
	sidebarLayout(
		# * sidebar panel for FAS input/options -----------------
		sidebarPanel(
			width = 3,
			# ** FAS location ===================================
			conditionalPanel(
			    condition = "output.checkFasStatus == 0", ns = ns,
			    
			    selectInput(
			        ns("whereFas"), "FAS not found! Please:",
			        choice = c("Install FAS", "Provide FAS path"),
			        selected = "Provide FAS path"
			    ),
			    conditionalPanel(
			        condition = "input.whereFas == 'Provide FAS path'",
			        ns = ns,
			        shinyFilesButton(
			            ns("greedyFasFile"),
			            "FAS location?" ,
			            title = "Please provide greedyFAS.py file:",
			            multiple = FALSE,
			            buttonType = "default"
			        )
			    ),
			    conditionalPanel(
			        condition = "input.whereFas == 'Install FAS'",
			        ns = ns,
			        bsButton(
			            "installFas", "Install FAS", 
			            onclick = "window.open('https://bionf.github.io/FAS/#installation', '_blank')"
			        )
			    )
			),
			hr(),

			# ** fasta input =======================================
			h3("Input and configurations"),
			hr(),
			shinyFilesButton(
				ns("seedInput"), "Input seed file!" ,
				title = "Please provide fasta file for seed:",
				multiple = FALSE,
				buttonType = "default", class = NULL
			),
			
			conditionalPanel(
			    condition = 'input.addQueryCheck', ns = ns,
			    br(),
			    shinyFilesButton(
			        ns("queryInput"), "Input query file!" ,
			        title = "Please provide fasta file for query:",
			        multiple = FALSE,
			        buttonType = "default", class = NULL
			    )
			),
			br(),
			checkboxInput(
			    ns("addQueryCheck"),
			    strong("Add query protein(s)"),
			    value = FALSE,
			    width = NULL
			),
			hr(),
			
			# ** job ID ========================================================
			textInput(ns("fasJob"), "Job ID", value = randFn(1)),
			bsPopover(
			    ns("fasJob"),
			    "",
			    paste(
			        "Name of job and log file(s)."
			    ),
			    "bottom"
			),
			bsButton(ns("newFasJob.btn"), "New job ID"),
			hr(),

			# ** annoFAS options ===============================================
			strong("annoFAS options"),
			textInput(
			    ns("seedName"), "Seed Name",
			    value = "seed", placeholder = "seed"
			),
			bsPopover(
			    ns("seedName"),
			    "",
			    paste(
			        "Name of annotation folder for seed protein(s)."
			    ),
			    "bottom"
			),
			
			conditionalPanel(
			    condition = "input.addQueryCheck", ns = ns,
			    textInput(
			        ns("queryName"), "Query Name",
			        value = "query", placeholder = "query"
			    ),
			    bsPopover(
			        ns("queryName"),
			        "",
			        paste(
			            "Name of annotation folder for query protein(s)."
			        ),
			        "bottom"
			    )
			),
			
			shinyDirButton("outAnnoDir", "Output directory", "Upload"),

			hr(),
			
			# ** greedyFAS options ==================================
			uiOutput(ns("seedID.ui")),
			bsPopover(
				ns("seedID"),
				"",
				paste(
					"Specifie the sequence identifier of the seed",
					"sequence in the reference protein set.",
					"If not provided, the program will attempt to",
					"determine it automatically."
				),
				"bottom"
			),

			conditionalPanel(
			    condition = "input.addQueryCheck", ns = ns,
			    uiOutput(ns("queryID.ui")),
			    bsPopover(
			        ns("queryID"),
			        "",
			        paste(
			            "Specifie the sequence identifier of the query",
			            "sequence in the reference protein set.",
			            "If not provided, the program will attempt to",
			            "determine it automatically."
			        ),
			        "bottom"
			    )
			)
		),
		# * main panel for FAS run ----------------------------
		mainPanel(
			width = 9,
			column(
				6,
				uiOutput(ns("annoBtn.ui")),
				hr(),
				strong("annoFAS OPTIONS"),
				br(), br(),
				uiOutput(ns("annoOptions.ui")),
				hr(),
				strong("Log file"),
				verbatimTextOutput(ns("logAnnoLocation")),
				strong("Output files"),
				verbatimTextOutput(ns("outputAnnoLocation")),
				hr(),
				verbatimTextOutput(ns("annoCmdText")),
				verbatimTextOutput(ns("annoLog"))
			),
			column(
				6,
				uiOutput(ns("fasBtn.ui")),
				hr(),
				strong("greedyFAS OPTIONS"),
				br(), br(),
				uiOutput(ns("greedyOptions.ui")),
				hr(),
				strong("Log file"),
				verbatimTextOutput(ns("logFasLocation")),
				strong("Output files"),
				verbatimTextOutput(ns("outputFasLocation")),
				hr(),
				verbatimTextOutput(ns("fasCmdText")),
				verbatimTextOutput(ns("fasLog"))
			)
		)
	)
}

fasApp <- function(input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
	ns <- session$ns
	
	# get greedyFAS location ===================================================
	output$checkFasStatus <- reactive({
	    fasLocation <- suppressWarnings(
	        system("which greedyFAS", intern = TRUE)
	    )
	    if (!is.na (fasLocation[1])){
	        return(1)
	    } else return(0)
	})
	outputOptions(output, "checkFasStatus", suspendWhenHidden = FALSE)
	
	getFasPath <- reactive({
	    fasLocation <- suppressWarnings(
	        system("which greedyFAS", intern = TRUE)
	    )
	    if (!is.na (fasLocation[1])){
	        return(fasLocation[1])
	    } else {
	        shinyFileChoose(
	            input, "greedyFasFile", roots = homePath, session = session
	        )
	        req(input$greedyFasFile)
	        file_selected <- parseFilePaths(homePath, input$greedyFasFile)
	        return(as.character(file_selected$datapath))
	    }
	})
	
	output$fasLocation <- renderText({
	    fasPath <- getFasPath()
	    paste(
	        "greedyFAS found at", fasPath
	    )
	})
	
	# get annoFAS location =====================================================
	getAnnoPath <- reactive({
	    fasPath <- getFasPath()
	    annoPath <- stringr::str_replace(fasPath, "greedyFAS", "annoFAS")
	    return(annoPath)
	})
	
	output$annoLocation <- renderText({
	    annoPath <- getAnnoPath()
	    paste(
	        "annoFAS found at", annoPath
	    )
	})

	# get input fasta (seed and query) =========================================
	getSeedPath <- reactive({
		shinyFileChoose(
		    input, "seedInput", roots = homePath, session = session,
		    filetypes = c('', 'fa', 'fasta')
		)
		file_selected <- parseFilePaths(homePath, input$seedInput)
		req(input$seedInput)
		return(as.character(file_selected$datapath))
	})

	getQueryPath <- reactive({
	    shinyFileChoose(
	        input, "queryInput", roots = homePath, session = session,
	        filetypes = c('', 'fa', 'fasta')
	    )
	    file_selected <- parseFilePaths(homePath, input$queryInput)
	    req(input$queryInput)
	    return(as.character(file_selected$datapath))
	})

	# get list of seed and query sequence IDs ==================================
	output$seedID.ui <- renderUI({
		seqIDs <- getSeqID(getSeedPath())
		selectInput(
			ns("seedID"), "Seed ID",
			choices = seqIDs,
			selected = seqIDs[1]
		)
	})

	output$queryID.ui <- renderUI({
	    seqIDs <- getSeqID(getQueryPath())
	    selectInput(
	        ns("queryID"), "Query ID",
	        choices = seqIDs,
	        selected = seqIDs[1]
	    )
	})

	# get output path ==========================================================
	# getOutputPath <- reactive({
	#     shinyDirChoose(
	#         input, 'outAnnoDir', roots=c(wd='.') #roots = homePath
	#     )
	#     outputPath <- parseDirPath(homePath, input$outAnnoDir)
	#     # outputPath <-
	#     #     file.path(home, paste(unlist(outAnnoDir()$path[-1]), collapse = .Platform$file.sep))
	#     # req(input$queryInput)
	#     return(as.character(outputPath))
	# })
	
	# generate new job ID ======================================================
	observeEvent(input$newFasJob.btn, {
	    jobID <- randFn(1)
	    updateTextInput(session, "fasJob", "Job ID", value = jobID)
	})

	# annoFAS options ==========================================================
	annoOptions <- reactive({
	    fasta <- paste0("--fasta=", getSeedPath())
	    if (input$addQueryCheck == TRUE) {
	        if (input$annoObj == "query") {
	            fasta <- paste0("--fasta=", getQueryPath())
	        }
	    }
		# path <- paste0("--path=", getOutputPath())
		path <- paste0("--path=", "/Users/bemun/Desktop/bionf/trvinh_github/phylosophy/tmp")
		name <- paste0("--name=", input$seedName)
		return(
			c(fasta, path, name)
		)
	})

	output$annoOptions.ui <- renderUI({
		HTML(paste(annoOptions(), collapse = "<br/>"))
	})

	# RUN annFAS ===============================================================
	output$annoBtn.ui <- renderUI({
	    if (length(input$seedInput) > 1) {
	        tagList(
	            bsButton(
	                ns("doAnno"), "Run annoFAS",
	                style = "warning", disabled = FALSE
	            ),
	            actionButton(ns("stopAnno"),label = "Stop"),
	            actionButton(ns("newAnno"),label = "New job"),
	            conditionalPanel(
	                condition = "input.addQueryCheck", ns = ns,
	                selectInput(
	                    ns("annoObj"), "for", choices = c("seed", "query"),
	                    selected = "seed"
	                )
	            ), 
	            textOutput(ns("annoLocation"))
	        )
	    }
	})
	
	observeEvent(input$newAnno, {
	    updateButton(session, ns("doAnno"), disabled = FALSE)
	    updateButton(session, ns("stopAnno"), disabled = FALSE)
	})
	
	annoCmd <- reactive({
	    return(
	        paste(
	            getAnnoPath(),
	            paste(annoOptions(), collapse = " ")
	        )
	    )
	})
	
	output$annoCmdText <- renderText({
	    # paste("python", fasCmd())
	    paste(annoCmd())
	})
	
	rvAnno <- reactiveValues(
	    textstream = c(""),
	    timer = reactiveTimer(1000),
	    started = FALSE
	)
	observeEvent(input$doAnno, {
	    rvAnno$started <- TRUE
	    cmd <- paste(
	        annoCmd(),
	        ">>",
	        paste0(input$fasJob, ".anno.log")
	    )
	    # system2("python", cmd, wait = FALSE)
	    system(cmd, wait = FALSE)
	    updateButton(session, ns("doAnno"), disabled = TRUE)
	    updateButton(session, ns("newFasJob.btn"), disabled = TRUE)
	})
	
	observeEvent(input$stopAnno, {
	    rvAnno$started <- FALSE
	    system2("rm", "*.anno.log")
	    updateButton(session, ns("stopAnno"), disabled = TRUE)
	})
	
	observe({
	    rvAnno$timer()
	    if (isolate(rvAnno$started)) {
	        rvAnno$textstream <- suppressWarnings(
	            readLines(paste0(input$fasJob, ".anno.log"),  n = -1) %>% 
	                tail(50) %>% paste(collapse = "\n")
	        )
	    }
	})
	output$annoLog <- renderText({
	    rvAnno$textstream
	})
	
	
	# greedyFAS options ========================================================
	fasOptions <- reactive({
	    return(
	        c("blablabla")
	    )
	})
	
	# RUN greedyFAS ============================================================
	output$fasBtn.ui <- renderUI({
	    if (length(input$seedInput) > 1 && length(input$queryInput) > 1) {
	        tagList(
	            bsButton(
	                ns("doFAS"), "Run greedyFAS",
	                style = "warning", disabled = FALSE
	            ),
	            actionButton(ns("stopFAS"),label = "Stop"),
	            actionButton(ns("newFAS"),label = "New job"),
	            textOutput(ns("fasLocation"))
	        )
	    }
	})

	observeEvent(input$newFAS, {
		updateButton(session, ns("doFAS"), disabled = FALSE)
		updateButton(session, ns("stopFAS"), disabled = FALSE)
	})

	fasCmd <- reactive({
		return(
			paste(
				getFasPath(),
				paste(fasOptions(), collapse = " ")
			)
		)
	})

	output$fasCmdText <- renderText({
		# paste("python", fasCmd())
	    paste(fasCmd())
	})

	rvFas <- reactiveValues(
		textstream = c(""),
		timer = reactiveTimer(1000),
		started = FALSE
	)
	observeEvent(input$doFAS, {
		rvFas$started <- TRUE
		cmd <- paste(
			fasCmd(),
			">>",
			paste0(input$fasJob, ".fas.log")
		)
		# system2("python", cmd, wait = FALSE)
		system(cmd, wait = FALSE)
		updateButton(session, ns("doFAS"), disabled = TRUE)
		updateButton(session, ns("newFasJob.btn"), disabled = TRUE)
	})

	observeEvent(input$stopFAS, {
		rvFas$started <- FALSE
		system2("rm", "*.fas.log")
		updateButton(session, ns("stopFAS"), disabled = TRUE)
	})

	observe({
		rvFas$timer()
		if (isolate(rvFas$started)) {
			rvFas$textstream <- suppressWarnings(
			    readLines(paste0(input$fasJob, ".fas.log"),  n = -1) %>% 
			        tail(50) %>% paste(collapse = "\n")
			)
		}
	})
	output$fasLog <- renderUI({
		HTML(rvFas$textstream)
	})

	# report results ===========================================================
	# output$logLocation <- renderText({
	#     paste0(getwd(), "/", input$fasJob, ".log")
	# })
	#
	# output$outputLocation <- renderText({
	#     # get default output folder of hamstr
	#     dataDir <- ""
	#     oneseqPath <- getOneseqPath()
	#     if (length(grep("oneSeq.pl", getOneseqPath()))) {
	#         dataDir <- stringr::str_replace(
	#             oneseqPath, "/bin/oneSeq.pl", "/data"
	#         )
	#     } else {
	#         dataDir <- stringr::str_replace(
	#             oneseqPath, "/bin/oneSeq", "/data"
	#         )
	#     }
	#     # return output files
	#     faOut <- paste0(dataDir, "/", input$fasJob, ".extended.fa")
	#     if (input$useFAS == TRUE) {
	#         ppOut <- paste0(dataDir, "/", input$fasJob, ".phyloprofile")
	#         domainFwOut <- paste0(dataDir, "/", input$fasJob, "_1.domains")
	#         domainRvOut <- paste0(
	#             "[", dataDir, "/", input$fasJob, "_1.domains", "]"
	#         )
	#         paste(faOut, ppOut, domainFwOut, domainRvOut, sep = "\n")
	#     } else {
	#         faOut
	#     }
	# })
}