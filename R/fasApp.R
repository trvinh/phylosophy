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
			    shinyFilesButton(
			        ns("greedyFasFile"),
			        "FAS location?" ,
			        title = "Please provide greedyFAS.py file:",
			        multiple = FALSE,
			        buttonType = "default"
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

			# ** annoFAS options ==================================
			checkboxInput(
			    ns("addQueryCheck"),
			    strong("Add query protein(s)"),
			    value = FALSE,
			    width = NULL
			),
			
			hr(),
			strong("annoFAS options"),

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
			textInput(
			    ns("seedName"), "Seed ID",
			    value = "seed", placeholder = "seed"
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
			    ),
			    textInput(
			        ns("queryName"), "Query ID",
			        value = "query", placeholder = "query"
			    )
			),

			shinyDirButton("dir", "Input directory", "Upload"),
			# uiOutput("fasJob.ui"),
			textInput(ns("fasJob"), "Job ID", value = randFn(1)),
			bsButton(ns("newFasJob.btn"), "New job ID")
		),
		# * main panel for FAS run ----------------------------
		mainPanel(
			width = 9,
			column(
				6,
				uiOutput(ns("fasBtn.ui")),
				hr(),
				strong("SELECTED OPTIONS"),
				br(), br(),
				strong("annoFAS"),
				br(), br(),
				uiOutput(ns("annoOptions.ui")),
				br(), br(),
				strong("greedyFAS"),
				br(), br(),
				uiOutput(ns("greedyOptions.ui")),
				hr(),
				strong("Log file"),
				verbatimTextOutput(ns("logLocation")),
				strong("Output files"),
				verbatimTextOutput(ns("outputLocation"))
			),
			column(
				6,
				verbatimTextOutput(ns("fasCmdText")),
				htmlOutput(ns("fasLog"))
			)
		)
	)
}

fasApp <- function(input, output, session) {
	volumes = getVolumes() # for shinyFileChoose
	ns <- session$ns

	# get fas location =========================================================
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
			shinyFileChoose(input, "greedyFasFile", roots = volumes, session = session)
			req(input$greedyFasFile)
			if(!is.null(input$greedyFasFile)){
				file_selected <- parseFilePaths(volumes, input$greedyFasFile)
				return(as.character(file_selected$datapath))
			}
		}
	})
	
	getAnnoPath <- reactive({
	    fasLocation <- suppressWarnings(
	        system("which annoFAS", intern = TRUE)
	    )
	    if (!is.na (fasLocation[1])){
	        return(fasLocation[1])
	    } else {
	        # shinyFileChoose(input, "annoFasFile", roots = volumes, session = session)
	        # req(input$annoFasFile)
	        # if(!is.null(input$annoFasFile)){
	        #     file_selected <- parseFilePaths(volumes, input$annoFasFile)
	        #     return(as.character(file_selected$datapath))
	        # }
	    }
	})

	output$fasLocation <- renderText({
		fasPath <- getFasPath()
		paste(
			"FAS found at", fasPath
		)
	})

	# get input fasta ==========================================================
	getSeedPath <- reactive({
		shinyFileChoose(
		    input, "seedInput", roots = volumes, session = session,
		    filetypes = c('', 'fa', 'fasta')
		)
		file_selected <- parseFilePaths(volumes, input$seedInput)
		req(input$seedInput)
		return(as.character(file_selected$datapath))
	})

	getQueryPath <- reactive({
	    shinyFileChoose(
	        input, "queryInput", roots = volumes, session = session,
	        filetypes = c('', 'fa', 'fasta')
	    )
	    file_selected <- parseFilePaths(volumes, input$queryInput)
	    req(input$queryInput)
	    return(as.character(file_selected$datapath))
	})

	# get list of sequence IDs =================================================
	getSeqID <- function(file = NULL){
		if (is.null(file)) stop("Input fasta file not provided!")
		faFile <- Biostrings::readAAStringSet(file)
		return(names(faFile))
	}

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

	# get list of available refspec ============================================
	# getRefspecList <- function(oneseqPath = NULL){
	# 	if (is.null(oneseqPath)) stop("HaMStR not found!")
	# 	if (length(grep("oneSeq.pl", oneseqPath))) {
	# 		blastDir <- stringr::str_replace(
	# 			oneseqPath, "/bin/oneSeq.pl", "/blast_dir"
	# 		)
	# 	} else {
	# 		blastDir <- stringr::str_replace(
	# 			oneseqPath, "/bin/oneSeq", "/blast_dir"
	# 		)
	# 	}
	# 	refspecPath <- list.dirs(
	# 		path = blastDir, full.names = TRUE, recursive = FALSE
	# 	)
	# 	refspecList <- stringr::str_replace(
	# 		refspecPath, paste0(blastDir,"/"), ""
	# 	)
	# 	return(refspecList)
	# }
	#
	# output$refSpec.ui <- renderUI({
	# 	refspecList <- c("undefined")
	# 	refspecList <- getRefspecList(getOneseqPath())
	# 	selectInput(
	# 		ns("refSpec"), "Reference species",
	# 		choices = c("undefined", refspecList),
	# 		selected = "undefined"
	# 	)
	# })


	# getOutputPath <- reactive({
	#     shinyDirChoose(
	#         input, 'dir', roots=c(wd='.') #roots = volumes
	#     )
	#     outputPath <- parseDirPath(volumes, input$dir)
	#     # outputPath <-
	#     #     file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
	#     # req(input$queryInput)
	#     return(as.character(outputPath))
	# })

	# annoFAS options =========================================================
	annoOptions <- reactive({

		# fasta <- paste0("--fasta=", getSeedPath()) #paste0("-seqFile=", getInputPath())
		fasta <- paste0("--fasta=/Users/bemun/Desktop/bionf/HaMStR/data/infile.fa")
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

	# RUN FAS ===============================================================
	# randFn <- function(n = 5000) {
	# 	a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
	# 	paste0(
	# 		a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)
	# 	)
	# }

	jobID <- reactiveValues()

	# output$fasJob.ui <- renderUI({
	# 	jobID <- randFn(1)
	# 	h3("BLABLABLA")
	# 	# textInput(ns("fasJob"), "Job ID", value = jobID)
	# })

	observeEvent(input$newFasJob.btn, {
		jobID <- randFn(1)
		updateTextInput(session, "fasJob", "Job ID", value = jobID)
	})

	output$fasBtn.ui <- renderUI({
		# if (
		# 	!is.null(input$refSpec) && !is.null(input$seqID) && input$seqID !=""
		# ) {
		# 	if (input$refSpec != "undefined") {
				tagList(
					bsButton(
						ns("doFAS"), "Run FAS",
						style = "warning", disabled = FALSE
					),
					actionButton(ns("stopFAS"),label = "Stop"),
					actionButton(ns("newFAS"),label = "New job"),
					textOutput(ns("fasLocation"))
				)
		# 	}
		# }
	})

	observeEvent(input$newFAS, {
		updateButton(session, ns("doFAS"), disabled = FALSE)
		updateButton(session, ns("stopFAS"), disabled = FALSE)
	})

	fasCmd <- reactive({
		return(
			paste(
				# getFasPath(),
				# "annoFAS", #
				getAnnoPath(),
				paste(annoOptions(), collapse = " ")
				# paste(optOptions(), collapse = " ")
			)
		)
	})

	output$fasCmdText <- renderText({
		# paste("python", fasCmd())
	    paste(fasCmd())
	})

	rv <- reactiveValues(
		textstream = c(""),
		timer = reactiveTimer(1000),
		started = FALSE
	)
	observeEvent(input$doFAS, {
		rv$started <- TRUE
		cmd <- paste(
			fasCmd(),
			">>",
			paste0(input$fasJob, ".log")
		)
		# system2("python", cmd, wait = FALSE)
		system(cmd)#, wait = FALSE)
		updateButton(session, ns("doFAS"), disabled = TRUE)
	})

	observeEvent(input$stopFAS, {
		rv$started <- FALSE
		system2("rm", "*.log")
		updateButton(session, ns("stopFAS"), disabled = TRUE)
	})

	# observe({
	# 	rv$timer()
	# 	if (isolate(rv$started)) {
	# 		rv$textstream <- suppressWarnings(
	# 		  paste(readLines(paste0(input$fasJob, ".log")), collapse = "<br/>")
	# 		)
	# 	}
	# })
	# output$hamstrLog <- renderUI({
	# 	HTML(rv$textstream)
	# })

	# report results ===========================================================
	# output$logLocation <- renderText({
	#     paste0(getwd(), "/", input$seqName, ".log")
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
	#     faOut <- paste0(dataDir, "/", input$seqName, ".extended.fa")
	#     if (input$useFAS == TRUE) {
	#         ppOut <- paste0(dataDir, "/", input$seqName, ".phyloprofile")
	#         domainFwOut <- paste0(dataDir, "/", input$seqName, "_1.domains")
	#         domainRvOut <- paste0(
	#             "[", dataDir, "/", input$seqName, "_1.domains", "]"
	#         )
	#         paste(faOut, ppOut, domainFwOut, domainRvOut, sep = "\n")
	#     } else {
	#         faOut
	#     }
	# })
}