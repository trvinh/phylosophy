#' FAS module

annoFasAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for FAS input/options -----------------
        sidebarPanel(
            width = 3,
            conditionalPanel(
                condition = "output.checkAnnoStatus == 0", ns = ns,
                h2(em("greedFAS not found! Please install it first!")),
                bsButton(
                    "installFas", "Install FAS",
                    onclick = "window.open('https://bionf.github.io/FAS/#installation', '_blank')"
                ),
                hr()
            ),
            
            h3("Input and configurations"),
            hr(),
            
            shinyFilesButton(
                ns("annoInput"), "Input fasta file!" ,
                title = "Please provide fasta file for annotation job:",
                multiple = FALSE,
                buttonType = "default", class = NULL
            ),
            br(), br(),
            
            # ** required options ==================================
            strong("Required options"),
            
            textInput(ns("annoJob"), "Job ID", value = randFn(1)),
            bsPopover(
                ns("annoJob"),
                "",
                paste(
                    "Name of job and log file(s). This will also be the name",
                    "of annotation output folder."
                ),
                "bottom"
            ),
            bsButton(ns("newAnnoJob.btn"), "New job ID"),
            br(), br(),
            
            shinyDirButton(
                ns("outAnnoDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            bsPopover(
                ns("outAnnoDir"),
                "",
                paste(
                    "Provide folder for output annotation"
                ),
                "top"
            ),
            hr(),
            
            # ** optional options ==================================
            checkboxInput(
                ns("optAnnoOption"),
                strong("Other options"),
                value = FALSE,
                width = NULL
            ),
            
            conditionalPanel(
                condition = "input.optAnnoOption", ns = ns,
                strong("Additional options"),
                br(),
                
                numericInput(
                    ns("annoCPU"),
                    "Number of CPUs for annotation",
                    value = 4,
                    min = 1,
                    max = 99,
                    step = 1
                ),
                bsPopover(
                    ns("annoCPU"),
                    "",
                    paste(
                        "Determine the number of CPUs used for doing annotation"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("force"), "Force override annotations",
                    value = FALSE
                ),
                bsPopover(
                    ns("force"),
                    "",
                    paste(
                        "Force override annotations"
                    ),
                    "bottom"
                ),
                
                selectInput(
                    ns("redo"), 
                    "Redo annotation with",
                    choices = c(
                        "all", "pfam", "smart", "flps", "coils", "seg", "tmhmm",
                        "signalp"
                    ),
                    selected = "all"
                ),
                bsPopover(
                    ns("redo"),
                    "",
                    paste(
                        "Database(s)/Tool(s) used for re-annotation"
                    ),
                    "top"
                ),
                
                checkboxInput(
                    ns("extract"), strong("Extract existing annotations"),
                    value = FALSE
                ),
                bsPopover(
                    ns("extract"),
                    "",
                    paste(
                        "Extract annotation for input sequence"
                    ),
                    "bottom"
                ),
                
                conditionalPanel(
                    condition = "input.extract", ns = ns,
                    shinyDirButton(
                        ns("refAnnoDir"), "Existing annotation folder",
                        title = "Please select a folder",
                        buttonType = "default", class = NULL
                    ),
                    bsPopover(
                        ns("refAnnoDir"),
                        "",
                        paste(
                            "Provide folder for existing annotations"
                        ),
                        "top"
                    ),
                    
                    uiOutput(ns("refSpecAnno.ui")),
                    bsPopover(
                        ns("refSpecAnno"),
                        "",
                        paste(
                            "Select taxon of input sequences for extracting",
                            "annotation"
                        ),
                        "top"
                    ),

                    uiOutput(ns("annoSeqs.ui")),
                    bsPopover(
                        ns("annoID"),
                        "",
                        paste(
                            "Specify the sequence identifier of the input",
                            "sequence in the reference protein set.",
                            "If not provided, the program will attempt to",
                            "determine it automatically."
                        ),
                        "bottom"
                    )
                )
            )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "output.checkRunAnno", ns = ns,
                # uiOutput(ns("annoBtn.ui")),
                bsButton(
                    ns("doAnno"), "Run annoFAS",
                    style = "success", disabled = FALSE
                ),
                actionButton(ns("stopAnno"),label = "Stop"),
                actionButton(ns("newAnno"),label = "New job"),
                textOutput(ns("annoLocation")),
                textOutput(ns("annoToolLocation")),
                hr(),
                checkboxInput(
                    ns("showOpts"),
                    strong("Show selected options"),
                    value = FALSE,
                    width = NULL
                ),
                conditionalPanel(
                    condition = "input.showOpts", ns = ns,
                    strong("annoFAS OPTIONS"),
                    br(), br(),
                    uiOutput(ns("annoOptions.ui")),
                    strong("Command"),
                    verbatimTextOutput(ns("annoCmdText")),
                    hr()
                ),
                strong("Log file"),
                textOutput(ns("logAnnoLocation")),
                strong("Output files"),
                textOutput(ns("outputAnnoLocation")),
                hr(),
                strong("Progress"),
                verbatimTextOutput(ns("annoLog")),
                hr(),
                strong("Select sene for plotting"),
                uiOutput(ns("annoIDplot.ui")),
                uiOutput(ns("annoPlot.ui"))
            )
        )
    )
}

annoFasApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # check annoFAS installed ==================================================
    output$checkAnnoStatus <- reactive({
        fasLocation <- suppressWarnings(
            system("which annoFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkAnnoStatus", suspendWhenHidden = FALSE)
    
    # get annoFAS location =====================================================
    getAnnoFasPath <- reactive({
        fasLocation <- suppressWarnings(
            system("which annoFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(fasLocation[1])
        } else {
            return(NULL)
        }
    })
    
    output$annoLocation <- renderText({
        annoPath <- getAnnoFasPath()
        if (!is.null(annoPath)) paste("annoFAS found at", annoPath)
    })
    
    # get annoToolFAS location =================================================
    getToolsPath <- reactive({
        req(getAnnoFasPath())
        testFaFile <- system.file(
            "extdata", "seed.fa", package = "phylosophy", mustWork = TRUE
        )
        if (!is.null(getAnnoFasPath())) {
            toolsPath <- suppressWarnings(
                system2(
                    "annoFAS",
                    paste(
                        "-i", testFaFile,
                        "-o", "tmp",
                        "-n", "getToolsPath",
                        "--getAnnoPath"
                    ),
                    stdout = TRUE,
                    stderr = TRUE
                )
            )
            return(toolsPath[1])
        } else {
            return(NULL)
        }
    })

    output$annoToolLocation <- renderText({
        paste(getToolsPath())
    })
    
    # get input fasta =========================================
    getPathAnnoInput <- reactive({
        shinyFileChoose(
            input, "annoInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$annoInput)
        req(input$annoInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    # get list of sequence IDs ==================================
    output$annoSeqs.ui <- renderUI({
        seqIDs <- getSeqID(getPathAnnoInput())
        tagList(
            selectInput(
                ns("annoID"), "Gene ID",
                choices = c("all", seqIDs),
                selected = seqIDs[1]
            ),
            bsPopover(
                ns("annoID"), "",
                paste("Sequence ID(s) for extracting annotations"), "top"
            )
        )
    })
    
    # get list of reference annotations ========================================
    getRefDir <- reactive({
        shinyDirChoose(
            input, "refAnnoDir", roots = homePath, session = session
        )
        refPath <- parseDirPath(homePath, input$refAnnoDir)
        return(replaceHomeCharacter(refPath))
    })
    
    getRefAnno <- reactive({
        refPath <- getRefDir()
        refPathSub <- list.dirs(
            path = refPath, full.names = TRUE, recursive = FALSE
        )
        refList <- stringr::str_replace(refPathSub, paste0(refPath, "/"), "")
        return(refList)
    })
    
    output$refSpecAnno.ui <- renderUI({
        selectInput(
            ns("refSpecAnno"), "Reference annotation",
            choices = c("undefined", getRefAnno()),
            selected = "undefined"
        )
    })
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outAnnoDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outAnnoDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
    # generate new job ID ======================================================
    observeEvent(input$newAnnoJob.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "annoJob", strong("Job ID"), value = jobID)
    })
    
    # annoFAS options ==========================================================
    annoOptions <- reactive({
        fasta <- paste0("--fasta=", getPathAnnoInput())
        path <- ""
        if (length(getOutputPath()) > 0) 
            path <- paste0("--path=", getOutputPath())
        name <- ""
        if (input$annoJob != "") name <- paste0("--name=", input$annoJob)
        
        redo <- ""
        if (input$redo != "all") redo <- paste0("--redo=", input$redo)
        force <- ""
        if (input$force == TRUE) force <- paste0("--force")
        cores <- ""
        if (input$annoCPU > 1) cores <- paste0("--cores=", input$annoCPU)
        
        annoOption <- c(fasta, path, name, redo, force, cores)
        
        extract <- ""
        if (input$extract == TRUE) {
            req(getRefDir())
            # name of existing annotation folder
            path <- paste0(
                "--path=", 
                getRefDir(), "/", input$refSpecAnno
            )
            # ID of sequence need to get annotation
            name <- paste0("--name=", input$annoID)
            # extract is output dir
            extract <- paste0(
                "--extract=", 
                getOutputPath(), "/", input$annoJob, "_", input$annoID
            )
            annoOption <- c(fasta, path, name, extract)
        }
        
        return(
            annoOption[unlist(lapply(annoOption, function (x) x != ""))]
        )
    })
    
    output$annoOptions.ui <- renderUI({
        HTML(paste(annoOptions(), collapse = "<br/>"))
    })
    
    # RUN annFAS ===============================================================
    output$checkRunAnno <- reactive({
        if (length(getPathAnnoInput()) == 0 || length(getOutputPath()) == 0)
            return(FALSE)
        return(TRUE)
    })
    outputOptions(output, "checkRunAnno", suspendWhenHidden = FALSE)
    
    observeEvent(input$newAnno, {
        updateButton(session, ns("doAnno"), disabled = FALSE)
        updateButton(session, ns("stopAnno"), disabled = FALSE)
    })
    
    annoCmd <- reactive({
        return(
            paste(
                getAnnoFasPath(),
                paste(annoOptions(), collapse = " ")
            )
        )
    })
    
    output$annoCmdText <- renderText({
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
            paste0(input$annoJob, ".anno.log")
        )
        
        system(cmd, wait = FALSE)
        updateButton(session, ns("doAnno"), disabled = TRUE)
        updateButton(session, ns("newAnnoJob.btn"), disabled = TRUE)
    })
    
    observeEvent(input$stopAnno, {
        rvAnno$started <- FALSE
        system2("rm", "*.anno.log")
        updateButton(session, ns("stopAnno"), disabled = TRUE)
    })
    
    observe({
        rvAnno$timer()
        if (isolate(rvAnno$started)) {
            if (file.exists(paste0(input$annoJob, ".anno.log"))) {
                rvAnno$textstream <- suppressWarnings(
                    readLines(paste0(input$annoJob, ".anno.log"),  n = -1) %>% 
                        tail(50) %>% paste(collapse = "\n")
                )
            }
        }
    })
    output$annoLog <- renderText({
        rvAnno$textstream
    })
    
    # render domain plot =======================================================
    output$annoIDplot.ui <- renderUI({
        req(getPathAnnoInput())
        seqIDs <- getSeqID(getPathAnnoInput())
        flag <- 0
        if (input$extract == TRUE && length(input$annoID) > 0) {
            if (!(input$annoID == "all")) {
                flag <- 1
            }
        }
        if (flag == 0) {
            selectInput(
                ns("annoIDplot"), "Gene ID",
                choices = seqIDs,
                selected = seqIDs[1]
            )
        } else {
            selectInput(
                ns("annoIDplot"), "Gene ID",
                choices = input$annoID,
                selected = input$annoID
            )
        }
    })
    
    getDomainInformation <- reactive({
        # req(input$doFAS)
        # req(input$doPlot)
        inputDomain <- paste0(
            getOutputPath(), "/", input$fasJob, "_forward.domains"
        )
        withProgress(message = 'Reading domain input...', value = 0.5, {
            domainDf <- parseDomainInput(
                NULL,
                inputDomain,
                "file"
            )
            return(domainDf)
        })
    })

    output$archiPlot <- renderPlot({
        if (input$doPlot > 0) {
            annoID <- input$annoIDplot
            annoID <- gsub("\\|", ":", annoID)
            orthoID <- input$annoIDplot #input$queryIDplot
            orthoID <- gsub("\\|", ":", orthoID)
            info <- c(annoID, orthoID)
            g <- createArchiPlot(
                info, getDomainInformation(), 12, 12
            )
            if (any(g == "No domain info available!")) {
                msgPlot()
            } else {
                grid::grid.draw(g)
            }
        }
    })

    output$archiPlotFas.ui <- renderUI({
        ns <- session$ns
        plotOutput(
            ns("archiPlot"),
            height = 400,
            width = 800
        )
    })
    
    # report results ===========================================================
    output$logAnnoLocation <- renderText({
        paste0(getwd(), "/", input$annoJob, ".anno.log")
    })
    
    output$outputAnnoLocation <- renderText({
        req(getOutputPath())
        annoOutPath <- getOutputPath()
        jobName <- input$annoJob
        outFiles <- paste0(annoOutPath, "/", jobName, "/*.xml")
        
        if (input$extract == TRUE) {
            outFiles <- paste0(
                getOutputPath(), "/", input$jobName, "_", input$annoID, "/*.xml"
            )
        }
        return(outFiles)
    })
}
