#' annoFAS module

annoFasAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for FAS input/options -----------------
        sidebarPanel(
            width = 3,
            conditionalPanel(
                condition = "output.checkAnnoStatus == 0", ns = ns,
                h2(em("annoFAS not found! Please install it first!")),
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
            uiOutput(ns("annoInput.ui")),
            br(), 
            
            # ** required options ==================================
            strong("Required options"),
            
            textInput(ns("annoJob"), strong("Job ID"), value = randFn(1)),
            bsPopover(
                ns("annoJob"),
                "",
                paste(
                    "Name of job and log file(s)."
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
                br(), br(),
                
                shinyDirButton(
                    ns("optAnnoTool"), "Opt. Path to annotation tools" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("optAnnoTool"),
                    "",
                    paste(
                        "Provide folder to annotation tools"
                    ),
                    "top"
                ),
                br(),
                uiOutput(ns("optAnnoTool.ui")),
                br(),
                
                uiOutput(ns("outName.ui")),
                bsPopover(
                    ns("outName.ui"),
                    "",
                    paste(
                        "Name of annotation output file"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("annoCPU"),
                    strong("Number of CPUs for annotation"),
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
                    ns("force"), strong("Force override annotations"),
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
                    strong("Redo annotation with"),
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
                    shinyFilesButton(
                        ns("existingAnno"), "Input existing annotation file!" ,
                        title = "Please provide JSON file for existing annotation:",
                        multiple = FALSE,
                        buttonType = "default", class = NULL
                    )
                ),
                uiOutput(ns("existingAnno.ui")),
                hr(),
                
                checkboxInput(
                    ns("toolOption"),
                    strong("Specific tool options"),
                    value = FALSE,
                    width = NULL
                ),
                
                conditionalPanel(
                    condition = "input.toolOption", ns = ns,
                    numericInput(
                        ns("eFeature"),
                        strong("eValue cutoff for PFAM/SMART domain (10^x)"),
                        value = -3,
                        min = -99,
                        max = 0,
                        step = 1
                    ),
                    
                    numericInput(
                        ns("eInstance"),
                        strong("eValue cutoff for PFAM/SMART instance (10^x)"),
                        value = -2,
                        min = -99,
                        max = 0,
                        step = 1
                    ),
                    
                    numericInput(
                        ns("eFlps"),
                        strong("eValue cutoff for fLPS (10^x)"),
                        value = -7,
                        min = -99,
                        max = 0,
                        step = 1
                    ),
                    
                    selectInput(
                        ns("org"),
                        strong("Organism of input for SignalP"),
                        choices = list(
                            "Eukaryote" = "euk", "Bacteria gram(+)" = "gram+",
                            "Bacteria gram(-)" = "gram-"
                        ),
                        selected = "euk"
                    )
                )
            )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "output.checkRunAnno", ns = ns,
                bsButton(
                    ns("doAnno"), "Run annoFAS",
                    style = "success", disabled = FALSE
                ),
                actionButton(ns("stopAnno"),label = "Stop"),
                actionButton(ns("newAnno"),label = "New job"),
                textOutput(ns("annoLocation")),
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
                strong("Output file"),
                textOutput(ns("outputAnnoFile")),
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
    
    # get input fasta =========================================
    getAnnoInput <- reactive({
        shinyFileChoose(
            input, "annoInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$annoInput)
        req(input$annoInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$annoInput.ui <- renderUI({
        req(getAnnoInput())
        if (length(getAnnoInput()) > 0) {
            outString <- getAnnoInput()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # # get reference annotation file ==========================================
    getExistingAnno <- reactive({
        shinyFileChoose(
            input, "existingAnno", roots = homePath, session = session,
            filetypes = c('', 'json')
        )
        fileSelected <- parseFilePaths(homePath, input$existingAnno)
        # req(input$existingAnno)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$existingAnno.ui <- renderUI({
        req(getExistingAnno())
        if (length(getExistingAnno()) > 0) {
            outString <- getExistingAnno()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outAnnoDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outAnnoDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
    # get output name ==========================================================
    output$outName.ui <- renderUI({
        req(getAnnoInput())
        inFile <- str_split(getAnnoInput(), '/')
        inFileTmp <- str_split(tail(inFile[[1]], 1), "\\.")
        textInput(ns("outName"), strong("Output name"), value = inFileTmp[[1]][1])
    })
    
    # generate new job ID ======================================================
    observeEvent(input$newAnnoJob.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "annoJob", strong("Job ID"), value = jobID)
    })
    
    # get optional annotation tool path ========================================
    getOptAnnoTool <- reactive({
        shinyDirChoose(
            input, "optAnnoTool", roots = homePath, session = session
        )
        optAnnoPath <- parseDirPath(homePath, input$optAnnoTool)
        return(replaceHomeCharacter(as.character(optAnnoPath)))
    })
    output$optAnnoTool.ui <- renderUI({
        req(getOptAnnoTool())
        if (length(getOptAnnoTool()) > 0) {
            outString <- getOptAnnoTool()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # annoFAS options ==========================================================
    annoOptions <- reactive({
        fasta <- paste0("--fasta ", getAnnoInput())
        path <- ""
        if (length(getOutputPath()) > 0) 
            path <- paste0("--outPath ", getOutputPath())
        annoOption <- c(fasta, path)
        
        if (input$extract == TRUE) {
            extract <- ""
            req(getExistingAnno())
            if (length(getAnnoInput()) > 0) {
                # existing anno file
                annoFile <- paste0("--annoFile ", getExistingAnno())
                # ID of sequence need to get annotation
                name <- paste0("--name=", input$annoID)
                extract <- "--extract"
                annoOption <- c(fasta, path, annoFile, extract)
            }
        }
        
        if (input$optAnnoOption == TRUE) {
            name <- ""
            if (input$outName != "") name <- paste0("--name ", input$outName)
            redo <- ""
            if (input$redo != "all") redo <- paste0("--redo ", input$redo)
            force <- ""
            if (input$force == TRUE) force <- paste0("--force")
            cores <- ""
            if (input$annoCPU > 1) cores <- paste0("--cpus ", input$annoCPU)
            toolPath <- ""
            if (length(getOptAnnoTool()) > 0)
                toolPath <- paste0("--toolPath ", getOptAnnoTool())
            annoOption <- c(annoOption, name, redo, force, cores, toolPath)
        }
        
        if (input$toolOption == TRUE) {
            efeature <- ""
            if (input$eFeature != "-3")
                efeature <- paste0("--eFeature ", 10^input$eFeature)
            einstance <- ""
            if (input$eInstance != "-2")
                einstance <- paste0("--eInstance ", 10^input$eInstance)
            eflps <- ""
            if (input$eFlps != "-7")
                eflps <- paste0("--eFlps ", 10^input$eFlps)
            org <- ""
            if (input$org != "euk")
                org <- paste0("--org ", input$org)
            annoOption <- c(annoOption, efeature, einstance, eflps, org)
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
        if (length(getAnnoInput()) == 0 || length(getOutputPath()) == 0)
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
        req(getAnnoInput())
        seqIDs <- getSeqID(getAnnoInput())
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
    
    output$outputAnnoFile <- renderText({
        req(getOutputPath())
        annoOutPath <- getOutputPath()
        outName <- input$outName
        outFiles <- paste0(annoOutPath, "/", outName, ".json")
        # 
        # if (input$extract == TRUE) {
        #     outFiles <- paste0(
        #         getOutputPath(), "/", input$jobName, "_", input$annoID, "/*.xml"
        #     )
        # }
        return(outFiles)
    })
}
