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
                h2(em("calcFAS not found! Please install FAS first!")),
                bsButton(
                    "installFas", "Install FAS",
                    onclick = "window.open('https://bionf.github.io/FAS/#installation', '_blank')"
                ),
                hr()
            ),
            
            # ** fasta input =======================================
            h3("Input and configurations"),
            # em("(*) required options"),
            hr(),
            shinyFilesButton(
                ns("seedInput"), "Input seed" ,
                title = "Please provide fasta file for seed:",
                multiple = FALSE,
                buttonType = "default", class = NULL
            ),
            uiOutput(ns("seedInput.ui")),
            br(),
            shinyFilesButton(
                ns("queryInput"), "Input query" ,
                title = "Please provide fasta file for query:",
                multiple = FALSE,
                buttonType = "default", class = NULL
            ),
            uiOutput(ns("queryInput.ui")),
            br(),
            
            # ** required options ==============================================
            strong("Required options"),
            br(),br(),
            shinyDirButton(
                ns("annoDir"), "Annotation directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            bsPopover(
                ns("annoDir"),
                "",
                paste(
                    "Provide folder for [existing|saving] annotations"
                ),
                "top"
            ),
            uiOutput(ns("annoDir.ui")),
            br(),
            
            shinyDirButton(
                ns("outFasDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            bsPopover(
                ns("outFasDir"),
                "",
                paste(
                    "Provide folder for FAS output"
                ),
                "top"
            ),
            uiOutput(ns("outFasDir.ui")),
            br(),
            
            uiOutput(ns("outName.ui")),
            bsPopover(
                ns("outName.ui"),
                "",
                paste(
                    "Name of output file"
                ),
                "bottom"
            ),
            
            textInput(ns("fasJob"), strong("Job ID"), value = randFn(1)),
            bsPopover(
                ns("fasJob"),
                "",
                paste("Name of job and log file(s)"),
                "bottom"
            ),
            bsButton(ns("newFasJob.btn"), "New job ID"),
            hr(),
            
            # ** optional options ==============================================
            checkboxInput(
                ns("optFasOption"),
                strong("Other options"),
                value = FALSE,
                width = NULL
            ),
            
            conditionalPanel(
                condition = "input.optFasOption", ns = ns,
                # *** general options ==========================================
                strong("General options"),
                br(),
                checkboxInput(
                    ns("bidirectional"), strong("Bi-directional FAS"),
                    value = TRUE
                ),
                bsPopover(
                    ns("bidirectional"),
                    "",
                    paste(
                        "calculate both scoring directions (separate files),",
                        "creates csv file with combined scores"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("cpus"), 
                    strong("Number of CPU cores for multiprocessing"),
                    min = 1, max = 999, step = 1, value = 1
                ),
                hr(),
                
                # *** I/O options ==============================================
                strong("Input/output options"),
                br(),
                
                uiOutput(ns("seedID.ui")),
                # ),
                bsPopover(
                    ns("seedID.ui"),
                    "",
                    paste(
                        "Choose specific proteins from the seed input for",
                        "calculation."
                    ),
                    "bottom"
                ),
                
                uiOutput(ns("queryID.ui")),
                bsPopover(
                    ns("queryID.ui"),
                    "",
                    paste(
                        "Choose specific proteins from the query input for",
                        "calculation."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("noArch"), strong("Not output domain XML file"),
                    value = FALSE
                ),
                
                checkboxInput(
                    ns("noDomain"), strong("Not output domain tabular output"),
                    value = FALSE
                ),
                
                checkboxInput(
                    ns("outputPhyloprofile"), strong("PhyloProfile output"),
                    value = FALSE
                ),
                conditionalPanel(
                    condition = "input.outputPhyloprofile", ns = ns,
                    shinyFilesButton(
                        ns("phyloprofile"), "Mapping file for PhyloProfile" ,
                        title = "Please provide mapping file for PhyloProfile",
                        multiple = FALSE,
                        buttonType = "default", class = NULL
                    ),
                    uiOutput(ns("phyloprofile.ui")),
                    br()
                ),
                bsPopover(
                    ns("outputPhyloprofile"),
                    "",
                    paste(
                        "Activate phyloprofile output, needs mapping file",
                        "for all query proteins, single seed only, will",
                        "run with more but output won't work without",
                        "editing"
                    ),
                    "bottom"
                ),
                br(),
                
                shinyFilesButton(
                    ns("featureTypes"), "File contains the annotation tools" ,
                    title = "Please provide file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("featureTypes"),
                    "",
                    paste(
                        "Input file that contains the tools/databases used to",
                        "predict features with annoFAS"
                    ),
                    "bottom"
                ),
                uiOutput(ns("featureTypes.ui")),
                br(),
                
                
                hr(),
                
                # *** annotation options =======================================
                strong("Annotation options"),
                br(),
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
                ),
                hr(),
                
                # *** weighting options ========================================
                strong("Weighting options"),
                br(), br(),
                shinyFilesButton(
                    ns("refFwd"), "Reference proteome for forward FAS" ,
                    title = "Please provide proteome file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("refFwd.ui")),
                br(),
                
                shinyFilesButton(
                    ns("refRev"), "Reference proteome for reverse FAS" ,
                    title = "Please provide proteome file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("refRev.ui")),
                br(),
                
                selectInput(
                    ns("weightCorrection"),
                    strong("Type of weight correction"),
                    choices = list(
                        "No correction" = "linear",
                        "Natural logarithm" = "loge",
                        "base-10 logarithm" = "log10",
                        "4th root" = "root4",
                        "8th root" = "root8"
                    ),
                    selected = "loge"
                ),
                bsPopover(
                    ns("weightCorrection"),
                    "",
                    paste(
                        "Function applied to the frequency of feature types",
                        "during weighting"
                    ),
                    "top"
                ),
                br(),
                
                shinyFilesButton(
                    ns("weightConstraints"), "Weight constraint file" ,
                    title = "Please provide file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("weightConstraints"),
                    "",
                    paste(
                        "Apply weight constraints via constraints file, by",
                        "default there are no constraints."
                    ),
                    "bottom"
                ),
                uiOutput(ns("weightConstraints.ui")),
                br(),
                hr(),
                
                # *** threshold options ========================================
                strong("Threshold options"),
                numericInput(
                    ns("maxOverlap"), 
                    strong("Maximum overlape (number of amino acid)"),
                    min = 0, max = 99999, step = 1, value = 0
                ),
                bsPopover(
                    ns("maxOverlap"),
                    "",
                    paste(
                        "Maximum size overlap allowed, default is 0 amino",
                        "acids"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("maxOverlapPercentage"), 
                    strong("Maximum overlape in percentage"),
                    min = 0, max = 1, step = 0.01, value = 0.4
                ),
                bsPopover(
                    ns("maxOverlapPercentage"),
                    "",
                    paste(
                        "Maximum percent of a feature the overlap is",
                        "allowed to cover, default is 0.4 (40%)"
                    ),
                    "bottom"
                ),
                numericInput(
                    ns("priorityThreshold"),
                    strong("Threshold for priority mode"),
                    min = 0, max = 999, step = 1, value = 50
                ),
                
                bsPopover(
                    ns("priorityThreshold"),
                    "",
                    paste(
                        "Change to define the feature number threshold for",
                        "activating priority mode in the path evaluation"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("maxCardinality"), 
                    strong("Threshold for maximal cardinality"),
                    min = 0, max = 99999999, step = 1, value = 5000
                ),
                bsPopover(
                    ns("maxCardinality"),
                    "",
                    paste(
                        "Change to define the threshold for the maximal",
                        "cardinality of feature paths in a graph. If max.",
                        "cardinality is exceeded the priority mode will be",
                        "used to for the path evaluation."
                    ),
                    "bottom"
                )
            )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            uiOutput(ns("fasBtn.ui")),
            hr(),
            checkboxInput(
                ns("showOpts"),
                strong("Show selected options"),
                value = FALSE,
                width = NULL
            ),
            conditionalPanel(
                condition = "input.showOpts", ns = ns,
                strong("greedyFAS OPTIONS"),
                br(),
                uiOutput(ns("fasOptions.ui")),
                strong("Command"),
                verbatimTextOutput(ns("fasCmdText")),
                hr()
            ),
            
            strong("Log file"),
            verbatimTextOutput(ns("logFasLocation")),
            strong("Output files"),
            verbatimTextOutput(ns("outputFasLocation")),
            hr(),

            strong("Progress"),
            verbatimTextOutput(ns("fasLog")),
            hr(),
            bsButton(
                ns("doPlot"), "Plot feature architecture", 
                style = "info", disabled = TRUE
            ),
            
            # * popup for domain plot ------------------------------------------
            bsModal(
                "domainPlot",
                "Protein feature architecture plot",
                ns("doPlot"),
                size = "large",
                fluidRow(
                    column(
                        2,
                        numericInput(
                            ns("archiHeight"),
                            "Plot height(px)",
                            min = 100,
                            max = 3200,
                            step = 50,
                            value = 400,
                            width = 100
                        ),
                        numericInput(
                            ns("archiWidth"),
                            "Plot width(px)",
                            min = 100,
                            max = 3200,
                            step = 50,
                            value = 800,
                            width = 100
                        )
                    ),
                    column(
                        4,
                        uiOutput(ns("seedIDplot.ui")),
                        uiOutput(ns("queryIDplot.ui"))
                    ),
                    column(
                        4,
                        selectInput(
                            ns("direction"),
                            "FAS direction",
                            choices = list("Forward" = "fwd", "Reverse" = "rev"),
                            selected = "fwd"
                        ),
                        verbatimTextOutput(ns("fasScore"))
                    )
                ),
                uiOutput(ns("archiPlotFas.ui")),
                br(),br(),
                downloadButton(ns("archiDownload"), "Download plot")
            )
        )
    )
}

fasApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # check calcFAS installed ==================================================
    output$checkFasStatus <- reactive({
        fasLocation <- suppressWarnings(
            system("which calcFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkFasStatus", suspendWhenHidden = FALSE)
    
    # get calcFAS location =====================================================
    getFasPath <- reactive({
        fasLocation <- suppressWarnings(
            system("which calcFAS", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(fasLocation[1])
        } else {
            return(NULL)
        }
    })
    
    output$fasLocation <- renderText({
        fasPath <- getFasPath()
        if (!is.null(fasPath)) paste("calcFAS found at", fasPath)
    })

    # get input fasta (seed and query) =========================================
    getSeedInput <- reactive({
        shinyFileChoose(
            input, "seedInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$seedInput)
        req(input$seedInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$seedInput.ui <- renderUI({
        req(getSeedInput())
        if (length(getSeedInput()) > 0) {
            em(getSeedInput())
        }
    })
    
    getQueryInput <- reactive({
        shinyFileChoose(
            input, "queryInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$queryInput)
        req(input$queryInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$queryInput.ui <- renderUI({
        req(getQueryInput())
        if (length(getQueryInput()) > 0) {
            em(getQueryInput())
        }
    })
    
    # get list of seed and query sequence IDs ==================================
    output$seedID.ui <- renderUI({
        seqIDs <- getSeqID(getSeedInput())
        tagList(
            selectInput(
                ns("seedID"), "Seed ID",
                choices = c("all", seqIDs),
                selected = "all"
            ),
            bsPopover(
                ns("seedID"),
                "",
                paste(
                    "Sequence ID for extracting annotation",
                    "OR for calculating FAS score"
                ),
                "top"
            )
        )
    })
    
    output$queryID.ui <- renderUI({
        seqIDs <- getSeqID(getQueryInput())
        tagList(
            selectInput(
                ns("queryID"), "Query ID",
                choices = c("all", seqIDs),
                selected = "all"
            ),
            bsPopover(
                ns("queryID"),
                "",
                paste(
                    "Sequence ID for extracting annotation",
                    "OR for calculating FAS score"
                ),
                "top"
            )
        )
    })
    
    # get anno path ============================================================
    getAnnoPath <- reactive({
        shinyDirChoose(
            input, "annoDir", roots = homePath, session = session
        )
        annoPath <- parseDirPath(homePath, input$annoDir)
        return(replaceHomeCharacter(as.character(annoPath)))
    })
    output$annoDir.ui <- renderUI({
        req(getAnnoPath())
        if (length(getAnnoPath()) > 0) {
            em(getAnnoPath())
        }
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
            em(getOptAnnoTool())
        }
    })
    
    # get output path ==========================================================
    getOutPath <- reactive({
        shinyDirChoose(
            input, "outFasDir", roots = homePath, session = session
        )
        outPath <- parseDirPath(homePath, input$outFasDir)
        return(replaceHomeCharacter(as.character(outPath)))
    })
    output$outFasDir.ui <- renderUI({
        req(getOutPath())
        if (length(getOutPath()) > 0) {
            em(getOutPath())
        }
    })
    
    # get output name ==========================================================
    output$outName.ui <- renderUI({
        req(getSeedInput())
        req(getQueryInput())
        seedFile <- str_split(getSeedInput(), '/')
        seedFileTmp <- str_split(tail(seedFile[[1]], 1), "\\.")
        queryFile <- str_split(getQueryInput(), '/')
        queryFileTmp <- str_split(tail(queryFile[[1]], 1), "\\.")
        textInput(
            ns("outName"), 
            strong("Prefix of output files"), 
            value = paste0(seedFileTmp[[1]][1], "_", queryFileTmp[[1]][1])
        )
    })
    
    # get reference genomes ====================================================
    getRefFwd <- reactive({
        shinyFileChoose(
            input, "refFwd", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$refFwd)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$refFwd.ui <- renderUI({
        req(getRefFwd())
        if (length(getRefFwd()) > 0) {
            em(getRefFwd())
        }
    })
    
    getRefRev <- reactive({
        shinyFileChoose(
            input, "refRev", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$refRev)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$refRev.ui <- renderUI({
        req(getRefRev())
        if (length(getRefRev()) > 0) {
            em(getRefRev())
        }
    })
    
    # get other optional files =================================================
    getPhyloprofileMapping <- reactive({
        shinyFileChoose(
            input, "phyloprofile", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$phyloprofile)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$phyloprofile.ui <- renderUI({
        req(getPhyloprofileMapping())
        if (length(getPhyloprofileMapping()) > 0) {
            em(getPhyloprofileMapping())
        }
    })
    
    getFeatureTypes <- reactive({
        shinyFileChoose(
            input, "featureTypes", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$featureTypes)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$featureTypes.ui <- renderUI({
        req(getFeatureTypes())
        if (length(getFeatureTypes()) > 0) {
            em(getFeatureTypes())
        }
    })
    
    getWeightConstraints <- reactive({
        shinyFileChoose(
            input, "weightConstraints", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$weightConstraints)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$weightConstraints.ui <- renderUI({
        req(getWeightConstraints())
        if (length(getWeightConstraints()) > 0) {
            em(getWeightConstraints())
        }
    })
    
    # generate new job ID ======================================================
    observeEvent(input$newFasJob.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "fasJob", strong("Job ID"), value = jobID)
    })
    
    # required options =========================================================
    reqOptions <- reactive({
        req(getSeedInput())
        req(getQueryInput())
        req(input$outName)
        seed <- paste0("--seed ", getSeedInput())
        query <- paste0("--query ", getQueryInput())
        annotationDir <- ""
        if (length(getAnnoPath()) > 0)
            annotationDir <- paste0("--annotation_dir ", getAnnoPath())
        outDir <- ""
        if (length(getOutPath()) > 0)
            outDir <- paste0("--out_dir ", getOutPath())
        outName <- ""
        if (input$outName != "")
            outName <- paste0("--out_name ", input$outName)
        reqOptions <- c(seed, query, annotationDir, outDir, outName)
        return(
            reqOptions[unlist(lapply(reqOptions, function (x) x != ""))]
        )
    })
    
    # other options ============================================================
    optOptions <- reactive({
        if (input$optFasOption == FALSE) return("")
        # * general options ====================================================
        bidirectional <- ""
        if (input$bidirectional == TRUE)
            bidirectional <- paste0("--bidirectional")
        
        cpus <- ""
        if (input$cpus > 1) 
            cpus <- paste0("--cpus ", input$cpus)
        
        optOptions <- c(bidirectional, cpus)

        # * I/O options ========================================================
        seedID <- ""
        req(input$seedID)
        if (input$seedID != "all")
            seedID <- paste0("--seed_id ", input$seedID)
        
        queryID <- ""
        req(input$queryID)
        if (input$queryID != "all")
            queryID <- paste0("--query_id ", input$queryID)
        
        noArch <- ""
        if (input$noArch)
            noArch <- paste0("--no_arch")
        
        domain <- paste0("--domain")
        if (input$noDomain == TRUE)
            domain <- ""
        
        phyloprofile <- ""
        if (input$outputPhyloprofile == TRUE) {
            if (length(getPhyloprofileMapping()) > 0) {
                phyloprofile <- paste0(
                    "--phyloprofile ", getPhyloprofileMapping()
                )
            }
        }
        
        featureTypes <- ""
        if (length(getFeatureTypes()) > 0) {
            featureTypes <- paste0(
                "--featuretypes ", getFeatureTypes()
            )
        }
        
        optOptions <- c(
            optOptions,
            seedID, queryID, noArch, domain, phyloprofile, featureTypes
        )

        # * annotation options =================================================
        force <- ""
        if (input$force == TRUE) force <- paste0("--force")
        
        toolPath <- ""
        if (length(getOptAnnoTool()) > 0)
            toolPath <- paste0("--toolPath ", getOptAnnoTool())
       
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
            optOptions <- c(optOptions, efeature, einstance, eflps, org)
        }
        
        optOptions <- c(optOptions, force, toolPath)
       
        # * weighting options ==================================================
        refFwd <- ""
        if (length(getRefFwd()) > 0)
            refFwd <- paste0("--ref_proteome ", getRefFwd())
        
        refRev <- ""
        if (length(getRefRev()) > 0)
            refRev <- paste0("--ref_2 ", getRefRev())
        
        weightCorrection <- ""
        if (input$weightCorrection != "loge") {
            weightCorrection <- paste0(
                "--weight_correction ", input$weightCorrection
            )
        }
        
        weightConstraints <- ""
        if (length(getWeightConstraints()) > 0) {
            weightConstraints <- paste0(
                "--weight_constraints ", getWeightConstraints()
            )
        }
        
        optOptions <- c(
            optOptions, refFwd, refRev, weightCorrection, weightConstraints
        )
       
        # * threshold options ==================================================
        maxOverlap <- ""
        if (input$maxOverlap > 0)
            maxOverlap <- paste0("--max_overlap ", input$maxOverlap)
        
        maxOverlapPercentage <- ""
        if (input$maxOverlapPercentage != 0.4) {
            maxOverlapPercentage <- paste0(
                "--max_overlap_percentage ", input$maxOverlapPercentage
            )
        }
        
        priorityThreshold <- ""
        if (input$priorityThreshold != "50") {
            priorityThreshold <- paste0(
                "--priority_threshold ", input$priorityThreshold
            )
        }
        
        maxCardinality <- ""
        if (input$maxCardinality != "5000") {
            maxCardinality <- paste0(
                "--max_cardinality ", input$maxCardinality
            )
        }
        
        optOptions <- c(
            optOptions, maxOverlap, maxOverlapPercentage, priorityThreshold, 
            maxCardinality
        )
        return(optOptions)
    })
    
    # greedyFAS options ========================================================
    fasOptions <- reactive({
        fasOptions <- c(reqOptions(), optOptions())
        return(
            fasOptions[unlist(lapply(fasOptions, function (x) x != ""))]
        )
    })
    
    output$fasOptions.ui <- renderUI({
        HTML(paste(fasOptions(), collapse = "<br/>"))
    })
    
    # RUN greedyFAS ============================================================
    output$fasBtn.ui <- renderUI({
        if (length(reqOptions()) == 5) {
            tagList(
                bsButton(
                    ns("doFAS"), "Run greedyFAS",
                    style = "success", disabled = FALSE
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
        # updateButton(session, ns("doPlot"), disabled = FALSE)
    })
    
    observeEvent(input$stopFAS, {
        rvFas$started <- FALSE
        system2("rm", "*.fas.log")
        updateButton(session, ns("stopFAS"), disabled = TRUE)
    })
    
    observe({
        rvFas$timer()
        if (isolate(rvFas$started)) {
            if (file.exists(paste0(input$fasJob, ".fas.log"))) {
                rvFas$textstream <- suppressWarnings(
                    readLines(paste0(input$fasJob, ".fas.log"),  n = -1) %>% 
                        tail(50) %>% paste(collapse = "\n")
                )
            }
        }
    })
    output$fasLog <- renderText({
        rvFas$textstream
    })
    
    # render domain plot =======================================================
    observe({
        if (!("--bidirectional" %in% fasOptions()))
            updateButton(session, ns("doPlot"), disabled = TRUE)
        else {
            updateButton(session, ns("doPlot"), disabled = FALSE)
        }
    })
    
    output$seedIDplot.ui <- renderUI({
        req(input$seedID)
        seqIDs <- getSeqID(getSeedInput())
        if (input$seedID == "all") {
            selectInput(
                ns("seedIDplot"), "Seed ID",
                choices = seqIDs,
                selected = seqIDs[1]
            )
        } else {
            selectInput(
                ns("seedIDplot"), "Seed ID",
                choices = input$seedID, #seqIDs,
                selected = input$seedID
            )
        }
    })
    
    output$queryIDplot.ui <- renderUI({
        req(input$queryID)
        seqIDs <- getSeqID(getQueryInput())
        if (input$queryID == "all") {
            selectInput(
                ns("queryIDplot"), "Query ID",
                choices = seqIDs,
                selected = seqIDs[1]
            )
        } else {
            selectInput(
                ns("queryIDplot"), "Query ID",
                choices = input$queryID, #seqIDs,
                selected = input$queryID
            )
        }
    })
    
    output$fasScore <- renderPrint({
        req(input$doFAS)
        req(input$doPlot)
        fasOutput <- paste0(
            getOutPath(), "/", input$outName, "_table.csv"
        )
        req(file.exists(fasOutput))
        fasOutDf <- read.csv(
            fasOutput, header = TRUE, sep = ",", stringsAsFactors = FALSE
        )
        if (input$direction == "fwd") {
            return(paste(
                "Fwd FAS SCORE =",
                fasOutDf$forward[
                    fasOutDf$seedID == input$seedIDplot 
                    & fasOutDf$queryID == input$queryIDplot
                ]
            ))
        } else {
            return(paste(
                "Rev FAS SCORE =",
                fasOutDf$reverse[
                    fasOutDf$seedID == input$seedIDplot 
                    & fasOutDf$queryID == input$queryIDplot
                    ]
            ))
        }
    })
        
    getDomainInformation <- reactive({
        req(input$doFAS)
        req(input$doPlot)
        if (input$direction == "fwd") {
            inputDomain <- paste0(
                getOutPath(), "/", input$outName, "_forward.domains"
            )
        } else {
            inputDomain <- paste0(
                getOutPath(), "/", input$outName, "_reverse.domains"
            )
        }
        if (!(file.exists(inputDomain))) return(NULL)
        
        withProgress(message = 'Reading domain input...', value = 0.5, {
            domainDf <- parseDomainInput(
                NULL,
                inputDomain,
                "file"
            )
            return(domainDf)
        })
    })
    
    getInfoPair <- reactive({
        seedID <- input$outName #input$seedIDplot
        seedID <- gsub("\\|", ":", seedID)
        orthoID <- input$queryIDplot
        orthoID <- gsub("\\|", ":", orthoID)
        return(c(seedID, orthoID))
    })

    output$archiPlot <- renderPlot({
        if (input$doPlot > 0) {
            if (is.null(getDomainInformation())) {
                msgPlot()
            } else {
                g <- createArchiPlot(
                    getInfoPair(), getDomainInformation(), 12, 12
                )
                if (any(g == "No domain info available!")) {
                    msgPlot()
                } else {
                    grid::grid.draw(g)
                }
            }
        }
    })

    output$archiPlotFas.ui <- renderUI({
        ns <- session$ns
        plotOutput(
            ns("archiPlot"),
            height = input$archiHeight,
            width = input$archiWidth
        )
    })
    
    output$archiDownload <- downloadHandler(
        filename = function() {
            c("domains.pdf")
        },
        content = function(file) {
            g <- createArchiPlot(
                getInfoPair(), getDomainInformation(), 12, 12
            )
            grid.draw(g)
            ggsave(
                file, plot = g,
                width = input$archiWidth * 0.056458333,
                height = input$archiHeight * 0.056458333,
                units = "cm", dpi = 300, device = "pdf", limitsize = FALSE
            )
        }
    )
    
    # report results ===========================================================
    output$logFasLocation <- renderText({
        paste0(getwd(), "/", input$fasJob, ".fas.log")
    })
    
    output$outputFasLocation <- renderText({
        req(getOutPath())
        annoOutPath <- getOutPath()
        outName <- input$outName
        
        fasFile <- paste0(
            getOutPath(), "/", outName, ".xml"
        )
        archiFile <- ""
        if (input$noArch == FALSE) {
            archiFile <- paste0(
                getOutPath(), "/", outName, "_architecture.xml"
            )
        }
        if (input$optFasOption == TRUE) {
            revFile <- ""
            if (input$bidirectional == TRUE) {
                revFile <- paste0(
                    getOutPath(), "/", outName, "_reverse.xml"
                )
            }
            domainOut <- ""
            if (input$noDomain == FALSE) {
                domainOut <- paste0(
                    getOutPath(), "/", outName, "_*.domains"
                )
            }
            ppOut <- ""
            if (input$phyloprofile == TRUE) {
                ppOut <- paste0(
                    getOutPath(), "/", outName, ".phyloprofile"
                )
            }
            return(
                paste(fasFile, archiFile, revFile, domainOut, ppOut, sep = "\n")
            )
        } else {
            return(paste(fasFile, archiFile, sep = "\n"))
        }
    })
}
