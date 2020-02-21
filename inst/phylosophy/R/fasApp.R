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
            em("(*) required options"),
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
            bsPopover(
                ns("addQueryCheck"),
                "",
                paste(
                    "Add another sequences (required for calculating FAS)"
                ),
                "bottom"
            ),
            hr(),
            
            # ** job ID ========================================================
            textInput(ns("fasJob"), strong("Job ID (*)"), value = randFn(1)),
            bsPopover(
                ns("fasJob"),
                "",
                paste(
                    "Name of job and log file(s). This will also be file name",
                    "of greedyFAS outputs."
                ),
                "bottom"
            ),
            bsButton(ns("newFasJob.btn"), "New job ID"),
            hr(),
            
            # ** annoFAS options ===============================================
            strong("Annotation settings"),
            br(), br(),
            shinyDirButton(
                ns("outAnnoDir"), "Annotation directory (*)" ,
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
            br(), br(),
            
            textInput(
                ns("seedName"), "Seed Name (*)",
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
                    ns("queryName"), "Query Name (*)",
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
            
            checkboxInput(
                ns("optAnnoOption"),
                strong("Other options"),
                value = FALSE,
                width = NULL
            ),
            
            conditionalPanel(
                condition = "input.optAnnoOption", ns = ns,
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
                    strong("Do annotation with"),
                    choices = c(
                        "all", "pfam", "smart", "cast", "coils", "seg", "tmhmm",
                        "signalp"
                    ),
                    selected = "all"
                ),
                bsPopover(
                    ns("redo"),
                    "",
                    paste(
                        "Database(s)/Tool(s) used for annotation"
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
                )
            ),
            hr(),
            
            # ** Reference annotation settings =================================
            strong("Reference annotation"),
            conditionalPanel(
                condition = "input.extract || input.addQueryCheck", ns = ns,
                shinyDirButton(
                    ns("refAnnoDir"), "Reference annotation" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                bsPopover(
                    ns("refAnnoDir"),
                    "",
                    paste(
                        "Provide folder for existing reference annotation"
                    ),
                    "top"
                ),
                
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
            ),
            hr(),
            
            # ** greedyFAS options ==================================
            strong("greedyFAS options"),
            br(), br(),
            conditionalPanel(
                condition = "input.addQueryCheck", ns = ns,
                
                uiOutput(ns("refProteome.ui")),
                bsPopover(
                    ns("refProteome.ui"),
                    "",
                    paste(
                        "Path to annotation of a reference proteome which can",
                        "be used for the weighting of features, by default",
                        "there is no reference proteome used, the weighting",
                        "will be uniform."
                    ),
                    "bottom"
                ),
                
                conditionalPanel(
                    condition = "
			        input.bidirectional && input.refProteome != 'undefined'",
                    ns = ns,
                    uiOutput(ns("refProteome2.ui")),
                    bsPopover(
                        ns("refProteome2.ui"),
                        "",
                        paste(
                            "Give a second reference for bidirectional mode,",
                            "does not do anything if bidirectional mode is not",
                            "active or if no main reference was given"
                        ),
                        "bottom"
                    )
                ),
                
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
                
                checkboxInput(
                    ns("optOption"),
                    strong("Other options"),
                    value = FALSE,
                    width = NULL
                ),
                
                conditionalPanel(
                    condition = "input.optOption", ns = ns,
                    selectInput(
                        ns("rawOutput"), strong("Output type"),
                        choices = c(0,1,2), selected = 2
                    ),
                    bsPopover(
                        ns("rawOutput"),
                        "",
                        paste(
                            "If set to 1, the FAS score will be printed to",
                            "STDOUT. If 0, scores will be printed into output",
                            "file (XML format). If 2, both output variants",
                            "are conducted."
                        ),
                        "top"
                    ),
                    
                    checkboxInput(
                        ns("noArch"), strong("No architecture output"),
                        value = FALSE
                    ),
                    bsPopover(
                        ns("noArch"),
                        "",
                        paste(
                            "Deactivate creation of architecture.xml file"
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("outputDomain"), strong("Domain tabular output"),
                        value = TRUE
                    ),
                    bsPopover(
                        ns("outputDomain"),
                        "",
                        paste(
                            "Activate domain tabular output"
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("outputPhyloprofile"), strong("PhyloProfile output"),
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.outputPhyloprofile", ns = ns,
                        shinyFilesButton(
                            ns("phyloprofile"), "Mapping file for PhyloProfile!" ,
                            title = "Please provide mapping file for PhyloProfile",
                            multiple = FALSE,
                            buttonType = "default", class = NULL
                        )
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
                    
                    checkboxInput(
                        ns("featureInfo"), strong("Feature stat output"),
                        value = FALSE
                    ),
                    bsPopover(
                        ns("featureInfo"),
                        "",
                        paste(
                            "Create a file with information on the abundance",
                            "of all seed and query features in the reference"
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
                    ),
                    
                    numericInput(
                        ns("efilter"),
                        strong("HMM search e-value cutoff for feature (10^x)"),
                        value = -3,
                        min = -99,
                        max = 0,
                        step = 1
                    ),
                    bsPopover(
                        ns("efilter"),
                        "",
                        paste(
                            "E-value filter for hmm based search methods",
                            "(feature based/complete sequence)."
                        ),
                        "bottom"
                    ),
                    
                    numericInput(
                        ns("instEfilter"),
                        strong(
                            "HMM search e-value cutoff for instances (10^x)"
                        ),
                        value = -2,
                        min = -99,
                        max = 0,
                        step = 1
                    ),
                    bsPopover(
                        ns("instEfilter"),
                        "",
                        paste(
                            "E-value filter for hmm based search methods",
                            "(instances based/complete sequence)."
                        ),
                        "bottom"
                    ),
                    
                    selectInput(
                        ns("weightcorrection"), 
                        strong("Weight correction type"),
                        choices = c(
                            "linear", "loge", "log10", "root4", "root8"
                        ),
                        selected = "loge"
                    ),
                    bsPopover(
                        ns("weightcorrection"),
                        "",
                        paste(
                            "Function applied to the frequency of feature",
                            "types during weighting, options are linear(no",
                            "function), loge(natural logarithm[Default]),",
                            "log10(base-10 logarithm), root4(4th root) and",
                            "root8(8th root)"
                        ),
                        "top"
                    ),
                    
                    checkboxInput(
                        ns("useWeightConstraints"), 
                        strong("Use weight constraints"),
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.useWeightConstraints", ns = ns,
                        shinyFilesButton(
                            ns("weightConstraints"), "Weight constraints file!",
                            title = "Please provide weight constraints file",
                            multiple = FALSE,
                            buttonType = "default", class = NULL
                        )
                    ),
                    bsPopover(
                        ns("weightConstraints"),
                        "",
                        paste(
                            "Provide weight constraints via constraints file.",
                            "By default there are no constraints."
                        ),
                        "bottom"
                    ),
                    
                    checkboxInput(
                        ns("limitFeatureTypes"), strong("Limit feature types"),
                        value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.limitFeatureTypes", ns = ns,
                        shinyFilesButton(
                            ns("featureTypes"), "Feature types file!" ,
                            title = "Please provide feature types file",
                            multiple = FALSE,
                            buttonType = "default", class = NULL
                        )
                    ),
                    bsPopover(
                        ns("featureTypes"),
                        "",
                        paste(
                            "Provide file that contains the tools/databases",
                            "used to predict features"
                        ),
                        "bottom"
                    ),
                    
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
                        ns("timelimit"), 
                        strong("Time limit"),
                        min = 0, max = 99999999, step = 1, value = 7200
                    ),
                    bsPopover(
                        ns("timelimit"),
                        "",
                        paste(
                            "Sets a maximum time-limit in seconds for the",
                            "calculation between a pair of proteins,default is",
                            "2 hours after which it will stop, set to 0 to",
                            "deactivate; As FAS divides this time among",
                            "multiple processes, this limit does not",
                            "necessarily represent the actual runtime,",
                            "especially if multiple cores are used"
                        ),
                        "bottom"
                    ),
                    
                    numericInput(
                        ns("cores"), 
                        strong("Number of cores"),
                        min = 1, max = 999, step = 1, value = 1
                    ),
                    bsPopover(
                        ns("cores"),
                        "",
                        paste(
                            "Number of cores available for calculation, only",
                            "useful when not using priority_mode"
                        ),
                        "bottom"
                    )
                )
            )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
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
                strong("Command"),
                verbatimTextOutput(ns("annoCmdText")),
                strong("Progress"),
                verbatimTextOutput(ns("annoLog"))
            ),
            column(
                6,
                uiOutput(ns("fasBtn.ui")),
                hr(),
                strong("greedyFAS OPTIONS"),
                br(), br(),
                uiOutput(ns("fasOptions.ui")),
                hr(),
                strong("Log file"),
                verbatimTextOutput(ns("logFasLocation")),
                strong("Output files"),
                verbatimTextOutput(ns("outputFasLocation")),
                hr(),
                strong("Command"),
                verbatimTextOutput(ns("fasCmdText")),
                strong("Progress"),
                verbatimTextOutput(ns("fasLog")),
                hr(),
                bsButton(
                    ns("doPlot"), "Plot feature architecture", 
                    style = "info", disabled = TRUE
                )
            ),
            
            # * popup for domain plot ------------------------------------------
            bsModal(
                "domainPlot",
                "Protein feature architecture plot",
                ns("doPlot"),
                size = "large",
                wellPanel(
                    column(
                        6,
                        uiOutput(ns("seedIDplot.ui")),
                    ),
                    column(
                        6,
                        uiOutput(ns("queryIDplot.ui")),
                    )
                ),
                hr(),
                uiOutput(ns("archiPlot.ui"))
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
        fileSelected <- parseFilePaths(homePath, input$seedInput)
        req(input$seedInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    getQueryPath <- reactive({
        shinyFileChoose(
            input, "queryInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$queryInput)
        req(input$queryInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    # get list of seed and query sequence IDs ==================================
    output$seedID.ui <- renderUI({
        seqIDs <- getSeqID(getSeedPath())
        tagList(
            selectInput(
                ns("seedID"), "Seed ID",
                choices = c("all", seqIDs),
                selected = seqIDs[1]
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
        seqIDs <- getSeqID(getQueryPath())
        tagList(
            selectInput(
                ns("queryID"), "Query ID",
                choices = c("all", seqIDs),
                selected = seqIDs[1]
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
    
    output$refProteome.ui <- renderUI({
        refAnnoList <- c("undefined")
        selectInput(
            ns("refProteome"), "Reference annotation",
            choices = c("undefined", getRefAnno()),
            selected = "undefined"
        )
    })
    
    output$refProteome2.ui <- renderUI({
        refAnnoList <- c("undefined")
        selectInput(
            ns("refProteome2"), "Reference annotation (backward)",
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
    observeEvent(input$newFasJob.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "fasJob", "Job ID", value = jobID)
    })
    
    # annoFAS options ==========================================================
    annoOptions <- reactive({
        fasta <- paste0("--fasta=", getSeedPath())
        path <- ""
        if (length(getOutputPath()) > 0) 
            path <- paste0("--path=", getOutputPath())
        name <- ""
        if (input$seedName != "") name <- paste0("--name=", input$seedName)
        
        if (input$addQueryCheck == TRUE && !is.null(input$annoObj)) {
            if (input$annoObj == "query") {
                fasta <- paste0("--fasta=", getQueryPath())
                name <- ""
                if (input$queryName != "") 
                    name <- paste0("--name=", input$queryName)
            }
        }
        redo <- ""
        if (input$redo != "all") redo <- paste0("--redo=", input$redo)
        force <- ""
        if (input$force == TRUE) force <- paste0("--force")
        
        annoOption <- c(fasta, path, name, redo, force)
        
        extract <- ""
        if (input$extract == TRUE) {
            req(getRefDir())
            # name of existing annotation folder (identified by seed name)
            path <- paste0(
                "--path=", 
                getRefDir(), "/", input$seedName
            )
            # ID of sequence need to get annotation
            name <- paste0("--name=", input$seedID)
            # extract is output dir
            extract <- paste0(
                "--extract=", 
                getOutputPath(), "/", input$seedName, "_", input$seedID
            )
            
            if (input$addQueryCheck == TRUE && !is.null(input$annoObj)) {
                if (input$annoObj == "query") {
                    path <- paste0(
                        "--path=", 
                        getRefDir(), "/", input$queryName
                    )
                    name <- paste0("--name=", input$queryID)
                    extract <- paste0(
                        "--extract=", getOutputPath(), "/", 
                        input$queryName, "_", input$queryID
                    )
                }
            }
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
    output$annoBtn.ui <- renderUI({
        if (length(input$seedInput) > 1 && length(input$outAnnoDir) > 1) {
            tagList(
                bsButton(
                    ns("doAnno"), "Run annoFAS",
                    style = "success", disabled = FALSE
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
    getPhyloprofileMapping <- reactive({
        shinyFileChoose(
            input, "phyloprofile", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$phyloprofile)
        req(input$phyloprofile)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    getWeightConstraints <- reactive({
        shinyFileChoose(
            input, "weightConstraints", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$weightConstraints)
        req(input$weightConstraints)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    getFeatureTypes <- reactive({
        shinyFileChoose(
            input, "featureTypes", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$featureTypes)
        req(input$featureTypes)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    
    fasOptions <- reactive({
        req(input$addQueryCheck)
        req(getOutputPath())
        query <- ""
        if (input$queryName != "") 
            query <- paste0("--query=", getOutputPath(), "/", input$queryName)
        seed <- ""
        if (input$seedName != "") 
            seed <- paste0("--seed=", getOutputPath(), "/", input$seedName)
        job <- ""
        if (input$fasJob != "") 
            job <- paste0("--job=", getOutputPath(), "/", input$fasJob)
        rawOutput <- paste0("--raw_output=", input$rawOutput)
        queryID <- ""
        if (input$queryID != "all")
            queryID <- paste0("--query_id=\"", input$queryID, "\"")
        seedID <- ""
        if (input$seedID != "all")
            seedID <- paste0("--seed_id=\"", input$seedID, "\"")
        
        refProteome <- ""
        if (input$refProteome != "undefined")
            refProteome <- paste0("--ref_proteome=", input$refProteome)
        refProteome2 <- ""
        if (input$refProteome2 != "undefined")
            refProteome2 <- paste0("--ref_2=", input$refProteome2)
        
        bidirectional <- ""
        if (input$bidirectional == TRUE)
            bidirectional <- paste0("--bidirectional")
        
        featureInfo <- ""
        if (input$featureInfo == TRUE)
            featureInfo <- paste0("--feature_info")
        
        priorityThreshold <- ""
        if (input$priorityThreshold != "50") {
            priorityThreshold <- paste0(
                "--priority_threshold=", input$priorityThreshold
            )
        }
        maxCardinality <- ""
        if (input$maxCardinality != "5000") {
            maxCardinality <- paste0(
                "--max_cardinality=", input$maxCardinality
            )
        }
        
        efilter <- ""
        if (input$efilter != "-3")
            efilter <- paste0("--efilter=", 10^input$efilter)
        instEfilter <- ""
        if (input$instEfilter != "-2")
            instEfilter <- paste0("--inst_efilter=", 10^input$instEfilter)
        
        weightcorrection <- ""
        if (input$weightcorrection != "loge") {
            weightcorrection <- paste0(
                "--weightcorrection=", input$weightcorrection
            )
        }
        
        phyloprofile <- ""
        if (input$outputPhyloprofile == TRUE) {
            if (length(getPhyloprofileMapping()) > 0)
                phyloprofile <- paste0(
                    "--phyloprofile=", getPhyloprofileMapping()
                )
        }
        
        domain <- paste("--domain")
        if (input$outputDomain == FALSE) domain <- ""
        
        weightConstraints <- ""
        if (input$useWeightConstraints == TRUE) {
            if (length(getWeightConstraints()) > 0) {
                weightConstraints <- paste0(
                    "--weight_constraints=", getWeightConstraints()
                )
            }
        }
        
        featureTypes <- ""
        if (input$limitFeatureTypes == TRUE) {
            if (length(getFeatureTypes()) > 0) {
                featureTypes <- paste0(
                    "--featuretypes=", getFeatureTypes()
                )
            }
        }
        
        maxOverlap <- ""
        if (input$maxOverlap > 0)
            maxOverlap <- paste0("--max_overlap=", input$maxOverlap)
        
        maxOverlapPercentage <- ""
        if (input$maxOverlapPercentage != 0.4) {
            maxOverlapPercentage <- paste0(
                "--max_overlap_percentage=", input$maxOverlapPercentage
            )
        }
        
        timelimit <- ""
        if (input$timelimit != 7200)
            timelimit <- paste0("--timelimit=", input$timelimit)
        
        cores <- ""
        if (input$cores > 1)
            cores <- paste0("--cores=", input$cores)
        
        fasOption <- c(
            query, seed, job, rawOutput, queryID, seedID, refProteome, 
            refProteome2, bidirectional, featureInfo, phyloprofile, domain,
            priorityThreshold, maxCardinality, efilter, instEfilter, 
            weightcorrection, weightConstraints, featureTypes, maxOverlap, 
            maxOverlapPercentage, timelimit, cores
        )
        return(
            fasOption[unlist(lapply(fasOption, function (x) x != ""))]
        )
    })
    
    output$fasOptions.ui <- renderUI({
        HTML(paste(fasOptions(), collapse = "<br/>"))
    })
    
    # RUN greedyFAS ============================================================
    output$fasBtn.ui <- renderUI({
        if (input$addQueryCheck == TRUE && length(input$outAnnoDir) > 1) {
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
        updateButton(session, ns("doPlot"), disabled = FALSE)
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
    output$fasLog <- renderText({
        rvFas$textstream
    })
    
    # render domain plot =======================================================
    output$seedIDplot.ui <- renderUI({
        req(input$seedID)
        seqIDs <- getSeqID(getSeedPath())
        if (input$seedID == "all") {
            selectInput(
                ns("seedIDplot"), "Seed ID",
                choices = seqIDs,
                selected = seqIDs[1]
            )
        } else {
            selectInput(
                ns("seedIDplot"), "Seed ID",
                choices = seqIDs,
                selected = input$seedID
            )
        }
    })
    
    output$queryIDplot.ui <- renderUI({
        req(input$queryID)
        seqIDs <- getSeqID(getQueryPath())
        if (input$queryID == "all") {
            selectInput(
                ns("queryIDplot"), "Query ID",
                choices = seqIDs,
                selected = seqIDs[1]
            )
        } else {
            selectInput(
                ns("queryIDplot"), "Query ID",
                choices = seqIDs,
                selected = input$queryID
            )
        }
    })
        
    getDomainInformation <- reactive({
        req(input$doFAS)
        req(input$doPlot)
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
            seedID <- input$fasJob
            orthoID <- input$queryID #plot
            orthoID <- gsub("\\|", ":", orthoID)
            info <- c(seedID, orthoID)
            g <- createArchiPlot(
                info, getDomainInformation(), 12, 12
            )
            if (any(g == "No domain info available!")) {
                return("No domain info available!")
            } else {
                grid::grid.draw(g)
            }
        }
    })

    output$archiPlot.ui <- renderUI({
        ns <- session$ns
        plotOutput(
            ns("archiPlot"),
            height = 400,
            width = 800
        )
    })
    
    # report results ===========================================================
    output$logAnnoLocation <- renderText({
        paste0(getwd(), "/", input$fasJob, ".anno.log")
    })
    
    output$outputAnnoLocation <- renderText({
        req(getOutputPath())
        annoOutPath <- getOutputPath()
        jobName <- input$seedName
        if (input$addQueryCheck == TRUE  && !is.null(input$annoObj)) {
            if (input$annoObj == "query") {
                jobName <- input$queryName
            }
        }
        outFiles <- paste0(annoOutPath, "/", jobName, "/*.xml")
        
        if (input$extract == TRUE) {
            outFiles <- paste0(
                getOutputPath(), "/", input$seedName, "_", input$seedID,
                "/*.xml"
            )
            if (input$addQueryCheck == TRUE  && !is.null(input$annoObj)) {
                if (input$annoObj == "query") {
                    outFiles <- paste0(
                        getOutputPath(), "/", input$queryName, "_", 
                        input$queryID, "/*.xml"
                    )
                }
            }
        }
        return(outFiles)
    })
    
    output$logFasLocation <- renderText({
        paste0(getwd(), "/", input$fasJob, "fas.log")
    })
    
    output$outputFasLocation <- renderText({
        req(getOutputPath())
        annoOutPath <- getOutputPath()
        jobName <- input$fasJob
        
        if (input$addQueryCheck == TRUE) {
            fasFile <- paste0(
                getOutputPath(), "/", jobName, ".xml"
            )
            archiFile <- paste0(
                getOutputPath(), "/", jobName, "_architecture.xml"
            )
            revFile <- ""
            if (input$bidirectional == TRUE) {
                revFile <- paste0(
                    getOutputPath(), "/", jobName, "_reverse.xml"
                )
            }
            domainOut <- ""
            if (input$outputDomain == TRUE) {
                domainOut <- paste0(
                    getOutputPath(), "/", jobName, "_*.domains"
                )
            }
            ppOut <- ""
            if (input$phyloprofile == TRUE) {
                ppOut <- paste0(
                    getOutputPath(), "/", jobName, ".phyloprofile"
                )
            }
            return(
                paste(fasFile, archiFile, revFile, ppOut, domainOut, sep = "\n")
            )
        } else {
            return("")
        }
    })
}
