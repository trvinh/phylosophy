#' HaMStR module

hamstrAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for hamstr input/options -----------------
        sidebarPanel(
            width = 3,
            # ** hamstr location ===================================
            conditionalPanel(
                condition = "output.checkHamstrStatus == 0", ns = ns,
                selectInput(
                    ns("whereHamstr"), "HaMStR not found! Please:",
                    choice = c("Install HaMStR", "Provide HaMStR path"),
                    selected = "Provide HaMStR path"
                ),
                conditionalPanel(
                    condition = "input.whereHamstr == 'Provide HaMStR path'",
                    ns = ns,
                    shinyFilesButton(
                        ns("hamstrFile"),
                        "HaMStR location?" ,
                        title = "Please provide oneSeq.pl file:",
                        multiple = FALSE,
                        buttonType = "default"
                    )
                ),
                conditionalPanel(
                    condition = "input.whereHamstr == 'Install HaMStR'",
                    ns = ns,
                    bsButton(
                        "installHamstr", "Install HaMStR", 
                        onclick = "window.open('https://bionf.github.io/HaMStR/#how-to-install', '_blank')"
                    )
                )
            ),
            hr(),
            
            # ** fasta input =======================================
            h3("Input and configurations"),
            hr(),
            shinyFilesButton(
                ns("hamstrInput"), "Input fasta file!" ,
                title = "Please provide fasta input file:", 
                multiple = FALSE,
                buttonType = "default", class = NULL
            ),
            br(),
            br(),
            
            # ** required options ==================================
            strong("Required options"),
            uiOutput(ns("seqID.ui")),
            bsPopover(
                ns("seqID"),
                "",
                paste(
                    "Specifie the sequence identifier of the seed",
                    "sequence in the reference protein set.",
                    "If not provided, the program will attempt to",
                    "determine it automatically."
                ),
                "bottom"
            ),
            
            uiOutput(ns("refSpec.ui")),
            bsPopover(
                ns("refSpec"),
                "",
                paste(
                    "Determine the reference species for the",
                    "hamstr search. It should be the species the",
                    "seed sequence was derived from."
                ),
                "bottom"
            ),
            
            selectInput(
                ns("minDist"), "Min systematic distance",
                choices = c(
                    "strain",
                    "species",
                    "genus",
                    "family",
                    "order",
                    "class",
                    "phylum",
                    "kingdom",
                    "superkingdom"
                ),
                selected = "genus"
            ),
            bsPopover(
                ns("minDist"),
                "",
                paste(
                    "Specify the minimum systematic distance of",
                    "primer taxa for the core set compilation."
                ),
                "top"
            ),
            
            selectInput(
                ns("maxDist"), "Max systematic distance",
                choices = c(
                    "strain",
                    "species",
                    "genus",
                    "family",
                    "order",
                    "class",
                    "phylum",
                    "kingdom",
                    "superkingdom"
                ),
                selected = "kingdom"
            ),
            bsPopover(
                ns("maxDist"),
                "",
                paste(
                    "Specify the maximum systematic distance of",
                    "primer taxa fto be considered for core set",
                    "compilation."
                ),
                "top"
            ),
            
            numericInput(
                ns("coreOrth"), "Number of core orthologs",
                value = 5,
                min = 3,
                max = 99
            ),
            bsPopover(
                ns("coreOrth"),
                "",
                paste(
                    "Specify the number of orthologs added to the",
                    "core set."
                ),
                "top"
            ),
            
            uiOutput(ns("seqName.ui")),
            bsPopover(
                ns("seqName"),
                "",
                paste(
                    "Specifies a name for the search. If not",
                    "defined, a random name will be set."
                ),
                "bottom"
            ),
            bsButton(ns("newSeqName.btn"), "New job name"),
            hr(),
            
            # ** FAS option ========================================
            checkboxInput(
                ns("useFAS"),
                strong("Use FAS"),
                value = TRUE,
                width = NULL
            ),
            conditionalPanel(
                condition = "input.useFAS && output.checkFasStatus == 0", 
                ns = ns,
                shinyFilesButton(
                    ns("greedyFasFile"), 
                    "FAS location?" ,
                    title = "Please provide greedyFAS.py file:", 
                    multiple = FALSE,
                    class = NULL
                    
                )
            ),
            bsPopover(
                ns("useFAS"),
                "",
                paste(
                    "FAS is blablabla",
                    "that do blebleble"
                ),
                "right"
            ),
            
            conditionalPanel(
                condition = "input.useFAS", ns = ns,
                numericInput(
                    ns("annoCores"),
                    strong("Number of CPUs for annotation"),
                    value = 4,
                    min = 1,
                    max = 99,
                    step = 1
                ),
                bsPopover(
                    ns("annoCores"),
                    "",
                    paste(
                        "Determine the number of CPUs used for doing annotation"
                    ),
                    "bottom"
                ),
            ),
            hr(),
            
            # ** optional options ==================================
            checkboxInput(
                ns("optOption"),
                strong("Other options"),
                value = FALSE,
                width = NULL
            ),
            
            conditionalPanel(
                condition = "input.optOption", ns = ns,
                strong("Additional options"),
                
                checkboxInput(
                    ns("otherOutpath"),
                    strong("Set output directory"),
                    value = FALSE,
                    width = NULL
                ),
                conditionalPanel(
                    condition = "input.otherOutpath", ns = ns,
                    shinyDirButton(
                        ns("outHamstrDir"), "Output directory" ,
                        title = "Please select a folder",
                        buttonType = "default", class = NULL
                    ),
                    bsPopover(
                        ns("outHamstrDir"),
                        "",
                        paste(
                            "Provide output directory"
                        ),
                        "top"
                    )
                ),
                bsPopover(
                    ns("otherOutpath"),
                    "",
                    paste(
                        "Default output path is the current working directory"
                    ),
                    "bottom"
                ),
                
                uiOutput(ns("coreTaxa.ui")),
                bsPopover(
                    ns("coreTaxa"),
                    "",
                    paste(
                        "You can provide a list of primer taxa",
                        "that should exclusively be used for the",
                        "compilation of the core ortholog set."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("strict"),
                    strong("Strict mode"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("strict"),
                    "",
                    paste(
                        "Run the final HaMStR search in strict",
                        "mode. An ortholog is only then accepted",
                        "when the reciprocity is fulfilled for",
                        "each sequence in the core set."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("coreStrict"),
                    strong("Strict mode for core set"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("coreStrict"),
                    "",
                    paste(
                        "Run the HaMStR for the compilation of",
                        "the core set in strict mode."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("checkCoorthologsRef"),
                    strong("Check co-orthologs"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("checkCoorthologsRef"),
                    "",
                    paste(
                        "During the final HaMStR search, accept",
                        "an ortholog also when its best hit in",
                        "the reverse search is not the core",
                        "ortholog itself, but a co-ortholog of it."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("corecheckCoorthologsRef"),
                    strong("Check co-orthologs for core set"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("corecheckCoorthologsRef"),
                    "",
                    paste(
                        "Invokes the checkCoorthologsRef",
                        "behavior in the course of the core set",
                        "compilation."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("rbh"),
                    strong("Reciprocal best hit"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("rbh"),
                    "",
                    paste(
                        "Requires a reciprocal best hit during the",
                        "HaMStR search to accept a new ortholog."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("rep"),
                    strong("Only most similar sequence"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("rep"),
                    "",
                    paste(
                        "Set this flag to obtain only the sequence",
                        "being most similar to the corresponding",
                        "sequence in the core set rather than all",
                        "putative co-orthologs."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("coreRep"),
                    strong("Only most similar sequence for core set"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("coreRep"),
                    "",
                    paste(
                        "Set this flag to invoke the -rep",
                        "behaviour for the core ortholog",
                        "compilation."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("coreOnly"),
                    strong("Compile only the core orthologs"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("coreOnly"),
                    "",
                    paste(
                        "Set this flag to compile only the core",
                        "orthologs. These sets can later be used",
                        "for a stand alone HaMStR search."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("reuseCore"),
                    strong("Reuse existing core-set"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("reuseCore"),
                    "",
                    paste(
                        "Set this flag if the core set for your",
                        "sequence is already existing. No check",
                        "currently implemented."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("blast"),
                    strong("Auto determin seqID and refspec"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("blast"),
                    "",
                    paste(
                        "Set this flag to determine sequence id",
                        "and refspec automatically. Note, the",
                        "chosen sequence id and reference species",
                        "does not necessarily reflect the species",
                        "the sequence was derived from."
                    ),
                    "bottom"
                ),
                
                textInput(
                    ns("group"), strong("Limit search in systematic group"),
                    value = ""
                ),
                bsPopover(
                    ns("group"),
                    "",
                    paste(
                        "Allows to limit the search to a certain",
                        "systematic group"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("evalBlast"),
                    strong("Blast e-value cutoff (10^x)"),
                    value = -5,
                    min = -99,
                    max = 0,
                    step = 1
                ),
                bsPopover(
                    ns("evalBlast"),
                    "",
                    paste(
                        "This option allows to set the e-value",
                        "cut-off for the Blast search.",
                        "Default: 10^-5"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("evalHmmer"),
                    strong("HMM search e-value cutoff (10^x)"),
                    value = -5,
                    min = -99,
                    max = 0,
                    step = 1
                ),
                bsPopover(
                    ns("evalHmmer"),
                    "",
                    paste(
                        "This option allows to set the e-value",
                        "cut-off for the HMM search.",
                        "Default: 10^-5"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("evalRelaxfac"),
                    strong("Relax factor for e-value cutoff"),
                    value = 10,
                    min = 1,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("evalRelaxfac"),
                    "",
                    paste(
                        "This options allows to set the factor to",
                        "relax the e-value cut-off (Blast search",
                        "and HMM search) for the final HaMStR run.",
                        "Default: 10"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("hitLimit"),
                    strong("Number of HMM hits"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("hitLimit"),
                    "",
                    paste(
                        "Provide an integer specifying the number",
                        "of hits of the initial pHMM based search",
                        "that should be evaluated via a reverse",
                        "search. Default: 10"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("coreHitLimit"),
                    strong("Number of HMM hits for core set"),
                    value = 3,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("coreHitLimit"),
                    "",
                    paste(
                        "Provide an integer specifying the number",
                        "of hits of the initial pHMM based search",
                        "that should be evaluated via a reverse",
                        "search. Default: 3"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("autoLimit"),
                    strong("Auto HMM hit limit"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("autoLimit"),
                    "",
                    paste(
                        "Setting this flag will invoke a lagPhase",
                        "analysis on the score distribution from",
                        "the hmmer search. This will determine",
                        "automatically a hit limit for each query.",
                        "Note, when setting this flag, it will be",
                        "effective for both the core ortholog",
                        "compilation and the final ortholog search."
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("scoreThreshold"),
                    strong("Threshold for HMM score"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("scoreThreshold"),
                    "",
                    paste(
                        "Instead of setting an automatic hit",
                        "limit, you can specify with this flag",
                        "that only candidates with an hmm score no",
                        "less than x percent of the hmm score of",
                        "the best hit are further evaluated.",
                        "Default is x = 10. You can change this",
                        "cutoff with the option -scoreCutoff.",
                        "Note, when setting this flag, it will be",
                        "effective for both the core ortholog",
                        "compilation and the final ortholog search."
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("scoreCutoff"),
                    strong("Percent range of the hmm score"),
                    value = 10,
                    min = 0,
                    max = 999,
                    step = 1
                ),
                bsPopover(
                    ns("scoreCutoff"),
                    "",
                    paste(
                        "In combination with -scoreThreshold you",
                        "can define the percent range of the hmms",
                        "core of the best hit up to which a",
                        "candidate of the hmmsearch will be",
                        "subjected for further evaluation.",
                        "Default: 10%"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("ignoreDistance"),
                    strong("Ignore taxon distance"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("ignoreDistance"),
                    "",
                    paste(
                        "Set this flag to ignore the distance",
                        "between taxa and to choose orthologs",
                        "only based on score"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("distDeviation"),
                    strong("Distance deviation"),
                    value = 1,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                bsPopover(
                    ns("distDeviation"),
                    "",
                    paste(
                        "Specify the deviation in score in percent",
                        "(1=100%, 0=0%) allowed for two taxa to be",
                        "considered similar"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("cpu"),
                    strong("Number of CPUs"),
                    value = 4,
                    min = 1,
                    max = 99,
                    step = 1
                ),
                bsPopover(
                    ns("cpu"),
                    "",
                    paste(
                        "Determine the number of threads to be run",
                        "in parallel"
                    ),
                    "bottom"
                ),
                
                selectInput(
                    ns("aligner"), "Alignment tool",
                    choices = c(
                        "mafft-linsi",
                        "muscle"
                    ),
                    selected = "muscle"
                ),
                bsPopover(
                    ns("aligner"),
                    "",
                    paste(
                        "Choose between mafft-linsi or muscle for",
                        "the multiple sequence alignment.",
                        "DEFAULT: muscle"
                    ),
                    "top"
                ),
                
                selectInput(
                    ns("alignStrategy"), "Alignment strategy",
                    choices = c(
                        "local",
                        "glocal",
                        "global"
                    ),
                    selected = "local"
                ),
                bsPopover(
                    ns("alignStrategy"),
                    "",
                    paste(
                        "Choose alignment strategy for core ortholog",
                        "compilation. DEFAULT: local"
                    ),
                    "top"
                ),
                
                checkboxInput(
                    ns("force"),
                    strong("Force mode"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("force"),
                    "",
                    paste(
                        "Force the final HaMStR search to create",
                        "output file. Existing files will be",
                        "overwritten."
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("append"),
                    strong("Append output"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("append"),
                    "",
                    paste(
                        "Set this flag to append the output to",
                        "existing output files"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("silent"),
                    strong("Silent run"),
                    value = FALSE,
                    width = NULL
                ),
                bsPopover(
                    ns("silent"),
                    "",
                    paste(
                        "Surpress output to the command line"
                    ),
                    "bottom"
                ),
                
                checkboxInput(
                    ns("cleanup"),
                    strong("Clean up tmp files"),
                    value = TRUE,
                    width = NULL
                ),
                bsPopover(
                    ns("cleanup"),
                    "",
                    paste(
                        "Temporary output will be deleted."
                    ),
                    "bottom"
                )
            )
        ),
        # * main panel for hamstr run ----------------------------
        mainPanel(
            width = 9,
            column(
                6,
                uiOutput(ns("hamstrBtn.ui")),
                hr(),
                strong("SELECTED OPTIONS"),
                br(), br(),
                strong("Required"),
                br(), br(),
                uiOutput(ns("reqOptions.ui")),
                br(), br(),
                strong("Optional"),
                br(), br(),
                uiOutput(ns("optOptions.ui")),
                hr(),
                strong("Log file"),
                verbatimTextOutput(ns("logLocation")),
                strong("Output files"),
                verbatimTextOutput(ns("outputLocation"))
            ),
            column(
                6,
                strong("Command"),
                verbatimTextOutput(ns("hamstrCmdText")),
                hr(),
                strong("Progress"),
                verbatimTextOutput(ns("hamstrLog"))
            )
        )
    )
}

hamstrApp <- function(input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # get hamstr location ======================================================
    output$checkHamstrStatus <- reactive({
        hamstrLocation <- suppressWarnings(
            system("which oneSeq", intern = TRUE)
        )
        if (!is.na (hamstrLocation[1])){
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkHamstrStatus", suspendWhenHidden = FALSE)
    
    getHamstrPath <- reactive({
        hamstrLocation <- suppressWarnings(
            system("which oneSeq", intern = TRUE)
        )
        if (!is.na (hamstrLocation[1])){
            return(hamstrLocation[1])
        } else {
            shinyFileChoose(
                input, "hamstrFile", roots = homePath, session = session
            )
            req(input$hamstrFile)
            fileSelected <- parseFilePaths(homePath, input$hamstrFile)
            return(replaceHomeCharacter(as.character(fileSelected$datapath)))
        }
    })
    
    output$hamstrLocation <- renderText({
        hamstrPath <- getHamstrPath()
        paste(
            "HaMStR found at", hamstrPath
        )
    })
    
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
            shinyFileChoose(
                input, "greedyFasFile", roots = homePath, session = session
            )
            req(input$greedyFasFile)
            fileSelected <- parseFilePaths(homePath, input$greedyFasFile)
            return(replaceHomeCharacter(as.character(fileSelected$datapath)))
        }
    })
    
    output$fasLocation <- renderText({
        fasPath <- getFasPath()
        paste(
            "FAS found at", fasPath
        )
    })
    
    # get input fasta ==========================================================
    getInputPath <- reactive({
        shinyFileChoose(
            input, "hamstrInput", session = session, roots = homePath, 
            filetypes = c('', 'fa', 'fasta')
        )
        req(input$hamstrInput)
        fileSelected <- parseFilePaths(homePath, input$hamstrInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    
    # get list of sequence IDs =================================================
    output$seqID.ui <- renderUI({
        seqIDs <- getSeqID(getInputPath())
        selectInput(
            ns("seqID"), "Seed ID",
            choices = seqIDs,
            selected = seqIDs[1]
        )
    })
    
    # get list of available refspec ============================================
    getSubFolderDir <- function (hamstrPath = NULL, subFolderName = NULL) {
        if (is.null(hamstrPath)) stop("HaMStR not found!")
        if (length(grep("oneSeq.pl", hamstrPath))) {
            return(
                stringr::str_replace(
                    hamstrPath, "/bin/oneSeq.pl", paste0("/", subFolderName)
                )
            )
        } else {
            return(
                stringr::str_replace(
                    hamstrPath, "/bin/oneSeq", paste0("/", subFolderName)
                )
            )
        }
    }
    
    getRefspecList <- function(hamstrPath = NULL, subFolderName = NULL){
        outDir <- getSubFolderDir(getHamstrPath(), subFolderName)
        outDirPath <- list.dirs(
            path = outDir, full.names = TRUE, recursive = FALSE
        )
        outDirList <- stringr::str_replace(
            outDirPath, paste0(outDir,"/"), ""
        )
        return(outDirList)
    }
    
    output$refSpec.ui <- renderUI({
        refspecList <- c("undefined")
        refspecList <- getRefspecList(getHamstrPath(), "blast_dir")
        selectInput(
            ns("refSpec"), "Reference species",
            choices = c("undefined", refspecList),
            selected = "undefined"
        )
    })
    
    output$coreTaxa.ui <- renderUI({
        coreTaxaList <- "all"
        coreTaxaList <- getRefspecList(getHamstrPath(), "blast_dir")
        selectInput(
            ns("coreTaxa"), strong("Core taxa"),
            choices = c("all", coreTaxaList),
            selected = "all",
            multiple = TRUE
        )
    })
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outHamstrDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outHamstrDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
    # required options =========================================================
    reqOptions <- reactive({
        req(getInputPath())
        # copy input file to hamstr/data folder
        dataDir <- getSubFolderDir(getHamstrPath(), "data")
        system2(
            "rsync", paste("-aq", getInputPath(), dataDir, sep = " "), 
            wait = TRUE
        )
        
        # seqFile <- paste0("-seqFile=", "infile.fa")
        seqFile <- paste0("-seqFile=", getFileName(getInputPath()))
        seqId <- paste0("-seqId=", input$seqID)
        refSpec <- paste0("-refSpec=", input$refSpec)
        minDist <- paste0("-minDist=", input$minDist)
        maxDist <- paste0("-maxDist=", input$maxDist)
        coreOrth <- paste0("-coreOrth=", input$coreOrth)
        return(
            c(seqFile, seqId, refSpec, minDist, maxDist, coreOrth)
        )
    })
    
    output$reqOptions.ui <- renderUI({
        HTML(paste(reqOptions(), collapse = "<br/>"))
    })
    
    # optional options =========================================================
    optOptions <- reactive({
        outpath <- ""
        if (length(getOutputPath()) > 0) {
            outpath <- paste0("-outpath=", getOutputPath())
        }
        
        fasoff <- ""
        if (input$useFAS == FALSE) {
            fasoff <- paste0("-fasoff")
        }
        
        annoCores <- ""
        if (input$annoCores != "1") {
            annoCores <- paste0("-annoCores=", input$annoCores)
        }
        
        seqName <- ""
        if (!is.null(input$seqName)) {
            seqName <- paste0("-seqName=", input$seqName)
        }
        
        coreTaxa <- ""
        if(input$optOption == TRUE && !is.null(input$coreTaxa)) {
            if (!("all" %in% input$coreTaxa)) {
                coreTaxa <- paste0(
                    "-coreTaxa=", paste(input$coreTaxa, collapse = ",")
                )
            }
        } 
        
        strict <- ""
        if (input$strict == TRUE) {
            strict <- paste0("-strict")
        }
        coreStrict <- ""
        if (input$coreStrict == TRUE) {
            coreStrict <- paste0("-coreStrict")
        }
        checkCoorthologsRef <- ""
        if (input$checkCoorthologsRef == TRUE) {
            checkCoorthologsRef <- paste0("-checkCoorthologsRef")
        }
        corecheckCoorthologsRef <- ""
        if (input$corecheckCoorthologsRef == TRUE) {
            corecheckCoorthologsRef <- paste0("-CorecheckCoorthologsRef")
        }
        
        rbh <- ""
        if (input$rbh == TRUE) {
            rbh <- paste0("-rbh")
        }
        rep <- ""
        if (input$rep == TRUE) {
            rep <- paste0("-rep")
        }
        coreRep <- ""
        if (input$coreRep == TRUE) {
            coreRep <- paste0("-coreRep")
        }
        
        coreOnly <- ""
        if (input$coreOnly == TRUE) {
            coreOnly <- paste0("-coreOnly")
        }
        reuseCore <- ""
        if (input$reuseCore == TRUE) {
            reuseCore <- paste0("-reuseCore")
        }
        blast <- ""
        if (input$blast == TRUE) {
            blast <- paste0("-blast")
        }
        
        group <- ""
        if (input$group != "") {
            group <- paste0("-group=", input$group)
        }
        
        evalBlast <- ""
        if (input$evalBlast != "-5") {
            evalBlast <- paste0("-evalBlast=", 10^input$evalBlast)
        }
        evalHmmer <- ""
        if (input$evalHmmer != "-5") {
            evalHmmer <- paste0("-evalHmmer=", 10^input$evalHmmer)
        }
        evalRelaxfac <- ""
        if (input$evalRelaxfac != 10) {
            evalRelaxfac <- paste0("-evalRelaxfac=", input$evalRelaxfac)
        }
        
        hitLimit <- ""
        if (input$hitLimit != "10") {
            hitLimit <- paste0("-hitLimit=", input$hitLimit)
        }
        coreHitLimit <- ""
        if (input$coreHitLimit != "3") {
            coreHitLimit <- paste0("-coreHitLimit=", input$coreHitLimit)
        }
        
        autoLimit <- ""
        if (input$autoLimit == TRUE) {
            autoLimit <- paste0("-autoLimit")
        }
        
        scoreThreshold <- ""
        if (input$scoreThreshold != "10") {
            scoreThreshold <- paste0("-scoreThreshold=", input$scoreThreshold)
        }
        scoreCutoff <- ""
        if (input$scoreCutoff != "10") {
            scoreCutoff <- paste0("-scoreCutoff=", input$scoreCutoff)
        }
        
        ignoreDistance <- ""
        if (input$ignoreDistance == TRUE) {
            ignoreDistance <- paste0("-ignoreDistance")
        }
        distDeviation <- ""
        if (input$distDeviation != "1") {
            distDeviation <- paste0("-distDeviation=", input$distDeviation)
        }
        
        cpu <- ""
        if (input$cpu != "1") {
            cpu <- paste0("-cpu=", input$cpu)
        }
        aligner <- ""
        if (input$aligner != "muscle") {
            aligner <- paste0("-aligner=", input$aligner)
        }
        alignStrategy <- ""
        if (input$alignStrategy != "local") {
            alignStrategy <- paste0("-", input$alignStrategy)
        }
        
        force <- ""
        if (input$force == TRUE) {
            force <- paste0("-force")
        }
        append <- ""
        if (input$append == TRUE) {
            append <- paste0("-append")
        }
        silent <- ""
        if (input$silent == TRUE) {
            silent <- paste0("-silent")
        }
        cleanup <- ""
        if (input$cleanup == TRUE) {
            cleanup <- paste0("-cleanup")
        }
        
        optOptionList <- c(
            outpath, fasoff, annoCores, seqName, coreTaxa, strict, coreStrict, 
            checkCoorthologsRef, corecheckCoorthologsRef, rbh, rep, coreRep, 
            coreOnly, reuseCore, blast, group, evalBlast, evalHmmer, 
            evalRelaxfac, hitLimit, coreHitLimit, autoLimit, scoreThreshold, 
            scoreCutoff,ignoreDistance, distDeviation, cpu, aligner, 
            alignStrategy, force, append, silent, cleanup
        )
        return(
            optOptionList[unlist(lapply(optOptionList, function (x) x != ""))]
        )
    })
    
    output$optOptions.ui <- renderUI({
        HTML(paste(optOptions(), collapse = "<br/>"))
    })
    
    # generate new job ID ======================================================
    output$seqName.ui <- renderUI({
        jobID <- randFn(1)
        textInput(ns("seqName"), "Job name", value = jobID)
    })
    
    observeEvent(input$newSeqName.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "seqName", "Job name", value = jobID)
    })
    
    # RUN HAMSTR ===============================================================
    output$hamstrBtn.ui <- renderUI({
        if (
            !is.null(input$refSpec) && !is.null(input$seqID) && input$seqID !=""
        ) {
            if (input$refSpec != "undefined") {
                tagList(
                    bsButton(
                        ns("doHamstr"), "Run HaMStR",
                        style = "success", disabled = FALSE
                    ),
                    actionButton(ns("stopHamstr"),label = "Stop"),
                    actionButton(ns("newHamstr"),label = "New job"),
                    textOutput(ns("hamstrLocation")),
                    
                    conditionalPanel(
                        condition = 'input.useFAS', ns = ns,
                        textOutput(ns("fasLocation"))
                    )
                )
            }
        }
    })
    
    observeEvent(input$newHamstr, {
        updateButton(session, ns("doHamstr"), disabled = FALSE)
        updateButton(session, ns("stopHamstr"), disabled = FALSE)
    })
    
    hamstrCmd <- reactive({
        return(
            paste(
                getHamstrPath(),
                paste(reqOptions(), collapse = " "),
                paste(optOptions(), collapse = " ")
            )
        )
    })
    
    output$hamstrCmdText <- renderText({
        paste("perl", hamstrCmd())
    })
    
    rv <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )
    observeEvent(input$doHamstr, {
        rv$started <- TRUE
        cmd <- paste(
            hamstrCmd(),
            ">>",
            paste0(input$seqName, ".log")
        )
        system2("perl", cmd, wait = FALSE)
        updateButton(session, ns("doHamstr"), disabled = TRUE)
        updateButton(session, ns("newSeqName.btn"), disabled = TRUE)
    })
    
    observeEvent(input$stopHamstr, {
        rv$started <- FALSE
        system2("rm", "*.log")
        updateButton(session, ns("stopHamstr"), disabled = TRUE)
    })
    
    observe({
        rv$timer()
        if (isolate(rv$started)) {
            if (file.exists(paste0(input$seqName, ".log"))) {
                rv$textstream <- suppressWarnings(
                    paste(
                        readLines(paste0(input$seqName, ".log"),  n = -1) %>% 
                            tail(50) %>% paste(collapse = "\n")
                    )
                )
            }
        }
    })
    output$hamstrLog <- renderText({
        rv$textstream
    })
    
    # report results ===========================================================
    output$logLocation <- renderText({
        paste0(getwd(), "/", input$seqName, ".log")
    })
    
    returnOutput <- reactive({
        req(getHamstrPath())
        req(getInputPath())
        # get output path
        if (input$otherOutpath == TRUE && length(getOutputPath()) > 0) {
            outpath <- paste0(getOutputPath(), "/", input$seqName)
        } else {
            outpath <- paste0(getwd(), "/", input$seqName)
        }
        # dataDir <- getSubFolderDir(getHamstrPath(), "data")
        # return output files
        faOut <- paste0(outpath, "/", input$seqName, ".extended.fa")
        ppOut <- paste0(outpath, "/", input$seqName, ".phyloprofile")
        if (input$useFAS == TRUE) {
            domainFwOut <- paste0(outpath, "/", input$seqName, "_1.domains")
            return(c(faOut, ppOut, domainFwOut))
        } else {
            return(c(faOut, ppOut, NULL))
        }
    })
    
    output$outputLocation <- renderText({
        # req(getHamstrPath())
        # req(getInputPath())
        # # get default output folder of hamstr
        # dataDir <- getSubFolderDir(getHamstrPath(), "data")
        # # return output files
        # faOut <- paste0(dataDir, "/", input$seqName, ".extended.fa")
        # ppOut <- paste0(dataDir, "/", input$seqName, ".phyloprofile")
        # if (input$useFAS == TRUE) {
        #     inFaOut <- paste0(dataDir, "/", getFileName(getInputPath()))
        #     domainFwOut <- paste0(dataDir, "/", input$seqName, "_1.domains")
        #     domainRvOut <- paste0(
        #         "[", dataDir, "/", input$seqName, "_0.domains", "]"
        #     )
        #     paste(inFaOut, faOut, ppOut, domainFwOut, domainRvOut, sep = "\n")
        # } else {
        #     paete(faOut, ppOut, sep = "\n")
        # }
        return(paste(returnOutput(), collapse = "\n"))
    })
    
    # return
    return(returnOutput)
}
