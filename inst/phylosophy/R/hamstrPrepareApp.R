#' add new taxon to hamstr module

hamstrPrepareAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for input/options -----------------
        sidebarPanel(
            width = 3,
            # ** hamstr location ===================================
            conditionalPanel(
                condition = "output.checkHamstrStatus == 0", ns = ns,
                h2(em("HaMStR not found! Please install it first!")),
                bsButton(
                    "installHamstr", "Install HaMStR", 
                    onclick = "window.open('https://bionf.github.io/HaMStR/#how-to-install', '_blank')"
                ),
                hr()
            ),
            
            # ** fasta input =======================================
            h3("Input and configurations"),
            hr(),
            
            # ** required options ==================================
            selectInput(
                ns("inputType"),
                "",
                choices = c("Add single taxon", "Add list of taxa"),
                selected = "Add single taxon"
            ),
            
            conditionalPanel(
                condition = 'input.inputType == "Add single taxon"', ns = ns,
                shinyFilesButton(
                    ns("fastaInput"), "Input fasta file" ,
                    title = "Please provide fasta file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("fastaInput.ui")),
                br(), 
                
                strong("Taxon ID"),
                numericInput(
                    ns("taxid"), "", value = 999999999, min = 1, step = 1
                ),
                uiOutput(ns("idCheck.ui")),
                br(),
                strong("Acronym name"),
                textInput(
                    ns("abbrName"), "", 
                    placeholder = "e.g. HOMSA for Homo sapiens"
                ),
                checkboxInput(ns("useName"), "Use suggested name", value = TRUE),
                uiOutput(ns("nameCheck.ui"))
            ),
            
            conditionalPanel(
                condition = 'input.inputType == "Add list of taxa"', ns = ns,
                shinyDirButton(
                    ns("inputDir"), "Input directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("inputDir.ui")),
                br(),
                
                shinyFilesButton(
                    ns("mappingFile"), "Input mapping file" ,
                    title = "Please provide mapping file",
                    multiple = FALSE,
                    buttonType = "default", class = NULL
                ),
                uiOutput(ns("mappingFile.ui")),
            ),
            br(),
            
            strong("Output directory"),
            checkboxInput(ns("optOutPath"), "Other output path", value = FALSE),
            conditionalPanel(
                condition = "input.optOutPath", ns = ns,
                shinyDirButton(
                    ns("outPrepareDir"), "Set output directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                )
            ),
            uiOutput(ns("outputLocation.ui")),
            br(),
            
            textInput(ns("prepareJob"), strong("Job ID"), value = randFn(1)),
            bsPopover(
                ns("prepareJob"),
                "",
                paste(
                    "Name of job and log file(s)."
                ),
                "bottom"
            ),
            bsButton(ns("newPrepareJob.btn"), "New job ID"),
            br(),
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
                
                checkboxInput(
                    ns("coreTaxa"), strong("Include to core taxa"),
                    value = FALSE
                ),
                bsPopover(
                    ns("coreTaxa"),
                    "",
                    paste(
                        "Include this taxon to the core taxa, which are",
                        "normally can be found in HaMStR/blast_dir"
                    ),
                    "bottom"
                ),
                
                numericInput(
                    ns("ver"), strong("Proteome version"), 
                    value = 1, min = 1, step = 1
                ),
                
                checkboxInput(
                    ns("noAnno"), strong("DO NOT do feature annotation"),
                    value = FALSE
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
                    ns("force"), strong("Overwrite existing data"),
                    value = FALSE
                )
            )
        ),
        # * main panel -------------------------------
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "output.checkRunPrepare", ns = ns,
                bsButton(
                    ns("doPrepare"), "Run",
                    style = "success", disabled = FALSE
                ),
                actionButton(ns("stopPrepare"),label = "Stop"),
                actionButton(ns("newPrepare"),label = "New job"),
                hr()
            ),
            conditionalPanel(
                condition = 'input.inputType == "Add list of taxa"', ns = ns,
                dataTableOutput(ns("checkTable")),
                uiOutput(ns("checkMsg.ui"))
            ),
            conditionalPanel(
                condition = "output.checkRunPrepare", ns = ns,
                strong("Command"),
                verbatimTextOutput(ns("prepareCmdText")),
                strong("Log file"),
                textOutput(ns("logPrepareLocation")),
                hr(),
                strong("Progress"),
                verbatimTextOutput(ns("prepareLog")),
            )
        )
    )
}

hamstrPrepareApp <- function (input, output, session, nameFullDf) {
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
            hamstrPath <- gsub("/bin/oneSeq", "", hamstrLocation[1])
            return(hamstrPath)
        } else {
            return(NULL)
        }
    })
    
    getPreparePath <- reactive({
        hamstrPath <- getHamstrPath()
        if (!is.null(hamstrPath)){
            if (input$inputType == "Add single taxon")
                return(paste0(hamstrPath, "/bin/addTaxonHamstr.py"))
            else
                return(paste0(hamstrPath, "/bin/addTaxaHamstr.py"))
        } else {
            return(NULL)
        }
    })
    
    # get input fasta ==========================================================
    getFastaInput <- reactive({
        shinyFileChoose(
            input, "fastaInput", roots = homePath, session = session,
            filetypes = c('', 'fa', 'fasta')
        )
        fileSelected <- parseFilePaths(homePath, input$fastaInput)
        req(input$fastaInput)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$fastaInput.ui <- renderUI({
        req(getFastaInput())
        if (length(getFastaInput()) > 0) {
            outString <- getFastaInput()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    # get input folder and mapping file ========================================
    getInputDir <- reactive({
        shinyDirChoose(
            input, "inputDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$inputDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    output$inputDir.ui <- renderUI({
        req(getInputDir())
        if (length(getInputDir()) > 0) {
            outString <- getInputDir()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
    
    getMappingFile <- reactive({
        shinyFileChoose(
            input, "mappingFile", roots = homePath, session = session
        )
        fileSelected <- parseFilePaths(homePath, input$mappingFile)
        # req(input$mappingFile)
        return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    })
    output$mappingFile.ui <- renderUI({
        req(getMappingFile())
        if (length(getMappingFile()) > 0) {
            outString <- getMappingFile()
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
            input, "outPrepareDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outPrepareDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
    outputLocation <- reactive({
        if (input$optOutPath == FALSE) {
            return(getHamstrPath())
        } else {
            if (length(getOutputPath()) > 0)
                return(getOutputPath())
            else
                return(NULL)
        }
    })
    
    output$outputLocation.ui <- renderUI({
        if (!is.null(outputLocation())) {
            outString <- outputLocation()
            if (nchar(outString) > 30)
                outString <- paste0(
                    substrLeft(outString, 15), "...", substrRight(outString, 15)
                )
            em(outString)
        }
    })
 
    # generate new job ID ======================================================
    observeEvent(input$newPrepareJob.btn, {
        jobID <- randFn(1)
        updateTextInput(session, "prepareJob", strong("Job ID"), value = jobID)
    })
    
    # check taxon ID ===========================================================
    getRefspec <- reactive ({
        # get spec ID from blast_dir
        outDir <- paste0(getHamstrPath(), "/genome_dir")
        outDirPath <- list.dirs(
            path = outDir, full.names = TRUE, recursive = FALSE
        )
        outDirTaxIds <- stringr::str_replace(
            outDirPath, paste0(outDir,"/"), ""
        )
        # ids <- str_replace_all(str_match(outDirTaxIds, "@.+@"), "@", "")
        return(outDirTaxIds)
    })
    
    checkTaxId <- reactive({
        req(input$taxid)
        taxInfo <- nameFullDf[nameFullDf$ncbiID == input$taxid,]
        if (nrow(taxInfo) == 0) {
            return(0)
        } else {
            if (
                paste0(input$abbrName, "@", input$taxid, "@", input$ver) 
                %in% getRefspec()
            ) {
                return(1)
            } else {
                return(paste(as.list(taxInfo), collapse = ", "))
            }
        }
    })
    
    output$idCheck.ui <- renderUI({
        if (checkTaxId() == 0) {
            em(paste("NOTE: This ID not found in NCBI taxonomy database!"))
        } else if (checkTaxId() == 1) {
            em("ERROR: This gene set already present in HaMStR!")
        } else {
            em(paste("Taxon:", checkTaxId()))
        }
    })
    
    # check IDs in mapping file ================================================
    parseMappingFile <- reactive({
        req(getMappingFile())
        mapDf <- read.csv(
            getMappingFile(), header = TRUE, sep = "\t", fill = TRUE,
            na.strings = "NA", stringsAsFactors = FALSE
        )
        if (nrow(mapDf) < 1) return(NULL)
        if (ncol(mapDf) < 4) return("Wrong mapping file!")
        mapDf[mapDf == ""] <- NA
        mapDf$version[is.na(mapDf$version)] <- 1
        colnames(mapDf) <- c("fileName", "ncbiID", "taxName", "version")
        mapDf <- merge(
            mapDf, nameFullDf[, c("ncbiID", "fullName")], by = "ncbiID",
            all.x = TRUE
        )
        abbrNameList <- lapply(
            mapDf$fullName, function (x) createAbbrName(x)
        )
        mapDf$abbrName <- abbrNameList
        mapDf$taxName[is.na(mapDf$taxName)] <-
            mapDf$abbrName[is.na(mapDf$taxName)]
        mapDf$taxName[is.na(mapDf$taxName)] <-
            paste0("UNK", mapDf$ncbiID[is.na(mapDf$taxName)])
        mapDf$suggestedName <- paste0(
            mapDf$taxName, "@", mapDf$ncbiID, "@", mapDf$version
        )
        mapDf$exist <- mapDf$suggestedName %in% getRefspec()
        mapDf$note[is.na(mapDf$fullName)] <- "Not found in NCBI"
        mapDf$note[mapDf$exist == TRUE] <- "Already exists"
        mapDf$note[is.na(mapDf$note)] <- "OK"
        outDf <- mapDf[
            , c("fileName", "ncbiID", "fullName", "suggestedName", "note")
        ]
        return(outDf[order(outDf$note),])
    })

    output$checkTable <- renderDataTable(rownames= FALSE, {
        parseMappingFile()
    })

    checkMappingFile <- reactive({
        df <- parseMappingFile()
        df <- df[df$note == "Already exists",]
        return(nrow(df))
    })
    
    output$checkMsg.ui <- renderUI({
        req(getHamstrPath())
        if (checkMappingFile() > 0) {
            msg <- paste(
                "Some gene sets already exist in", getHamstrPath(),
                ". Please remove them or change their name/id/version",
                "in the mapping file"
            )
            tagList(
                br(),
                em(strong(msg)),
                hr(),
            )
        }
    })
    
    # get suggest taxon abbr name ==============================================
    suggestName <- reactive({
        req(checkTaxId())
        if (checkTaxId() != 0 && checkTaxId() != 1) {
            taxInfo <- str_split(checkTaxId(), ", ")
            suggestName <- createAbbrName(taxInfo[[1]][2])
            return(suggestName)
        }
    })
    
    observeEvent(suggestName(),{
        if (input$useName == TRUE) {
            updateTextInput(session, "abbrName", "", value = suggestName())
        }
    })
    
    output$nameCheck.ui <- renderUI({
        req(input$abbrName)
        req(input$taxid)
        em(
            "Output name: ", 
            paste0("", input$abbrName, "@", input$taxid, "@", input$ver)
        )
    })
    
    # required options =========================================================
    reqOptions <- reactive({
        req(input$taxid)
        fasta <- paste0("--fasta ", getFastaInput())
        outPath <- ""
        if (!is.null(outputLocation())) 
            outPath <- paste0("--outPath ", outputLocation())
        name <- ""
        if (!(input$abbrName == ""))
            name <- paste("--name", input$abbrName)
        taxid <- ""
        if (input$taxid > 0)
            taxid <- paste("--taxid", input$taxid)
        reqOption <- c(fasta, outPath, name, taxid)
        return(
            reqOption[unlist(lapply(reqOption, function (x) x != ""))]
        )
    })
    
    reqOptionsTaxa <- reactive({
        input <- ""
        if (length(getInputDir()) > 0)
            input <- paste("--input", getInputDir())
        mapping <- ""
        if (length(getMappingFile()) > 0)
            mapping <- paste("--mapping", getMappingFile())
        outPath <- ""
        if (!is.null(outputLocation())) 
            outPath <- paste0("--outPath ", outputLocation())
        reqOption <- c(input, mapping, outPath)
        return(
            reqOption[unlist(lapply(reqOption, function (x) x != ""))]
        )
    })
    
    # optional options =========================================================
    optOptions <- reactive({
        coreTaxa <- ""
        if (input$coreTaxa == TRUE) coreTaxa <- paste("--coreTaxa")
        ver <- ""
        if (input$ver > 1) ver <- paste("--verProt", input$ver)
        noAnno <- ""
        if (input$noAnno == TRUE) noAnno <- paste("--noAnno")
        cpus <- ""
        if (input$annoCPU > 1) cores <- paste0("--cpus ", input$annoCPU)
        force <- ""
        if (input$force == TRUE) force <- paste("--force")
        optOptions <- c(coreTaxa, ver, noAnno, cpus, force)
        return(
            optOptions[unlist(lapply(optOptions, function (x) x != ""))]
        )
    })
    
    # RUN annFAS ===============================================================
    output$checkRunPrepare <- reactive({
        if (input$inputType == "Add single taxon") {
            if (length(reqOptions()) == 4 && checkTaxId() != 1)
                return(TRUE)
        } else {
            if (length(reqOptionsTaxa()) == 3 && checkMappingFile() == 0)
                return(TRUE)
        }
        return(FALSE)
    })
    outputOptions(output, "checkRunPrepare", suspendWhenHidden = FALSE)
    
    observeEvent(input$newPrepare, {
        updateButton(session, ns("doPrepare"), disabled = FALSE)
        updateButton(session, ns("stopPrepare"), disabled = FALSE)
    })
    
    prepareCmd <- reactive({
        if (input$inputType == "Add single taxon") {
            return(
                paste(
                    getPreparePath(),
                    paste(reqOptions(), collapse = " "),
                    paste(optOptions(), collapse = " ")
                )
            )
        } else {
            return(
                paste(
                    getPreparePath(),
                    paste(reqOptionsTaxa(), collapse = " "),
                    paste(optOptions(), collapse = " ")
                )
            )
        }

    })
    
    output$prepareCmdText <- renderText({
        paste("python3", prepareCmd())
    })
    
    rvPrepare <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )
    
    observeEvent(input$doPrepare, {
        rvPrepare$started <- TRUE
        cmd <- paste(
            "python3",
            prepareCmd(),
            ">>",
            paste0(input$prepareJob, ".prepare.log")
        )
        
        system(cmd, wait = FALSE)
        updateButton(session, ns("doPrepare"), disabled = TRUE)
        updateButton(session, ns("newPrepareJob.btn"), disabled = TRUE)
    })
    
    observeEvent(input$stopPrepare, {
        rvPrepare$started <- FALSE
        system2("rm", "*.prepare.log")
        updateButton(session, ns("stopPrepare"), disabled = TRUE)
    })
    
    observe({
        rvPrepare$timer()
        if (isolate(rvPrepare$started)) {
            if (file.exists(paste0(input$prepareJob, ".prepare.log"))) {
                rvPrepare$textstream <- suppressWarnings(
                    readLines(
                        paste0(input$prepareJob, ".prepare.log"),  n = -1
                    ) %>% tail(50) %>% paste(collapse = "\n")
                )
            }
        }
    })
    output$prepareLog <- renderText({
        rvPrepare$textstream
    })
    
    # report results ===========================================================
    output$logPrepareLocation <- renderText({
        paste0(getwd(), "/", input$prepareJob, ".prepare.log")
    })
}
