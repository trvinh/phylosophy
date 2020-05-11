#' DCC module

dccAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for FAS input/options -----------------
        sidebarPanel(
            width = 3,

            # ** fasta input =======================================
            h3("Input and configurations"),
            hr(),

            # conditionalPanel(
            #     condition = "output.checkPython == 0", ns = ns,
                # type of inputs
                radioButtons(
                    ns("inputTyp"), strong("Select your input type"),
                    choices = list(
                        "NCBI Taxonomy ID" = "ncbiID",
                        "Scientific name" = "speciesName",
                        "Input taxa list" = "inputFile",
                        "OMA Group ID" = "OmaId",
                        "OMA standalone file" = "omaFile"
                    ),
                    selected = "ncbiID"
                ),
                
                conditionalPanel(
                    condition = "input.inputTyp == 'omaFile'", ns = ns,
                    shinyFilesButton(
                        ns("omaFileInput"),
                        "Upload OMA file (orthoXML)" ,
                        title = "Please provide OMA file in orthoXML format",
                        multiple = FALSE,
                        buttonType = "default"
                    ),
                    br(), br(),
                    
                    selectInput(
                        ns("taxType"), strong("Get genne sets from:"),
                        c("Local directory", "OMA Database"),
                        selected = "Local directory"
                    ),
                    
                    conditionalPanel(
                        condition = "input.taxType == 'Local directory'", 
                        ns = ns,
                        shinyDirButton(
                            ns("genomeDir"), "Gene set directory" ,
                            title = "Please select gene set directory",
                            buttonType = "default", class = NULL
                        ),
                        br(), br(),
                    ),
                    
                    shinyFilesButton(
                        ns("taxMappingFile"),
                        "TaxID mapping file" ,
                        title = "Please provide taxon ID mapping file",
                        multiple = FALSE,
                        buttonType = "default"
                    ),
                    br(), br(),
                ),

                conditionalPanel(
                    condition = "input.inputTyp != 'omaFile'", ns = ns,
                    # oma data path
                    shinyDirButton(
                        ns("omaDataDir"), "OMA data directory" ,
                        title = "Please select OMA data directory",
                        buttonType = "default", class = NULL
                    ),
                    br(), br(),
                    # list of avail oma spec
                    uiOutput(ns("omaSpec")),
                    
                    # No. of missing species allowed a common OmaGroup
                    uiOutput(ns("nrMissingSpecies")),
                    
                    # update mode or the normal mode should be used
                    uiOutput(ns("update")),
                    conditionalPanel(
                        condition = "input.inputTyp == 'ncbiID' || input.inputTyp == 'speciesName' || input.inputTyp == 'inputFile'", 
                        ns = ns,
                        checkboxInput(ns("update"), label = "update Mode")
                    )
                ),
                
                # MSA tool selection
                radioButtons(
                    ns("MSA"),
                    strong("MSA tool"), choices = c("MAFFT", "MUSCLE"), selected = "MAFFT"
                ),
                
                br(),
                shinyDirButton(
                    ns("outAnnoDir"), "Output directory" ,
                    title = "Please select a folder",
                    buttonType = "default", class = NULL
                ),
                hr(),
                
                # creates the action button to run the calculation
                bsButton(ns("submit"), "Run")
            # )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            # conditionalPanel(
            #     condition = "output.checkPython == 1", ns = ns,
            #     htmlOutput(ns("pythonMsg"))
            # ),
            # conditionalPanel(
            #     condition = "output.checkPython == 0", ns = ns,
                
                # oma version
                verbatimTextOutput(ns("version")),
                hr(),
                
                # list of taxa selected by names ir IDs
                DT::dataTableOutput(ns("speciesTable")),
                
                # list of taxa from user input file
                DT::dataTableOutput(ns("checkTax")),
                
                # list of taxa in the given oma group
                DT::dataTableOutput(ns("omaGroupSpeciesTable")),
                uiOutput(ns("speciesSelectionOmaGroup")),
                
                # error msg for invalid input taxa
                span(textOutput(ns("error")), style = "color:red"),
                
                # number of obtained common oma groups
                verbatimTextOutput(ns("nrOmaGroups")),
                
                # arguments of orthoxmlParser 
                conditionalPanel(
                    condition = "input.inputTyp == 'omaFile'", ns = ns,
                    strong("Standalone OMA parser options:"),
                    uiOutput(ns("omaParserOptions")),
                    br(), br(),
                ),
                
                # finishing msg
                uiOutput(ns("end")),
                uiOutput(ns("annoOptions.ui"))
            # )
        )
    )
}

dccApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # check python version ====================================================
    # output$checkPython <- reactive({
    #     # python <- suppressWarnings(system("python -V", intern = TRUE))
    #     if (try(system("python -V") < "Python 3")) {
    #         return(1)
    #     } else return(0)
    # })
    # outputOptions(output, "checkPython", suspendWhenHidden = FALSE)
    # 
    # output$pythonMsg <- renderText({
    #     # python <- suppressWarnings(system("python -V", intern = TRUE))
    #     paste(
    #         "<font color=\"#FF0000\"><b><p>", "Python2", # python, 
    #         " was found, while DCC requires Python3! </p>",
    #         "<p>Please check again!</p>", "</b></font>"
    #     )
    # })
    python <- reactive({
        if (try(system("python -V") < "Python 3")) {
            return("python3")
        } else {
            return("python")
        }
    })
    
    # get OMA data path =======================================================
    getOmaPath <- reactive({
        shinyDirChoose(
            input, "omaDataDir", roots = homePath, session = session
        )
        omaPath <- parseDirPath(homePath, input$omaDataDir)
        return(replaceHomeCharacter(as.character(omaPath)))
    })
    
    # render oma version =======================================================
    output$version <- renderPrint({
        req(getOmaPath())
        y <- cat(
            system(
                paste(python(), "scripts/getVersion.py", getOmaPath()), 
                intern = TRUE
            )
        )
    })
    
    # get standalone OMA info ==================================================
    # * oma input path ========================================================
    getLocalOma <- reactive({
        shinyFileChoose(
            input, "omaFileInput", roots = homePath, session = session
        )
        req(input$omaFileInput)
        file_selected <- parseFilePaths(homePath, input$omaFileInput)
        return(as.character(file_selected$datapath))
    })
    # * data set directory ===================================================
    getLocalDataset <- reactive({
        shinyDirChoose(
            input, "genomeDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$genomeDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    # * mapping file path =====================================================
    getTaxMapping <- reactive({
        shinyFileChoose(
            input, "taxMappingFile", roots = homePath, session = session
        )
        # req(input$taxMappingFile)
        file_selected <- parseFilePaths(homePath, input$taxMappingFile)
        return(as.character(file_selected$datapath))
    })
    
    observe({
        if (input$inputTyp == "omaFile") {
            if (length(getLocalOma()) == 0) disable("submit")
            if (length(getLocalDataset()) == 0) disable("submit")
            if (length(getTaxMapping()) == 0) disable("submit")
        }
    })
    
    # * read taxonomy info from mapping file ==================================
    readTaxMapping <- reactive({
        req(getTaxMapping())
        taxFile <- read.table(
            getTaxMapping(), sep = "\t", header = FALSE, comment.char = "", 
            stringsAsFactors = FALSE
        )
        colnames(taxFile) <- c("NCBI ID", "Gene set name", "Abbr specices name")
        return(taxFile)
    })
    
    # * render orthoxmlParser options =========================================
    omaParserOptions <- reactive({
        inFile <- ""
        if (length(getLocalOma()) > 0) 
            inFile <- paste("--inFile", getLocalOma())
        outPath <- ""
        if (length(getOutputPath()) > 0) 
            outPath <- paste("--outPath", getOutputPath())
        data <- ""
        if (length(getLocalDataset()) > 0) 
            data <- paste("--geneSet", getLocalDataset())
        mapping <- ""
        if (length(getTaxMapping()) > 0) 
            mapping <- paste("--mappingFile", getTaxMapping())
        tool <- paste("--alignTool", input$MSA)
        parserOption <- c(inFile, outPath, data, mapping, tool)
        return(
            parserOption[unlist(lapply(parserOption, function (x) x != ""))]
        )
    })
    
    output$omaParserOptions <- renderUI({
        HTML(paste(omaParserOptions(), collapse = "<br/>"))
    })

    # process dowloaded OMA database ==========================================
    # * load the oma-species file from OmaDb ===================================
    readOmaSpec <- reactive({
        req(getOmaPath())
        taxTable <- fread(
            paste0(getOmaPath(), "/oma-species.txt"), header = FALSE, skip = 2, sep = "\t"
        )
        colnames(taxTable) <- c(
            "OMAcode", "TaxonID", "ScientificName", "GenomeSource",
            "Version/Release"
        )
        return(taxTable)
    })

    # * load the transformed oma-groups-tmp.txt ================================
    readOmaGroup <- reactive({
        req(getOmaPath())
        groupTable <- fread(paste0(getOmaPath(), "/oma-groups-tmp.txt"))
        return(groupTable)
    })

    # * render list of avail taxa based on selected input type =================
    # * except OMA standalone
    output$omaSpec <- renderUI({
        omaSpecTable <- readOmaSpec()
        if (input$inputTyp == "ncbiID") {
            selectInput(
                inputId = ns("species"),
                label = "Select up to 10 species",
                multiple = TRUE,
                choices = omaSpecTable$TaxonID
            )
        } else if (input$inputTyp == "speciesName") {
            selectInput(
                inputId = ns("species"),
                label = "Select up to 10 species",
                multiple = TRUE,
                choices = omaSpecTable$ScientificName
            )
        } else if (input$inputTyp == "inputFile") {
            fileInput(ns("taxFile"), "Choose File")
        } else if (input$inputTyp == "OmaId") {
            x = readOmaGroup()
            numericInput(
                inputId = ns("omaGroupId"),
                value = NULL,
                label = "Select a Oma Group Id between 1 and 866647",
                min = 1,
                max = nrow(x),
                step = 1
            )
        }
    })

    # * render number of allowed missing species ===============================
    # * except OMA standalone
    output$nrMissingSpecies <- renderUI({
        if (input$inputTyp != "OmaId") {
            if (input$inputTyp != "inputFile") {
                selectInput(
                    ns("nrMissingSpecies"),
                    strong("How many species can be missed in an OmaGroup"),
                    choices = seq(0,(length(input$species)-1))
                )
            } else if (input$inputTyp == "inputFile") {
                inFile <- input$taxFile
                if (!(is.null(inFile))) {
                    #         taxaInFile <- 1
                    #         selectInput(
                    #                 inputId = ns("nrMissingSpecies"),
                    #                 label = "How many species can be missed in an OmaGroup",
                    #                 choices = 0
                    #         )
                    # } else {
                    taxaInFile <- read.table(inFile$datapath, header = FALSE)
                    selectInput(
                        ns("nrMissingSpecies"),
                        strong("How many species can be missed in an OmaGroup"),
                        choices = seq(0,(nrow(taxaInFile)-1))
                    )
                }
            }
        }
    })

    # render table of selected taxa ===========================================
    # * from taxa FILE ========================================================
    # load input file
    getFileTable <- reactive({
        inFile <- input$taxFile
        speciesTable <- readOmaSpec()
        if (is.null(inFile)) {
            DF <- NULL
            return(DF)
        } else if (input$inputTyp == "inputFile") {
            taxaInFile <- read.table(inFile$datapath, header = FALSE)
            colnames(taxaInFile) <- "TaxonID"
            DF <- merge(
                taxaInFile, speciesTable[,c("TaxonID", "ScientificName")],
                by = "TaxonID",
                all.x = TRUE
            )
            return(DF)
        }
    })
    
    # get valid & invalid taxa from input file
    makeWarningWindow <- reactive({
        req(input$taxFile)
        DF <- getFileTable()
        notAvailableTaxons <- DF$TaxonID[is.na(DF$ScientificName)]
        if (length(notAvailableTaxons) > 0) {
            shinyalert(
                title = "warning",
                text = paste(
                    "The following TaxonIds aren't available in Oma. Do you",
                    "want to continue without them?", notAvailableTaxons
                ),
                type = "warning", showConfirmButton = TRUE,
                showCancelButton = TRUE
            )
        }
    })
    
    disableButton <- observeEvent(input$shinyalert, {
        if (length(input$shinyalert) > 0) {
            if (input$shinyalert == FALSE) {
                disable("submit")
            }
        }
    })
    
    getTaxaFromFile <- reactive({
        req(input$taxFile)
        DF <- getFileTable()
        notAvailableTaxons <- DF$TaxonID[is.na(DF$ScientificName)]
        DF$ScientificName[is.na(DF$ScientificName)] <- "not available yet"
        if (length(notAvailableTaxons) > 0) {
            makeWarningWindow()
        }
        return(DF)
    })
    
    # render taxa list from input file
    output$checkTax <- DT::renderDataTable({
        req(input$taxFile)
        fileTable <- getTaxaFromFile()
    })

    # * from OMA group ID =====================================================
    createOmaIdOutput <- reactive({
        tableGroup <- readOmaGroup()
        speciesTable <- readOmaSpec()
        if (is.null(input$omaGroupId)) {
            return(NULL)
        } else {
            speciesList <- strsplit(
                tableGroup$V2[tableGroup$V1 == input$omaGroupId], ","
            )
            specCode <- data.frame(OMAcode = speciesList[[1]])
            DF <- merge(
                specCode,
                speciesTable[,c("TaxonID", "ScientificName", "OMAcode")],
                by = "OMAcode",
                all.x = TRUE
            )
        }
        return(DF)
    })
    
    output$omaGroupSpeciesTable <- DT::renderDataTable({
        if (input$inputTyp == "OmaId") {
            speciesTable <- createOmaIdOutput()
        }
    })
    
    output$speciesSelectionOmaGroup <- renderUI({
        if (input$inputTyp == "OmaId") {
            speciesTable <- createOmaIdOutput()
            selectInput(
                inputId = "GroupSpecies",
                label = "Select up to 10 species",
                multiple = TRUE,
                choices = speciesTable$TaxonID,
                selected = speciesTable$TaxonID[1]
            )
        }
    })

    # * from selected taxon NAMES or IDs or standalone OMA ====================
    createTableOutput <- reactive({
        speciesTable <- readOmaSpec()
        DF <- NULL
        if (input$inputTyp == "ncbiID") {
            if (is.null(input$species) == TRUE) {
                DF <- data.table(
                    TaxonmyIDs = "Nothing selected",
                    ScientificNames = "Nothing selected")
            } else {
                if (sum(speciesTable$TaxonID %in% input$species) > 0) {
                    DF <- data.table(
                        TaxonomyIDs = input$species,
                        ScientificNames = unlist(
                            speciesTable$ScientificName[
                                speciesTable$TaxonID %in% input$species]
                        )
                    )
                }
            }
        } else if (input$inputTyp == "speciesName") {
            if (is.null(input$species) == TRUE) {
                DF <- data.table(
                    TaxonmyIDs = "Nothing selected",
                    ScientificNames = "Nothing selected")
            } else {
                if (sum(speciesTable$ScientificName %in% input$species) > 0) {
                    DF <- data.table(
                        TaxonomyIDs = speciesTable$TaxonID[
                            speciesTable$ScientificName %in% input$species],
                        ScientificNames = unlist(input$species)
                    )
                }
            }
        } else {
            DF <- NULL
        }
        return(DF)
    })

    output$speciesTable <- DT::renderDataTable({
        if (input$inputTyp == "omaFile") {
            readTaxMapping()
        } else {
            speciesTable <- createTableOutput()
        }
    })

    # get OmaCode for chosen species ==========================================
    findOmaCode <- function(outputSpecies, speciesTable) {
        if (input$inputTyp == "ncbiID") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies]
        } else if (input$inputTyp == "speciesName") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$ScientificName %in% outputSpecies]
        } else {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies]
        }
        return(omaCode)
    }
    
    # get taxonomy Ids for chosen species ====================================
    findTaxId <- function(outputSpecies, speciesTable) {
        if (input$inputTyp == "ncbiID") {
            omaCode <- speciesTable$TaxonID[
                speciesTable$TaxonID %in% outputSpecies]
        } else if (input$inputTyp == "speciesName") {
            omaCode <- speciesTable$TaxonID[
                speciesTable$ScientificName %in% outputSpecies]
        } else {
            omaCode <- speciesTable$TaxonID[
                speciesTable$TaxonID %in% outputSpecies]
        }
        return(omaCode)
    }
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outAnnoDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outAnnoDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    observe({
        if (length(getOutputPath()) == 0) disable("submit")
    })
    observe({
        if (length(getOutputPath()) > 0) enable("submit")
    })
    
    # run DCC ==================================================================
    
    # collects the datasets of the chossen species
    getDatasets <- function(omdDataDir, inputOmaCode, inputTaxId, path) {
        fileGettingDataset <- paste(python(), "scripts/getGenomes.py")
        system(paste(fileGettingDataset, omdDataDir, inputOmaCode,inputTaxId, path))
        return(0)
    }
    
    # collects the common Oma Groups of the choosen species
    getCommonOmaGroups <- function(omdDataDir, inputOmaCode, inputTaxId, nrMissingSpecies, path, update) {
        fileGettingOmaGroups <- paste(python(), "scripts/gettingOmaGroups.py")
        y <- cat(system(paste(fileGettingOmaGroups, omdDataDir, inputOmaCode, inputTaxId, nrMissingSpecies, path, update), inter = TRUE))
        return(y)
    }
    getOmaGroup <- function(omdDataDir, inputOmaCode, inputTaxId, omaGroupId, path) {
        fileGettingOmaGroup <- paste(python(), "scripts/gettingOmaGroup.py")
        y <- cat(system(paste(fileGettingOmaGroup, omdDataDir, inputOmaCode, inputTaxId, omaGroupId, path)))
        return(y)
    }
    
    startPythonScript <- observeEvent(input$submit, {
        disable("submit")
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        on.exit(progress$close())

        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, detail = detail)
        }
        
        path <- getOutputPath()
        
        if (input$inputTyp == 'omaFile') {
            # * parse standalone OMA ==========================================
            parseOmaCmd <- paste(
                python(), "scripts/orthoxmlParser.py",
                paste(omaParserOptions(), collapse = " "),
                "-l", 5
            )
            print(parseOmaCmd)
            system(parseOmaCmd, intern = TRUE)
        } else {
            if (input$inputTyp == "inputFile") {
                inFile <- input$taxFile
                taxa <- fread(inFile$datapath, header = FALSE)
                speciesInput <- taxa$V1
            } else if (input$inputTyp == "OmaId") {
                speciesInput <- input$GroupSpecies
            } else {
                speciesInput <- input$species
            }
            
            taxTable <- readOmaSpec()
            OmaCodes <- findOmaCode(speciesInput, taxTable)
            taxIds <- findTaxId(speciesInput, taxTable)
            inputOmaCode <- gsub(" ", "", toString(OmaCodes))
            inputTaxId <- gsub(" ", "", toString(taxIds))
            
            # * get data set ======================================================
            getDatasets(getOmaPath(), inputOmaCode, inputTaxId, path)
            updateProgress(
                detail = paste(
                    "Gene set collection is finished.",
                    "Start to compile Oma Groups"
                )
            )
            
            # * get oma groups ====================================================
            if (input$inputTyp == "OmaId") {
                y <- getOmaGroup(
                    getOmaPath(), inputOmaCode, inputTaxId, input$omaGroupId, path
                )
            } else {
                y <- getCommonOmaGroups(
                    getOmaPath(), inputOmaCode, inputTaxId, input$nrMissingSpecies, 
                    path, input$update
                )
            }
            updateProgress(
                detail = paste(
                    "OMA groups collection is finished.",
                    "Start to compile MSA using", input$MSA
                )
            )
            
            # * create sequence alignments ========================================
            if (input$MSA == "MAFFT") {
                fileMSA <- paste(python(), "scripts/makingMsaMafft.py")
            } else {
                fileMSA <- paste(python(), "scripts/makingMsaMuscle.py")
            }
            updateProgress(detail = y)
            system(paste(fileMSA, path), intern = TRUE)
            
            # * create pHMMs ======================================================
            fileHmm <- paste(python(), "scripts/makingHmms.py")
            updateProgress(detail = "Computing pHMMs with HMMER")
            system(paste(fileHmm, path), intern = TRUE)
            
            #* create BLASTDB =====================================================
            fileBlastDb <- paste(python(), "scripts/makingBlastdb.py")
            updateProgress(detail = "Computing blastDBs")
            system(paste(fileBlastDb, path), inter = TRUE)
            
        }
        
        output$end <- renderUI(
            strong("The calculation is finished!")
        )
    })

    # finishing msg ==========================================================
    output$annoOptions.ui <- renderUI({
        req(input$submit)
        paste("Your output is saved under: ", getOutputPath())
    })
}