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

            conditionalPanel(
                condition = "output.checkPython == 0", ns = ns,
                # type of inputs
                radioButtons(
                    ns("inputTyp"), "Select your input type",
                    choices = list(
                        "NCBI Taxonomy ID" = "ncbiID",
                        "Scientific name" = "speciesName",
                        "File input" = "inputFile",
                        "OMA Group ID" = "OmaId"
                    ),
                    selected = "ncbiID"
                ),
                uiOutput(ns("omaSpec")),
                
                # No. of missing species allowed a common OmaGroup
                uiOutput(ns("nrMissingSpecies")),
                
                # update mode or the normal mode should be used
                uiOutput(ns("update")),
                conditionalPanel(
                    condition = "input.inputTyp != 'OmaId'", ns = ns,
                    checkboxInput(ns("update"), label = "update Mode")
                ),
                
                # MSA tool selection
                radioButtons(
                    ns("MSA"),
                    "MSA tool", choices = c("MAFFT", "MUSCLE"), selected = "MAFFT"
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
            )
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            
            conditionalPanel(
                condition = "output.checkPython == 1", ns = ns,
                htmlOutput(ns("pythonMsg"))
            ),

            conditionalPanel(
                condition = "output.checkPython == 0", ns = ns,
                # oma version
                verbatimTextOutput(ns("version")),
                
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
                
                # finishing msg
                uiOutput(ns("annoOptions.ui")),
                uiOutput(ns("end"))
            )
        )
    )
}

dccApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # check python version ====================================================
    output$checkPython <- reactive({
        python <- suppressWarnings(system("python -V", intern = TRUE))
        if (python < "Python 3") {
            return(1)
        } else return(0)
    })
    outputOptions(output, "checkPython", suspendWhenHidden = FALSE)
    
    output$pythonMsg <- renderText({
        python <- suppressWarnings(system("python -V", intern = TRUE))
        paste(
            "<font color=\"#FF0000\"><b><p>", python, 
            " was found, while DCC requires Python3! </p>",
            "<p>Please check again!</p>", "</b></font>"
        )
    })
    
    # render oma version =======================================================
    output$version <- renderPrint({
        y <- cat(system(paste("python scripts/getVersion.py"), intern = TRUE))
    })

    # load the oma-species file from OmaDb ====================================
    readOmaSpec <- reactive({
        taxTable <- fread(
            "data/oma-species.txt", header = FALSE, skip = 2, sep = "\t"
        )
        colnames(taxTable) <- c(
            "OMAcode", "TaxonID", "ScientificName", "GenomeSource",
            "Version/Release"
        )
        return(taxTable)
    })

    # load the transformed oma-groups-tmp.txt =================================
    readOmaGroup <- reactive({
        groupTable <- fread("data/oma-groups-tmp.txt")
        return(groupTable)
    })

    # render list of taxa info based on selected input type ====================
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

    # render number of allowed missing species =================================
    output$nrMissingSpecies <- renderUI({
        if (input$inputTyp != "OmaId") {
            if (input$inputTyp != "inputFile") {
                selectInput(
                    inputId = ns("nrMissingSpecies"),
                    label = "How many species can be missed in an OmaGroup",
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
                        inputId = ns("nrMissingSpecies"),
                        label = "How many species can be missed in an OmaGroup",
                        choices = seq(0,(nrow(taxaInFile)-1))
                    )
                }
            }
        }
    })

    # create list of taxa from input FILE ======================================
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

    # create list of taxa from input OMA group ID ==============================
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

    # create list of taxa from selected taxon NAMES or IDs =================
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
        speciesTable <- createTableOutput()
    })
    
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
        taxIds <-    findTaxId(speciesInput, taxTable)
        inputOmaCode <- gsub(" ", "", toString(OmaCodes))
        inputTaxId <- gsub(" ", "", toString(taxIds))
        path <- getOutputPath()

        getDatasets(inputOmaCode, inputTaxId, path)

        updateProgress(
            detail = paste(
                "Dataset collection is finished.",
                "Start to compile commonOmaGroups"
            )
        )

        if (input$inputTyp == "OmaId") {
            y <- getOmaGroup(inputOmaCode, inputTaxId, path)
        } else {
            y <- getCommonOmaGroups(inputOmaCode, inputTaxId, path)
        }

        if (input$MSA == "MAFFT") {
            fileMSA <- "python scripts/makingMsaMafft.py"
        } else {
            fileMSA <- "python scripts/makingMsaMuscle.py"
        }
        updateProgress(detail = y)
        system(paste(fileMSA, path), intern = TRUE)

        fileHmm <- "python scripts/makingHmms.py"
        updateProgress(detail = "Computing hMMs with HMMER")
        system(paste(fileHmm, path), intern = TRUE)

        fileBlastDb <- "python scripts/makingBlastdb.py"
        updateProgress(detail = "Computing Blastdbs")
        system(paste(fileBlastDb, path), inter = TRUE)

        output$end <- renderText(
            paste("The calculation is finished!", path, y)
        )
    })

    # output msg ==========================================================
    output$annoOptions.ui <- renderUI({
        req(input$submit)
        paste("Your output will be saved under: ", getOutputPath())
    })
}

# returns the OmaCode of the species chosen from the user
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

# returns the taxonomy Ids from the species choosen from the user
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

# collects the datasets of the chossen species
getDatasets <- function(inputOmaCode, inputTaxId, path) {
    fileGettingDataset <- "python scripts/gettingDataset.py"
    system(paste(fileGettingDataset, inputOmaCode,inputTaxId, path))
    return(0)
}

# collects the common Oma Groups of the choosen species
getCommonOmaGroups <- function(inputOmaCode, inputTaxId, path) {
    fileGettingOmaGroups <- "python scripts/gettingOmaGroups.py"
    y <- cat(system(paste(fileGettingOmaGroups, inputOmaCode, inputTaxId, input$nrMissingSpecies, path, input$update), inter = TRUE))
    return(y)
}
getOmaGroup <- function(inputOmaCode, inputTaxId, path) {
    fileGettingOmaGroup <- "python scripts/gettingOmaGroup.py"
    y <- cat(system(paste(fileGettingOmaGroup, inputOmaCode, inputTaxId, input$omaGroupId, path)))
}
