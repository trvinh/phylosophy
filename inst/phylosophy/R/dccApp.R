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
            
            # type of inputs
            radioButtons(
                ns("inputTyp"), "Select your input mode",
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
            
            # MSA tool selection
            radioButtons(
                ns("MSA"), 
                "MSA tool", choices = c("MAFFT", "MUSCLE"), selected = "MAFFT"
            ),
            
            # ** annoFAS options ===============================================
            br(), 
            shinyDirButton(
                ns("outAnnoDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            hr(),
            
            # creates the action button to run the calculation
            actionButton(ns("submit"), "Run")
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            # creates an output where the user can see whether the version of his oma files are up to date or not
            verbatimTextOutput(ns("version")),
            
            # creates an output for the table where all chosen species can be seen
            DT::dataTableOutput(ns("speciesTable")),
            
            # creates an output table where the user can see which taxons uploaded from a file are available in omaDb
            DT::dataTableOutput(ns("checkTax")),
            
            # creates an output table where the user can see which species are included in the given oma group
            DT::dataTableOutput(ns("omaGroupSpeciesTable")),
            
            # creates an input where the user can select the species he want to collect
            # all species which were selectable are included in the selcted oma group
            uiOutput(ns("speciesSelectionOmaGroup")),
            
            # creates the error message if there were uploaded taxon ids which aren' available in omadb
            span(textOutput(ns("error")), style = "color:red"),
            
            # creates an user output which shows the number of common oma groups
            verbatimTextOutput(ns("nrOmaGroups")),
            uiOutput(ns("end")),
            uiOutput(ns("annoOptions.ui"))
        )
    )
}

dccApp <- function (input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # get input fasta (seed and query) =========================================
    # getSeedPath <- reactive({
    #     shinyFileChoose(
    #         input, "seedInput", roots = homePath, session = session,
    #         filetypes = c('', 'fa', 'fasta')
    #     )
    #     fileSelected <- parseFilePaths(homePath, input$seedInput)
    #     req(input$seedInput)
    #     return(replaceHomeCharacter(as.character(fileSelected$datapath)))
    # })
    
    # loads the oma-species file from OmaDb ######
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
    
    # loads the transformed oma-groups-tmp.txt #######
    readOmaGroup <- reactive({
        groupTable <- fread("data/oma-groups-tmp.txt")
        return(groupTable)
    })
    
    # returns the OmaCode of the species chosen from the user #######
    findOmaCode <- function(outputSpecies, speciesTable) {
        if (input$inputTyp == "ncbiID") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies
            ]
        } else if (input$inputTyp == "speciesName") {
            omaCode <- speciesTable$OMAcode[
                speciesTable$ScientificName %in% outputSpecies
            ]
        } else {
            omaCode <- speciesTable$OMAcode[
                speciesTable$TaxonID %in% outputSpecies
            ]
        }
        return(omaCode)
    }
    
    # returns the taxonomy Ids from the species choosen from the user ######
    findTaxId <- function(outputSpecies, speciesTable) {
        if (input$inputTyp == "ncbiID") {
            omaCode <- speciesTable$TaxonID[
                speciesTable$TaxonID %in% outputSpecies
            ]
        } else if (input$inputTyp == "speciesName") {
            omaCode <- speciesTable$TaxonID[
                speciesTable$ScientificName %in% outputSpecies
            ]
        } else {
            omaCode <- speciesTable$TaxonID[
                speciesTable$TaxonID %in% outputSpecies
            ]
        }
        return(omaCode)
    }
    
    # render list of taxa info based on selected input type #######
    output$omaSpec <- renderUI({
        omaSpecTable <- readOmaSpec()
        omaGroupTable <- readOmaGroup()
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
    
    # number of missing species allowed #########
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
                if (is.null(inFile)) {
                    taxaInFile <- 1
                    selectInput(
                        inputId = ns("nrMissingSpecies"),
                        label = "How many species can be missed in an OmaGroup",
                        choices = 0
                        
                    )
                } else {
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
    
    output$update <- renderUI({
        if (input$inputTyp != "OmaId") {
            checkboxInput(ns("update"), label = "update Mode")
        }
    })
    
    
    ######## check oma version ######
    output$version <-  renderPrint({
        y <- cat(system(paste("python scripts/getVersion.py"), intern = TRUE))
    })
    
    ####### warning if something is wrong #######
    makeWarningWindow <- reactive({
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
    
    disableButton <- observeEvent(input$shinyalert,{
        if (length(input$shinyalert) > 0) {
            if (input$shinyalert == FALSE) {
                disable("submit")
            }
        }
    })
    
    ###############
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
            return(DF)}
    })
    
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outAnnoDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outAnnoDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
  
    ######## RENDER TABLES #######
    
    # checks if the taxonomy ids of the file input is available in oma
    # returns a error if there is a not available taxon id
    # run button will than be disabled <- has to be changed, should ignore nr which aren't in oma
    createFileOutput <- reactive({
        DF <- getFileTable()
        notAvailableTaxons <- DF$TaxonID[is.na(DF$ScientificName)]
        DF$ScientificName[is.na(DF$ScientificName)] <- "not available yet"
        if (length(notAvailableTaxons) > 0) {
            makeWarningWindow()
        }
        return(DF)
    })
    
    # creates a output table if the user chooses to only input an oma group
    # all species which are included in this choosen oma group will be available in this produced table
    # afterwards the user can choose which species should be collected
    createOmaIdOutput <- reactive({
        tableGroup <- readOmaGroup()
        speciesTable <- readOmaSpec()
        if (is.null(input$omaGroupId)) {
            return(NULL)
        } else {
            speciesList <- strsplit(tableGroup$V2[tableGroup$V1 == input$omaGroupId], ",")
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
    
    # created a table which represends the species choosen from the user in mode ScientificName or TaxnonID
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
                        ScientificNames = unlist(speciesTable$ScientificName[speciesTable$TaxonID %in% input$species])
                    )
                }
            }
        } else if (input$inputTyp == "speciesName") {
            if (is.null(input$species) == TRUE) {
                DF <- data.table(
                    TaxonmyIDs = "Nothing selected",
                    ScientificNames = "Nothing selected")
            }
            else {
                if (sum(speciesTable$ScientificName %in% input$species) > 0) {
                    DF <- data.table(
                        TaxonomyIDs = speciesTable$TaxonID[speciesTable$ScientificName %in% input$species],
                        ScientificNames = unlist(input$species)
                    )}
            }
        } else {
            DF <- NULL
        }
        return(DF)
    })

    output$speciesTable <- DT::renderDataTable({
        # creates the species table output seen by the user
        speciesTable <- createTableOutput()
    })
    
    output$checkTax <- DT::renderDataTable({
        # creates the shiny output of the table with the species from the file input
        fileTable <- createFileOutput()
    })
    
    output$omaGroupSpeciesTable <- DT::renderDataTable({
        # creates the shiny output of the table which includes the species from the OmaGroup the user selected
        if (input$inputTyp == "OmaId") {
            speciesTable <- createOmaIdOutput()
        }
    })
    
    output$speciesSelectionOmaGroup <- renderUI({
        # creates a user input, where the user can choose the species which are included in the choosen OmaGroup
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
    
    ######## run DCC ###########
    startPythonScript <- observeEvent(input$submit, {
        # runs the calculation of the dataset collection, the collection of common oma groups and the MSAs, hMMS, Blastdbs...
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
        taxIds <-  findTaxId(speciesInput, taxTable)
        inputOmaCode <- gsub(" ", "", toString(OmaCodes))
        inputTaxId <- gsub(" ", "", toString(taxIds))
        path <- getOutputPath() #readDirectoryInput(session, 'directory')
        
        getDatasets(inputOmaCode, inputTaxId, path)
        
        updateProgress(detail = "Dataset collection is finished. Start to compile commonOmaGroups")
        
        if (input$inputTyp == "OmaId") {
            #print("OmaId")
            y <- getOmaGroup(inputOmaCode, inputTaxId, path)
        } else {
            y <-  getCommonOmaGroups(inputOmaCode, inputTaxId, path)
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
        
        output$end <- renderText(paste("The calculation is finished. Your output is saved under: ", path, y))
    })
    
    # output ==========================================================
    annoOptions <- reactive({
        fasta <- ""
        # fasta <- paste0("--fasta=", getSeedPath())
        path <- ""
        if (length(getOutputPath()) > 0)
            path <- paste0("--path=", getOutputPath())
        
        annoOption <- c(fasta, path)
        
        return(
            annoOption[unlist(lapply(annoOption, function (x) x != ""))]
        )
    })
    
    output$annoOptions.ui <- renderUI({
        paste("Your output will be saved under: ", getOutputPath())
    })
    
    # 
}

# this function runs the python script which collects the datasets of the chossen species
getDatasets <- function(inputOmaCode, inputTaxId, path) {
    fileGettingDataset <- "python scripts/gettingDataset.py"
    system(paste(fileGettingDataset, inputOmaCode,inputTaxId, path))
    return(0)
}

# this function runs the phyton script which collects the common Oma Groups of the choosen species
getCommonOmaGroups <- function(inputOmaCode, inputTaxId, path) {
    fileGettingOmaGroups <- "python scripts/gettingOmaGroups.py"
    y <- cat(system(paste(fileGettingOmaGroups, inputOmaCode, inputTaxId, input$nrMissingSpecies, path, input$update), inter = TRUE))
    return(y)
}

getOmaGroup <- function(inputOmaCode, inputTaxId, path) {
    fileGettingOmaGroup <- "python scripts/gettingOmaGroup.py"
    y <- cat(system(paste(fileGettingOmaGroup, inputOmaCode, inputTaxId, input$omaGroupId, path)))
}

