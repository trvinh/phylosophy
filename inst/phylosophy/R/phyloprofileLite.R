#' PhyloProfile module
library(PhyloProfile)

phyloprofileLiteUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        wellPanel(
            fluidRow(
                column(
                    2,
                    radioButtons(
                        inputId = ns("xAxis"),
                        label = "Choose type of x-axis:",
                        choices = list("taxa", "genes"),
                        selected = "taxa",
                        inline = TRUE
                    ),
                    actionButton(ns("mainPlotConfig"), "Appearance")
                ),
                column(
                    1,
                    createPlotSize(ns("width"), "Width (px)", 600)
                ),
                column(
                    1, createPlotSize(ns("height"), "Height (px)", 600)
                ),
                column(
                    2, uiOutput(ns("var1Cutoff.ui"))
                ),
                column(
                    2, uiOutput(ns("var2Cutoff.ui"))
                ),
                column(
                    2, uiOutput(ns("percentCutoff.ui"))
                ),
                column(
                    2,
                    numericInput(
                        ns("coortholog"),
                        "Max co-orthologs",
                        min = 1,
                        max = 999,
                        step = 1,
                        value = 999,
                        width = 150
                    ),
                    bsButton(
                        ns("resetMain"),
                        "Reset cutoffs",
                        style = "danger",
                        icon = icon("backward")
                    )
                )
            )
        ),
        sidebarLayout(
            # * sidebar panel for input/options -----------------
            sidebarPanel(
                width = 3,
                strong("Input files"),
                fileInput(ns("mainInput"), "Upload phyloprofile input:"),
                fileInput(ns("fileDomainInput"), "Upload domain file:"),
                hr(),

                strong("Set variable names"),
                fluidRow(
                    column(
                        6, uiOutput(ns("var1ID.ui"))
                    ),
                    column(
                        6, uiOutput(ns("var2ID.ui"))
                    )
                ),

                conditionalPanel(
                    condition = "output.unkTaxaStatus == 0", ns = ns,
                    strong("Seed (super)taxon:"),
                    uiOutput(ns("rankSelect.ui")),
                    uiOutput(ns("taxSelect.ui")),
                    bsButton(
                        ns("do"),
                        "PLOT",
                        type = "action",
                        style = "success",
                        disabled = FALSE
                    )
                )
            ),
            # * main panel for FAS run ----------------------------
            mainPanel(
                width = 9,
                conditionalPanel(
                    condition = "output.unkTaxaStatus == 1", ns = ns,
                    # uiOutput(ns("unkMsg.ui"))
                    # strong("PLEASE USE THE FULL VERSION OF PHYLOPROFILE")
                    htmlOutput(ns("unkMsg")),
                    DT::dataTableOutput(ns("unkTaxaFull")),
                    tags$head(tags$style("#unkTaxaFull{color: red;
                                 font-size: 20px;
                                         font-style: italic;}"
                         )
                    )
                ),
                conditionalPanel(
                    condition = "input.do > 0", ns = ns,
                    uiOutput(ns("plot.ui")),
                    br(),
                    downloadButton(ns("profileDownload"),"Download profile",
                                   class = "butDL"),
                    tags$head(
                        tags$style(HTML(
                            ".butDL{background-color:#476ba3;} .butDL{color: white;}"))
                    )
                )
            )
        ),
        
        # * popup for setting Main plot configurations -------------------------
        bsModal(
            "mainPlotConfigBs",
            "Plot appearance configuration",
            ns("mainPlotConfig"),
            size = "small",
            column(
                6, createTextSize(ns("xSize"), "X-axis label size (px)", 8, 100)
            ),
            column(
                6, createTextSize(ns("ySize"), "Y-axis label size (px)", 8, 100)
            ),
            column(
                6,
                createTextSize(ns("legendSize"), "Legend label size (px)", 8, 150)
            ),
            column(
                6,
                selectInput(
                    ns("mainLegend"), label = "Legend position:",
                    choices = list("Right" = "right",
                                   "Left" = "left",
                                   "Top" = "top",
                                   "Bottom" = "bottom",
                                   "Hide" = "none"),
                    selected = "right",
                    width = 150
                )
            ),
            column(
                12,
                HTML("<strong>Angle for x-axis label</strong>:<br>"),
                sliderInput(
                    ns("xAngle"),
                    "",
                    min = 0,
                    max = 90,
                    step = 10,
                    value = 60,
                    width = 250
                )#,
                # br()
            ),
            column(
                12,
                HTML("<strong>Zooming factor (α) for dots on
                     profile</strong>:<br>"),
                sliderInput(
                    ns("dotZoom"), "",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 0,
                    width = 250
                ),
                HTML("<em>dot size = (1+α)*defaultSize<br>defaultSize
                     =[0:5]</em>"),
                uiOutput(ns("dotSizeInfo")),
                br()
            ),
            column(
                12,
                HTML("<strong>Change default colors</strong>:<br>"),
                actionButton(ns("setColor"), "Set color")
            )
        ),
        
        # * popup for setting plot colors (profiles) ---------------------------
        bsModal(
            "color",
            "Set colors for profile",
            ns("setColor"),
            size = "small",
            colourpicker::colourInput(
                ns("lowColorVar1"),
                "Low variable 1 (dot)",
                value = "darkorange"
            ),
            colourpicker::colourInput(
                ns("highColorVar1"),
                "High variable 1 (dot)",
                value = "steelblue"
            ),
            actionButton(
                ns("defaultColorVar1"),
                "Default",
                style = "padding:4px; font-size:100%"
            ),
            hr(),
            colourpicker::colourInput(
                ns("lowColorVar2"),
                "Low variable 2 (background)",
                value = "grey95"
            ),
            colourpicker::colourInput(
                ns("highColorVar2"),
                "High variable 2 (background)",
                value = "khaki"
            ),
            actionButton(
                ns("defaultColorVar2"),
                "Default",
                style = "padding:4px; font-size:100%"
            ),
            hr(),
            colourpicker::colourInput(
                ns("paraColor"),
                "Color for inparalogs",
                value = "#07d000"
            ),
            actionButton(
                ns("defaultColorPara"),
                "Default",
                style = "padding:4px; font-size:100%"
            )
        ),
        
        # POINT INFO BOX =======================================================
        absolutePanel(
            bottom = 5, left = 30,
            fixed = TRUE,
            draggable = TRUE,
            h5("Point's info:"),
            verbatimTextOutput(ns("pointInfo")),
            style = "opacity: 0.80"
        )
    )
}

phyloprofileLite <- function(input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    # * check the status of unkTaxa --------------------------------------------
    unkTaxa <- reactive({
        withProgress(message = 'Checking for unknown taxa...', value = 0.5, {
            longDataframe <- getMainInput()
            req(longDataframe)
            
            pathToPhyloprofile <- paste0(
                path.package("PhyloProfile"), "/PhyloProfile"
            )
            rankListFile <- paste0(pathToPhyloprofile, "/data/rankList.txt")
            nameFullFile <- paste0(
                pathToPhyloprofile, "/data/preProcessedTaxonomy.txt"
            )
            
            inputTaxa <- levels(longDataframe$ncbiID)
            inputTaxa <- unlist(strsplit(inputTaxa, split = "\t"))
            
            if (inputTaxa[1] == "geneID") {
                # remove "geneID" element from vector inputTaxa
                inputTaxa <- inputTaxa[-1]
            }
            
            if (!file.exists(isolate(rankListFile))) {
                return(inputTaxa)
            } else {
                info <- file.info(rankListFile)
                if (info$size == 0) {
                    return(inputTaxa)
                } else {
                    # rankListFile <- paste0(getwd(), "/data/rankList.txt")
                    allTaxa <- as.factor(
                        unlist(
                            data.table::fread(file = rankListFile, select = 1)
                        )
                    )
                    
                    # list of unknown taxa
                    unkTaxa <- inputTaxa[!(inputTaxa %in% allTaxa)]
                    if (identical(unkTaxa, character(0))) return()
                    
                    # get non-ncbi taxa
                    unkTaxa <- data.frame(TaxonID = unkTaxa)
                    unkTaxa$id <- substring(unkTaxa$TaxonID, 5)
                    unkTaxa$Source <- "ncbi"
                    
                    ncbiTaxa <- as.factor(
                        unlist(
                            data.table::fread(file = nameFullFile, select = 1)
                        )
                    )
                    
                    ncbiID <- levels(ncbiTaxa)
                    maxNCBI <- max(sort(as.numeric(ncbiID[ncbiID != "ncbiID"])))
                    
                    if (nrow(unkTaxa[!(unkTaxa[,"id"] %in% ncbiTaxa),]) > 0) {
                        unkTaxaId <- unkTaxa[!(unkTaxa$id %in% ncbiTaxa),]$id
                        unkTaxa[unkTaxa$id %in% unkTaxaId,]$Source <- "unknown"
                        if (any(unkTaxaId < maxNCBI)) {
                            unkTaxa[
                                unkTaxa$id %in% unkTaxaId 
                                & unkTaxa$id < maxNCBI,]$Source <- "invalid"
                        }
                    }
                    
                    # return list of unkTaxa
                    return(unkTaxa)
                }
            }
        })
    })
    
    output$unkTaxaStatus <- reactive({
        unkTaxa <- unkTaxa()
        if (length(unkTaxa) > 0) {
            return(1)
        } else return(0)
    })
    outputOptions(output, "unkTaxaStatus", suspendWhenHidden = FALSE)
    
    output$unkMsg <- renderText({
        msg <- paste(
            "Invalid taxa found in your input file.",
            "Please use the full version of PhyloProfile!"
        )
        paste("<font color=\"#FF0000\"><b>", msg, "</b></font>")
    })

    output$unkTaxaFull <- DT::renderDataTable(
        options = list(searching = FALSE, pageLength = 10),{
            if (length(unkTaxa()) > 0) {
                tb <- unkTaxa()
                tb[, c("TaxonID", "Source")]
            }
        }
    )
    
    # * check if data is loaded and "plot" button is clicked -------------------
    v <- reactiveValues(doPlot = FALSE)
    observeEvent(input$do, {
        v$doPlot <- input$do
        filein <- input$mainInput
        if (is.null(filein)) {
            v$doPlot <- FALSE
            updateButton(session, ns("do"), disabled = TRUE)
        }
    })
    
    # * get main input ---------------------------------------------------------
    getMainInput <- reactive({
        withProgress(message = 'Reading main input...', value = 0.5, {
            filein <- input$mainInput
            if (is.null(filein)) return()
            inputType <- checkInputValidity(filein$datapath)
            if (inputType == "oma") {
                if (input$getDataOma[1] == 0) return()
                longDataframe <- createProfileFromOma(finalOmaDf())
                longDataframe <- as.data.frame(unclass(longDataframe))
            } else longDataframe <- createLongMatrix(filein$datapath)
            
            # convert geneID, ncbiID and orthoID into factor and
            # var1, var2 into numeric
            for (i in seq_len(3)) {
                longDataframe[, i] <- as.factor(longDataframe[, i])
            }
            if (ncol(longDataframe) > 3) {
                for (j in seq(4, ncol(longDataframe))){
                    longDataframe[,j] <- suppressWarnings(
                        as.numeric(as.character(longDataframe[,j]))
                    )
                }
            }
            
            # remove duplicated lines
            longDataframe <- longDataframe[!duplicated(longDataframe),]
            return(longDataframe)
        })
    })
    
    # * get domain input -------------------------------------------------------
    getDomainInformation <- reactive({
        withProgress(message = 'Reading domain input...', value = 0.5, {
            # if (v$doPlot == FALSE) return()
            mainInput <- getMainInput()
            inputDomain <- input$fileDomainInput
            domainDf <- parseDomainInput(
                NULL,
                inputDomain$datapath,
                "file"
            )
    print(head(domainDf))
            return(domainDf)
        })
    })
    
    # * get ID list of input taxa from main input ------------------------------
    inputTaxonID <- reactive({
        if (length(unkTaxa()) == 0) {
            withProgress(message = 'Getting input taxon IDs...', value = 0.5, {
                longDataframe <- getMainInput()
                inputTaxa <- getInputTaxaID(longDataframe)
            })
        } else return()
    })
    
    # * get NAME list of all (super)taxa ---------------------------------------
    inputTaxonName <- reactive({
        req(input$rankSelect)
        if (is.null(input$mainInput)) return()
        if (length(unkTaxa()) > 0) return()
        if (input$rankSelect == "") return()
        withProgress(message = 'Getting input taxon names...', value = 0.5, {
            inputTaxaName <- getInputTaxaName(input$rankSelect, inputTaxonID())
            return(inputTaxaName)
        })
    })
    
    
    # * render textinput for Variable 1 & 2 ------------------------------------
    output$var1ID.ui <- renderUI({
        longDataframe <- getMainInput()
        if (is.null(longDataframe)) {
            textInput(
                ns("var1ID"),
                h5("1st variable:"),
                value = "Variable 1",
                width = "100%",
                placeholder = "Name of first variable"
            )
        } else {
            textInput(
                ns("var1ID"), h5("1st variable:"),
                value = colnames(longDataframe)[4],
                width = "100%",
                placeholder = "Name of first variable"
            )
        }
    })
    
    output$var2ID.ui <- renderUI({
        longDataframe <- getMainInput()
        if (is.null(longDataframe)) {
            textInput(
                ns("var2ID"),
                h5("2st variable:"),
                value = "Variable 2",
                width = "100%",
                placeholder = "Name of second variable"
            )
        } else {
            textInput(
                ns("var2ID"), h5("2st variable:"),
                value = colnames(longDataframe)[5],
                width = "100%",
                placeholder = "Name of second variable"
            )
        }
    })
    
    # * render filter slidebars for Main plot ----------------------------------
    output$var1Cutoff.ui <- renderUI({
        createSliderCutoff(
            ns("var1"), paste(input$var1ID, "cutoff:"), 0.0, 1.0, input$var1ID
        )
    })
    
    output$var2Cutoff.ui <- renderUI({
        createSliderCutoff(
            ns("var2"), paste(input$var2ID, "cutoff:"), 0.0, 1.0, input$var2ID
        )
    })
    
    output$percentCutoff.ui <- renderUI({
        createSliderCutoff(
            ns("percent"), "% of present taxa:", 0.0, 1.0, "percent"
        )
    })
    
    # * reset cutoffs of Main plot ---------------------------------------------
    observeEvent(input$resetMain, {
        shinyjs::reset("var1")
        shinyjs::reset("var2")
        shinyjs::reset("percent")
        shinyjs::reset("coortholog")
    })
    
    # * render list of taxonomy ranks ------------------------------------------
    output$rankSelect.ui <- renderUI({
        selectInput(
            ns("rankSelect"), label = "Select taxonomy rank:",
            choices = getTaxonomyRanks(),
            selected = "species"
        )
    })
    
    # * render list of (super)taxa ---------------------------------------------
    output$taxSelect.ui <- renderUI({
        choice <- inputTaxonName()
        choice$fullName <- as.factor(choice$fullName)
        selectInput(
            ns("inSelect"), "Choose (super)taxon of interest:",
            as.list(levels(choice$fullName)),
            levels(choice$fullName)[1]
        )
    })
    
    # # * enable "PLOT" button ---------------------------------------------------
    # observeEvent(input$rankSelect,  ({
    #     if (input$rankSelect == "") updateButton(session, ns("do"), disabled = TRUE)
    #     else {
    #         unkTaxa <- unkTaxa()
    #         if (length(unkTaxa) == 0) {
    #             updateButton(session, ns("do"), disabled = FALSE)
    #         }
    #     }
    # }))
    
    # * sort taxonomy data of input taxa ---------------------------------------
    sortedtaxaList <- reactive({
        req(v$doPlot)
        withProgress(message = 'Sorting input taxa...', value = 0.5, {
            # get input taxonomy tree
            inputTaxaTree <- NULL
            treeIn <- input$inputTree
            if (!is.null(treeIn)) {
                inputTaxaTree <- read.tree(file = treeIn$datapath)
            }
            
            # sort taxonomy matrix based on selected refTaxon
            sortedOut <- sortInputTaxa(
                taxonIDs = inputTaxonID(),
                rankName = input$rankSelect,
                refTaxon = input$inSelect,
                taxaTree = inputTaxaTree
            )
            # return
            return(sortedOut)
        })
    })
    
    # * get subset data (default: first 30 genes) for plotting -----------------
    preData <- reactive({
        req(v$doPlot)
        # isolate start and end gene index
        # input$updateBtn
        # 
        # if (input$autoUpdate == TRUE) {
        #     startIndex <- input$stIndex
        #     endIndex <- input$endIndex
        # } else {
        #     startIndex <- isolate(input$stIndex)
        #     endIndex <- isolate(input$endIndex)
        # }
        # if (is.na(endIndex)) endIndex <- 30
        
        longDataframe <- getMainInput()
        req(longDataframe)
        withProgress(message = 'Subseting data...', value = 0.5, {
            # longDataframe <- unsortID(longDataframe, input$ordering)
            # listIn <- input$list
            # if (!is.null(listIn)) {
            #     list <- read.table(file = listIn$datapath, header = FALSE)
            #     listGeneOri <- list$V1
            #     if (startIndex <= length(listGeneOri)) {
            #         listGene <- listGeneOri[listGeneOri[startIndex:endIndex]]
            #     } else listGene <- listGeneOri
            #     data <- longDataframe[longDataframe$geneID %in% listGene, ]
            # } else {
            #     subsetID <-
            #         levels(longDataframe$geneID)[startIndex:endIndex]
            #     data <- longDataframe[longDataframe$geneID %in% subsetID, ]
            # }
            data <- longDataframe
            
            if (ncol(data) < 5) {
                for (i in seq_len(5 - ncol(data))) {
                    data[paste0("newVar", i)] <- 1
                }
            }
            
            # return preData
            if (nrow(data) == 0) return()
            colnames(data) <- c("geneID", "ncbiID", "orthoID", "var1", "var2")
            return(data)
        })
    })
    
    # * creating main dataframe for subset taxa (in species/strain level) ------
    # * get (super)taxa names (1)
    # * calculate percentage of presence (2),
    # * max/min/mean/median VAR1 (3) and VAR2 (4)
    getDataFiltered <- reactive({
        req(v$doPlot)
        req(preData())
        req(sortedtaxaList())
        withProgress(message = 'Parsing profile data...', value = 0.5, {
            fullMdData <- parseInfoProfile(
                inputDf = preData(),
                sortedInputTaxa = sortedtaxaList(),
                var1AggregateBy = "max",
                var2AggregateBy = "max"
            )
            return(fullMdData)
        })
    })
    
    # * reduce data from lowest level to supertaxon (e.g. phylum) --------------
    # * This data set contain only supertaxa
    # * and their value (%present, mVar1 & mVar2) for each gene
    dataSupertaxa <- reactive({
        req(v$doPlot)
        fullMdData <- getDataFiltered()
        withProgress(message = 'Reducing to supertaxon...', value = 0.5, {
            superDfExt <- reduceProfile(fullMdData)
            return(superDfExt)
        })
    })
    
    # * heatmap data input -----------------------------------------------------
    dataHeat <- reactive({
        req(v$doPlot)
        # {
        #     input$plotCustom
        #     input$updateBtn
        # }
        # check input file
        filein <- input$mainInput
        # if (input$demoData == "arthropoda" | input$demoData == "ampk-tor") {
        #     filein <- 1
        # }
        req(filein)
        withProgress(message = 'Creating data for plotting...', value = 0.5, {
            # get all cutoffs
            # if (input$autoUpdate == TRUE) {
                percentCutoff <- input$percent
                coorthologCutoffMax <- input$coortholog
                var1Cutoff <- input$var1
                var2Cutoff <- input$var2
            # } else {
                # percentCutoff <- isolate(input$percent)
                # coorthologCutoffMax <- isolate(input$coortholog)
                # var1Cutoff <- isolate(input$var1)
                # var2Cutoff <- isolate(input$var2)
            # }
            
            # get selected supertaxon name
            split <- strsplit(as.character(input$inSelect), "_")
            inSelect <- as.character(split[[1]][1])
            
            # # get gene categories
            # inputCatDt <- NULL
            # if (input$colorByGroup == TRUE) {
            #     # get gene category
            #     geneCategoryFile <- input$geneCategory
            #     if (!is.null(geneCategoryFile)) {
            #         inputCatDt <- read.table(
            #             file = geneCategoryFile$datapath,
            #             sep = "\t",
            #             header = TRUE,
            #             check.names = FALSE,
            #             comment.char = "",
            #             fill = TRUE
            #         )
            #         colnames(inputCatDt) <- c("geneID","group")
            #     } else inputCatDt <- NULL
            # }
            
            # create data for heatmap plotting
            dataHeat <- filterProfileData(
                DF = dataSupertaxa(),
                refTaxon = inSelect,
                percentCutoff,
                coorthologCutoffMax,
                var1Cutoff,
                var2Cutoff,
                "protein",
                "protein",
                groupByCat = FALSE,
                catDt = NULL
            )
            return(dataHeat)
        })
    })
    
    # ** create profiles for calculating distance matrix -----------------------
    getProfiles <- reactive({
        withProgress(message = 'Getting data for cluster...', value = 0.5, {
            req(dataHeat())
            profiles <- getDataClustering(
                dataHeat(),
                "binary",
                "max",
                "max"
            )
            return(profiles)
        })
    })
    
    # ** calculate distance matrix ---------------------------------------------
    getDistanceMatrixProfiles <- reactive({
        withProgress(message = 'Calculating distance matrix...', value = 0.5, {
            req(dataHeat())
            distanceMatrix <- getDistanceMatrix(getProfiles(), "mutualInformation")
            return(distanceMatrix)
        })
    })
    
    # * clustered heatmap data -------------------------------------------------
    clusteredDataHeat <- reactive({
        req(v$doPlot)
        dataHeat <- dataHeat()
        withProgress(message = 'Clustering profile data...', value = 0.5, {
            dat <- getProfiles()
            # do clustering based on distance matrix
            row.order <- hclust(
                getDistanceMatrixProfiles(), method = input$clusterMethod
            )$order

            # re-order distance matrix accoring to clustering
            datNew <- dat[row.order, ] #col.order

            # return clustered gene ID list
            clusteredGeneIDs <- as.factor(row.names(datNew))

            # sort original data according to clusteredGeneIDs
            dataHeat$geneID <- factor(dataHeat$geneID, levels =clusteredGeneIDs)
            
            dataHeat <- dataHeat[!is.na(dataHeat$geneID),]
            return(dataHeat)
        })
    })
    
    # # * close configuration windows of Main plot -------------------------------
    # observeEvent(input$applyMainConfig, {
    #     toggleModal(session, ns("mainPlotConfigBs"), toggle = "close")
    # })
    
    # * parameters for the main profile plot -----------------------------------
    getParameterInputMain <- reactive({
        inputPara <- list(
            "xAxis" = input$xAxis,
            "var1ID" = input$var1ID,
            "var2ID"  = input$var2ID,
            "lowColorVar1" =  input$lowColorVar1,
            "highColorVar1" = input$highColorVar1,
            "lowColorVar2" =  input$lowColorVar2,
            "highColorVar2" = input$highColorVar2,
            "paraColor" = input$paraColor,
            "xSize" = input$xSize,
            "ySize" = input$ySize,
            "legendSize" = input$legendSize,
            "mainLegend" = input$mainLegend,
            "dotZoom" = input$dotZoom,
            "xAngle" = input$xAngle,
            "guideline" = 1,
            "width" = input$width,
            "height" = input$height,
            "colorByGroup" = FALSE
        )
        return(inputPara)
    })
    
    # * reset profile plot colors ----------------------------------------------
    observeEvent(input$defaultColorVar2, {
        shinyjs::reset("lowColorVar2")
        shinyjs::reset("highColorVar2")
    })
    
    observeEvent(input$defaultColorVar1, {
        shinyjs::reset("lowColorVar1")
        shinyjs::reset("highColorVar1")
    })
    
    observeEvent(input$defaultColorPara, {
        shinyjs::reset("paraColor")
    })
    
    # # * reset configuration windows of Main plot -------------------------------
    # observeEvent(input$resetMainConfig, {
    #     shinyjs::reset("xSize")
    #     shinyjs::reset("ySize")
    #     shinyjs::reset("legendSize")
    #     shinyjs::reset("xAngle")
    #     shinyjs::reset("dotZoom")
    # })
    
    # * render dot size to dotSizeInfo ---------------------------------------
    output$dotSizeInfo <- renderUI({
        req(v$doPlot)
        
        dataHeat <- dataHeat()
        dataHeat$presSpec[dataHeat$presSpec == 0] <- NA
        presentVl <- dataHeat$presSpec[!is.na(dataHeat$presSpec)]
        
        minDot <- (floor(min(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        maxDot <- (floor(max(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        
        em(paste0("current point's size: ", minDot, " - ", maxDot))
    })
    
    # * plot main profile ------------------------------------------------------
    # mainpointInfo <- callModule(
    #     createProfilePlot, ns("mainProfile"),
    #     data = dataHeat,
    #     clusteredDataHeat = clusteredDataHeat,
    #     applyCluster = reactive(input$applyCluster),
    #     parameters = getParameterInputMain,
    #     inSeq = reactive(input$inSeq),
    #     inTaxa = reactive(input$inTaxa),
    #     rankSelect = reactive(input$rankSelect),
    #     inSelect = reactive(input$inSelect),
    #     taxonHighlight = reactive(input$taxonHighlight),
    #     geneHighlight = reactive(input$geneHighlight),
    #     typeProfile = reactive("mainProfile")
    # )
    # data for heatmap ---------------------------------------------------------
    dataPlot <- reactive({
        if (is.null(dataHeat())) stop("Profile data is NULL!")
        
        # if (typeProfile() == "customizedProfile") {
        #     if (is.null(inTaxa()) | is.null(inSeq())) return()
        #     
        #     dataHeat <- dataCustomizedPlot(data(), inTaxa(), inSeq())
        #     if (applyCluster() == TRUE) {
        #         dataHeat <- dataCustomizedPlot(
        #             clusteredDataHeat(), inTaxa(), inSeq()
        #         )
        #     }
        # } else {
            dataPlot <- dataMainPlot(dataHeat())
            # if (applyCluster() == TRUE) {
            #     dataHeat <- dataMainPlot(clusteredDataHeat())
            # }
        # }
        return(dataPlot)
    })
    
    # render heatmap profile ---------------------------------------------------
    output$plot <- renderPlot({
        if (is.null(dataHeat())) stop("Profile data is NULL!")
        # if (typeProfile() == "customizedProfile") {
        #     if (inSeq()[1] == "all" & inTaxa()[1] == "all") return()
        # }
        # print(getParameterInputMain())
        withProgress(message = 'PLOTTING...', value = 0.5, {
            highlightProfilePlot(
                dataPlot(),
                getParameterInputMain(),
                "none",
                input$rankSelect,
                "none"
            )
        })
        
    })
    
    output$plot.ui <- renderUI({
        ns <- session$ns
        
        # if (typeProfile() == "customizedProfile") {
        #     if (is.null(inSeq()[1]) | is.null(inTaxa()[1])) return()
        #     else if (inSeq()[1] == "all" & inTaxa()[1] == "all")  return()
        # }
        
        # shinycssloaders::withSpinner(
        plotOutput(
            ns("plot"),
            width = input$width,
            height = input$height,
            click = ns("plotClick")
        )
        # )
    })
    
    output$profileDownload <- downloadHandler(
        filename = function() {
            c("profile.pdf")
        },
        content = function(file) {
            ggplot2::ggsave(
                file,
                plot = highlightProfilePlot(
                    dataPlot(),
                    getParameterInputMain(),
                    "none",
                    input$rankSelect,
                    "none"
                ),
                
                width = input$width * 0.056458333,
                height = input$height * 0.056458333,
                units = "cm", dpi = 300, device = "pdf", limitsize = FALSE
            )
        }
    )
    
    # get info of clicked point on heatmap plot --------------------------------
    selectedpointInfo <- reactive({
        
        # get selected supertaxon name
        taxaList <- getNameList()
        rankName <- input$rankSelect
        inSelect <- taxaList$ncbiID[taxaList$fullName == input$inSelect]
        
        dataHeat <- dataPlot()
        if (is.null(dataHeat)) stop("Data for heatmap is NULL!")
        
        # if (typeProfile() == "customizedProfile") {
        #     # get sub-dataframe of selected taxa and sequences
        #     dataHeat$supertaxonMod <- substr(
        #         dataHeat$supertaxon,
        #         6,
        #         nchar(as.character(dataHeat$supertaxon))
        #     )
        #     
        #     if (is.null(inSeq()[1]) | is.null(inTaxa()[1]))  
        #         stop("Subset taxa or genes is NULL!")
        #     if (inTaxa()[1] == "all" & inSeq()[1] != "all") {
        #         # select data from dataHeat for selected sequences only
        #         dataHeat <- subset(dataHeat, geneID %in% inSeq())
        #     } else if (inSeq()[1] == "all" & inTaxa()[1] != "all") {
        #         # select data from dataHeat for selected taxa only
        #         dataHeat <- subset(dataHeat, supertaxonMod %in% inTaxa())
        #     } else {
        #         # select data from dataHeat for selected sequences and taxa
        #         dataHeat <- subset(dataHeat, geneID %in% inSeq()
        #                            & supertaxonMod %in% inTaxa())
        #     }
        #     
        #     # drop all other supertaxon that are not in sub-dataframe
        #     dataHeat$supertaxon <- factor(dataHeat$supertaxon)
        #     dataHeat$geneID <- factor(dataHeat$geneID)
        # }
        
        # get values
        if (is.null(input$plotClick$x)) return()
        else {
            # get cooridiate point
            if (input$xAxis == "genes") {
                corX <- round(input$plotClick$y);
                corY <- round(input$plotClick$x)
            } else {
                corX <- round(input$plotClick$x);
                corY <- round(input$plotClick$y)
            }
            
            # get geneID
            genes <- levels(dataHeat$geneID)
            geneID <- toString(genes[corY])
            # get supertaxon (spec)
            supertaxa <- levels(dataHeat$supertaxon)
            spec <- toString(supertaxa[corX])
            # get var1, percentage of present species and var2 score
            var1 <- NA
            if (!is.na(dataHeat$var1[dataHeat$geneID == geneID
                                     & dataHeat$supertaxon == spec][1])) {
                var1 <- max(
                    na.omit(dataHeat$var1[dataHeat$geneID == geneID
                                          & dataHeat$supertaxon == spec])
                )
            }
            Percent <- NA
            if (!is.na(dataHeat$presSpec[dataHeat$geneID == geneID
                                         & dataHeat$supertaxon == spec][1])) {
                Percent <- {
                    max(
                        na.omit(
                            dataHeat$presSpec[dataHeat$geneID == geneID
                                              & dataHeat$supertaxon == spec]
                        )
                    )
                }
            }
            var2 <- NA
            if (!is.na(dataHeat$var2[dataHeat$geneID == geneID
                                     & dataHeat$supertaxon == spec][1])) {
                var2 <- {
                    max(na.omit(dataHeat$var2[dataHeat$geneID == geneID
                                              & dataHeat$supertaxon == spec]))
                }
            }
            
            # get ortholog ID
            orthoID <- dataHeat$orthoID[dataHeat$geneID == geneID
                                        & dataHeat$supertaxon == spec]
            if (length(orthoID) > 1) {
                orthoID <- paste0(orthoID[1], ",...")
            }
            
            if (is.na(Percent)) return()
            else {
                info <- c(geneID,
                          as.character(orthoID),
                          spec,
                          round(var1, 2),
                          round(Percent, 2),
                          round(var2, 2))
                return(info)
            }
        }
    })
    
    # * show info into "point's info" box --------------------------------------
    output$pointInfo <- renderText({
        # GET INFO BASED ON CURRENT TAB
        # if (input$tabs == "Main profile") {
            # info contains groupID,orthoID,supertaxon,mVar1,%spec,var2
            info <- selectedpointInfo()
        # } else if (input$tabs == "Customized profile") {
        #     info <- selectedpointInfo()
        # } else return()
        
        req(info)
        orthoID <- info[2]
        
        if (is.na(orthoID)) return()
        else {
            a <- toString(paste("Seed-ID:", info[1]))
            b <- toString(paste0(
                "Hit-ID: ", orthoID,
                " (", info[3], ")"
            ))
            c <- ""
            if (input$var1ID != "") {
                c <- toString(paste(
                    input$var1AggregateBy, input$var1ID, ":", info[4]
                ))
            }
            d <- ""
            if (input$var2ID != "") {
                d <- toString(paste(
                    input$var2AggregateBy, input$var2ID, ":", info[6]
                ))
            }
            e <- toString(paste("% present taxa:", info[5]))
            paste(a, b, c, d, e, sep = "\n")
        }
    })
}

