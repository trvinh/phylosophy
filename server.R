#' Import function files
# sourceFiles = list.files( path = "R", pattern = "*.R$", full.names = TRUE)
# lapply(sourceFiles, source, .GlobalEnv)
library(PhyloProfile)

#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2 # size limit for input 9999mb
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    volumes = getVolumes() # for shinyFileChoose
    
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
    
    getOneseqPath <- reactive({
        hamstrLocation <- suppressWarnings(
            system("which oneSeq", intern = TRUE)
        )
        if (!is.na (hamstrLocation[1])){
            return(hamstrLocation[1])
        } else {
            shinyFileChoose(input, "oneSeqFile", roots = volumes, session = session)
            req(input$oneSeqFile)
            if(!is.null(input$oneSeqFile)){
                file_selected <- parseFilePaths(volumes, input$oneSeqFile)
                return(as.character(file_selected$datapath))
            }
        }
    })
    
    output$hamstrLocation <- renderText({
        oneseqPath <- getOneseqPath()
        paste(
            "HaMStR found at", oneseqPath
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
            system("which greedyFas", intern = TRUE)
        )
        if (!is.na (fasLocation[1])){
            return(fasLocation[1])
        } else {
            shinyFileChoose(input, "greedyFasFile", roots = volumes, session = session)
            req(input$greedyFasFile)
            if(!is.null(input$greedyFasFile)){
                file_selected <- parseFilePaths(volumes, input$greedyFasFile)
                return(as.character(file_selected$datapath))
            }
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
        shinyFileChoose(input, "hamstrInput", roots = volumes, session = session)
        file_selected <- parseFilePaths(volumes, input$hamstrInput)
        req(input$hamstrInput)
        return(as.character(file_selected$datapath))
    })
    
    # get list of sequence IDs =================================================
    getSeqID <- function(file = NULL){
        if (is.null(file)) stop("Input fasta file not provided!")
        faFile <- Biostrings::readAAStringSet(file)
        return(names(faFile))
    }
    
    output$seqID.ui <- renderUI({
        seqIDs <- getSeqID(getInputPath())
        selectInput(
            "seqID", "Seed ID",
            choices = seqIDs,
            selected = seqIDs[1]
        )
    })
    
    # get list of available refspec ============================================
    getRefspecList <- function(oneseqPath = NULL){
        if (is.null(oneseqPath)) stop("HaMStR not found!")
        blastDir <- stringr::str_replace(
            oneseqPath, "/bin/oneSeq.pl", "/blast_dir"
        )
        refspecPath <- list.dirs(
            path = blastDir, full.names = TRUE, recursive = FALSE
        )
        refspecList <- stringr::str_replace(
            refspecPath, paste0(blastDir,"/"), ""
        )
        return(refspecList)
    }
    
    output$refSpec.ui <- renderUI({
        refspecList <- c("undefined")
        refspecList <- getRefspecList(getOneseqPath())
        selectInput(
            "refSpec", "Reference species",
            choices = c("undefined", refspecList),
            selected = "undefined"
        )
    })
    
    # required options =========================================================
    reqOptions <- reactive({
        seqFile <- paste0("-seqFile=", getInputPath())
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
        fasoff <- ""
        if (input$useFAS == FALSE) {
            fasoff <- paste0("-fasoff")
        }
        return(c(fasoff))
    })
    
    output$optOptions.ui <- renderUI({
        HTML(paste(optOptions(), collapse = "<br/>"))
    })
    
    # RUN HAMSTR ===============================================================
    observeEvent(input$refSpec, {
        if (input$refSpec == "undefined") {
            updateButton(session, "doHamstr", disabled = TRUE)
        } else {
            updateButton(session, "doHamstr", disabled = FALSE)
        }
    })
    
    hamstrCmd <- reactive({
        paste(
            "perl",
            getOneseqPath(),
            paste(reqOptions(), collapse = " "),
            paste(optOptions(), collapse = " ")
        )
    })
    
    output$hamstrCmdText <- renderText({
        hamstrCmd()
    })
    
    # runHamstr <- function() {
    #     message("1) HaMStR is running with selected options...")
    #     # message(system(hamstrCmd(), intern = TRUE))
    #     # message(system2("perl", "/Volumes/External/work/bionf/HaMStR/bin/hamstr.pl -h"))
    #     message(system2("bash", "test.sh > test.log", wait = FALSE))
    #     # for (i in 1:30) {
    #     #     message(print(i))
    #     #     Sys.sleep(0.01)
    #     #     flush.console()
    #     # }
    #     message("2) FINISHED! Please check your result at blablabla!<br>")
    # }
    # 
    # observeEvent(input$doHamstr, {
    #     withCallingHandlers({
    #         shinyjs::html("hamstrLog", "")
    #         runHamstr()
    #     },
    #     message = function(m) {
    #         shinyjs::html(
    #             id = "hamstrLog", html = m$message, add = TRUE
    #         )
    #     })
    #     updateButton(session, "doHamstr", disabled = TRUE)
    # })
    
    
    rv <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )
    observeEvent(input$doHamstr, {
        rv$started <- TRUE
        system2("perl", "/Volumes/External/work/bionf/HaMStR/bin/hamstr.pl -h >> test.log", wait = FALSE)
    })
    observeEvent(input$btn_stop, { rv$started <- FALSE })
    observe({
        rv$timer()
        if (isolate(rv$started)) {
            rv$textstream <- paste(readLines("test.log"), collapse = "<br/>")
        }
    })
    # output$hamstrLog <- renderText({
    #     HTML(rv$textstream)
    # })
    output$hamstrLog <- renderUI({
        HTML(rv$textstream)
    })
})

