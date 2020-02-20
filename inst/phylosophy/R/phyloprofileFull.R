#' PhyloProfile module
library(PhyloProfile)

phyloprofileFullUI <- function(id) {
    ns <- NS(id)
    tagList(
        h3(
            "Use the full version to utilize all the features and capabilities 
            of PhyloProfile"
        ),
        hr(),
        uiOutput(ns("runPP.btn")),
        br(),
        verbatimTextOutput(ns("jobID")),
        br(),
        uiOutput(ns("stopPP.btn"))
        
    )
}

phyloprofileFull <- function(input, output, session) {
    ns <- session$ns
    
    # * run full version of PhyloProfile ---------------------------------------
    # check if phylosophy is run using rstudio or not
    output$runPP.btn <- renderUI({
        if (rstudioapi::isAvailable() == FALSE) {
            tagList(
                bsButton(
                    ns("runPhyloProfile"), "Run PhyloProfile full",
                    onclick = "window.open('https://applbio.biologie.uni-frankfurt.de/phyloprofile/', '_blank')"
                ),
                br(), br(),
                em(
                    "You are not using RStudio. An online version of 
                    PhyloProfile will be opened!"
                )
            )
        } else {
            currentVersion <- rstudioapi::versionInfo()$version
            if (currentVersion >= '1.2') {
                bsButton(
                    ns("runPhyloProfile"), "Run PhyloProfile full"
                )
            } else {
                tagList(
                    bsButton(
                        ns("runPhyloProfile"), "Run PhyloProfile full",
                        onclick = "window.open('https://applbio.biologie.uni-frankfurt.de/phyloprofile/', '_blank')"
                    ),
                    br(), br(),
                    em(
                        "You are using an older RStudio version than v1.2. 
                        An online version of PhyloProfile will be opened!"
                    )
                )
            }
        }
    })
    
    # check runPhyloProfile btn status 
    v <- reactiveValues(runPPapp = FALSE)
    observeEvent(input$runPhyloProfile, {
        v$runPPapp <- input$runPhyloProfile
    })
    
    # run phyloprofile app and return job ID
    # jobIDv <- reactiveValues()
    runPhyloprofile <- reactive({
        req(v$runPPapp)
        if (rstudioapi::isAvailable() == TRUE) {
            filePath <- system.file(
                "runPhyloProfileApp.R", package = "phylosophy", mustWork = TRUE
            )
            v$jobID <- rstudioapi::jobRunScript(path = filePath)
            # phyloprofileJob <- "RUN RUN RUN"
            return(v$jobID)
        } else {
            return(NULL)
        }
    })
    
    # get job ID for local phyloprofile job
    output$jobID <- renderText({
        jobID <- runPhyloprofile()
        updateButton(session, ns("runPhyloProfile"), disabled = TRUE)
        if (!is.null(jobID)) {
            return(paste("Job ID:", jobID))
        }
    })
    
    # kill phyloprofile job
    output$stopPP.btn <- renderUI({
        req(v$runPPapp)
        if (rstudioapi::isAvailable() == TRUE) {
            bsButton(
                ns("stopPhyloProfile"), "Kill job!"
            )
        }
    })
    
    observeEvent(input$stopPhyloProfile, {
        rstudioapi::jobRemove(v$jobID)
        updateButton(session, ns("stopPhyloProfile"), disabled = TRUE)
    })
}

