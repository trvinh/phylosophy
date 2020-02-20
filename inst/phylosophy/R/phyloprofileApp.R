#' PhyloProfile module

phyloprofileAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for input/options -----------------
        sidebarPanel(
            width = 3,
            strong("blablabla"),
            bsButton(
                ns("runPhyloProfile"), "Run PhyloProfile"
            )
        ),
        # * main panel for FAS run ----------------------------
        mainPanel(
            width = 9,
            strong("a lite version of phyloprofile"),
            uiOutput(ns("test"))
        )
    )
}

phyloprofileApp <- function(input, output, session) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    v <- reactiveValues(runPPapp = FALSE)
    observeEvent(input$runPhyloProfile, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        v$runPPapp <- input$runPhyloProfile
        # filein <- input$mainInput
        # if (rstudioapi::isAvailable() == FALSE) {
        #     v$doPlot <- FALSE
        #     updateButton(session, "do", disabled = TRUE)
        # }
    })
    
    runPhyloprofile <- reactive({
        print(v$runPPapp)
        req(v$runPPapp)
        if (rstudioapi::isAvailable() == TRUE) {
            filePath <- system.file(
                "runPhyloProfileApp.R", package = "phylosophy", mustWork = TRUE
            )
            phyloprofileJob <- rstudioapi::jobRunScript(path = filePath)
            return(phyloprofileJob)
        } else {
            updateButton(
                session, ns("runPhyloProfile"), 
                onclick = "window.open('https://applbio.biologie.uni-frankfurt.de/phyloprofile/', '_blank')"
            )
            return("abc")
        }
    })
    
    output$test <- renderUI({
        jobID <- runPhyloprofile()
        jobID
    })
}
