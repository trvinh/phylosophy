#' add new taxon to hamstr module

dccDownloadOmaAppUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        # * sidebar panel for FAS input/options -----------------
        sidebarPanel(
            width = 3,
            h3("Input and configurations"),
            hr(),

            shinyDirButton(
                ns("outOmaDir"), "Output directory" ,
                title = "Please select a folder",
                buttonType = "default", class = NULL
            ),
            br(),
            uiOutput(ns("outputLocation.ui")),
            br(),
            checkboxInput(
                ns("force"),
                strong("Override old data"),
                value = FALSE,
                width = NULL
            ),
            textInput(ns("downloadJob"), strong("Job ID"), value = randFn(1)),
            bsButton(ns("doDownload"), "Download OMA data", disabled = TRUE)
        ),
        # * main panel for annoFAS and greedyFAS -------------------------------
        mainPanel(
            width = 9,
            strong("Command"),
            verbatimTextOutput(ns("downloadCmdText")),
            strong("Progress"),
            verbatimTextOutput(ns("downloadLog")),
            br(),
            uiOutput(ns("end.ui"))
        )
    )
}

dccDownloadOmaApp <- function (input, output, session, nameFullDf) {
    homePath = c(wd='~/') # for shinyFileChoose
    ns <- session$ns
    
    python <- reactive({
        if (try(system("python -V") < "Python 3")) {
            return("python3")
        } else {
            return("python")
        }
    })
    
    
    # get output path ==========================================================
    getOutputPath <- reactive({
        shinyDirChoose(
            input, "outOmaDir", roots = homePath, session = session
        )
        outputPath <- parseDirPath(homePath, input$outOmaDir)
        return(replaceHomeCharacter(as.character(outputPath)))
    })
    
    outputLocation <- reactive({
        if (length(getOutputPath()) > 0)
            return(getOutputPath())
        else
            return(NULL)
    })
    
    # RUN createOmaDic.py ======================================================
    observe({
        if (length(getOutputPath()) > 0)
            updateButton(session, ns("doDownload"), disabled = FALSE)
    })
    
    downloadCmd <- reactive({
        cmd <- paste(
            python(), "scripts/dcc/createOmaDic.py",
            "--outPath", getOutputPath()
        )
        if (input$force) cmd <- paste(cmd, "--force")
        return(cmd)
    })
    
    output$downloadCmdText <- renderText({
        downloadCmd()
    })
    
    rvDownload <- reactiveValues(
        textstream = c(""),
        timer = reactiveTimer(1000),
        started = FALSE
    )
    
    observeEvent(input$doDownload, {
        rvDownload$started <- TRUE
        cmd <- paste(downloadCmd(), ">>", paste0(input$downloadJob, ".downloadOma.log"))
        system(cmd, wait = FALSE)
        updateButton(session, ns("doDownload"), disabled = TRUE)
    })
    
    observe({
        rvDownload$timer()
        if (isolate(rvDownload$started)) {
            if (file.exists(paste0(input$downloadJob, ".downloadOma.log"))) {
                # print("YES")
                rvDownload$textstream <- suppressWarnings(
                    readLines(
                        paste0(input$downloadJob, ".downloadOma.log"),  n = -1
                    ) %>% tail(10) %>% paste(collapse = "\n")
                )
                # print(rvDownload$textstream)
                # if (grepl("finished", rvDownload$textstream))
                #     rvDownload$started <- FALSE
            }
        }
    })
    output$downloadLog <- renderText({
        rvDownload$textstream
    })
    
    # finishing msg ============================================================
    output$end.ui <- renderUI({
        req(input$doDownload)
        tagList(
            strong("Output"),
            br(),
            paste("Your output will be saved at: ", getOutputPath())
        )
    })
}
