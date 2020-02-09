#' Import function files
# sourceFiles = list.files(path = "R", pattern = "*.R$", full.names = TRUE)
# lapply(sourceFiles, source, .GlobalEnv)

#' MAIN UI ====================================================================
shinyUI(
    fluidPage(
        
        theme = shinytheme("yeti"),
        tags$style(type = "text/css", "body {padding-top: 80px;}"),
        useShinyjs(),

        # Application title
        titlePanel("", windowTitle = "PhyloProfile"),
        
        # MAIN NARVARPAGE TABS -------------------------------------------------
        navbarPage(
            em(strong("Phylosophy v0.0.1")),
            id = "tabs",
            collapsible = TRUE,
            inverse = TRUE,
            fluid = TRUE,
            position = "fixed-top",

            # HAMSTR TAB =======================================================
            tabPanel(
                "HaMStR",
                sidebarLayout(
                    # * sidebar panel for hamstr input/options -----------------
                    sidebarPanel(
                        width = 3,
                        conditionalPanel(
                            condition = "output.checkHamstrStatus == 0",
                            shinyFilesButton(
                                "oneSeqFile",
                                "HaMStR location?" ,
                                title = "Please provide oneSeq.pl file:",
                                multiple = FALSE,
                                buttonType = "default"
                            )
                        ),
                        hr(),
                        
                        h3("Input and configurations"),
                        hr(),
                        shinyFilesButton(
                            "hamstrInput", "Input fasta file!" ,
                            title = "Please provide fasta input file:", 
                            multiple = FALSE,
                            buttonType = "default", class = NULL
                        ),
                        br(),
                        br(),
                        
                        strong("Required options"),
                        
                        uiOutput("seqID.ui"),
                        bsPopover(
                            "seqID",
                            "",
                            paste(
                                "Specifie the sequence identifier of the seed",
                                "sequence in the reference protein set.",
                                "If not provided, the program will attempt to",
                                "determine it automatically.",
                                sep = "<br>"
                            ),
                            "bottom"
                        ),
                        
                        uiOutput("refSpec.ui"),
                        bsPopover(
                            "refSpec",
                            "",
                            paste(
                                "Determine the reference species for the",
                                "hamstr search. It should be the species the",
                                "seed sequence was derived from.",
                                sep = "<br>"
                            ),
                            "bottom"
                        ),
                        
                        selectInput(
                            "minDist", "Min systematic distance",
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
                            "minDist",
                            "",
                            paste(
                                "Specify the minimum systematic distance of",
                                "primer taxa for the core set compilation.",
                                sep = "<br>"
                            ),
                            "top"
                        ),
                        
                        selectInput(
                            "maxDist", "Max systematic distance",
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
                            "maxDist",
                            "",
                            paste(
                                "Specify the maximum systematic distance of",
                                "primer taxa fto be considered for core set",
                                "compilation.",
                                sep = "<br>"
                            ),
                            "top"
                        ),
                        
                        numericInput(
                            "coreOrth", "Number of core orthologs",
                            value = 5,
                            min = 3,
                            max = 99
                        ),
                        bsPopover(
                            "coreOrth",
                            "",
                            paste(
                                "Specify the number of orthologs added to the",
                                "core set.",
                                sep = "<br>"
                            ),
                            "top"
                        ),
                        hr(),
                        
                        checkboxInput(
                            "useFAS",
                            strong("Use FAS"),
                            value = TRUE,
                            width = NULL
                        ),
                        conditionalPanel(
                            condition = "output.checkFasStatus == 0",
                            shinyFilesButton(
                                "greedyFasFile", 
                                "FAS location?" ,
                                title = "Please provide greedyFAS.py file:", 
                                multiple = FALSE,
                                class = NULL
                                
                            )
                        ),
                        textOutput("fasLocation"),
                        bsPopover(
                            "useFAS",
                            "",
                            paste(
                                "FAS is blablabla",
                                "that do blebleble",
                                sep = "<br>"
                            ),
                            "right"
                        )
                    ),
                    # * main panel for hamstr run ----------------------------
                    mainPanel(
                        width = 9,
                        column(
                            6,
                            bsButton(
                                "doHamstr", "Run HaMStR",
                                style = "warning", disabled = TRUE
                            ),
                            actionButton("btn_stop",label = "Stop"),
                            conditionalPanel(
                                condition = 'input.refSpec != "undefined"',
                                textOutput("hamstrLocation")
                            ),
                            hr(),
                            strong("SELECTED OPTIONS"),
                            br(), br(),
                            strong("Required"),
                            br(), br(),
                            uiOutput("reqOptions.ui"),
                            br(), br(),
                            strong("Optional"),
                            br(), br(),
                            uiOutput("optOptions.ui")
                        ),
                        column(
                            6,
                            verbatimTextOutput("hamstrCmdText"),
                            # verbatimTextOutput("hamstrLog")
                            htmlOutput("hamstrLog")
                        )
                    )
                )
            ),

            # HELP TAB =========================================================
            navbarMenu(
                "Help",
                tabPanel(
                    a(
                        "Wiki",
                        href = "https://github.com/BIONF/PhyloProfile/wiki",
                        target = "_blank"
                    )
                ),
                tabPanel(
                    a(
                        "About",
                        href = "https://BIONF.github.io/PhyloProfile/",
                        target = "_blank"
                    )
                )
            )
        )
    )
)
