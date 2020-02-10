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
            
            # DCC TAB ==========================================================
            tabPanel(
            	"DCC"
            ),
            
            # HAMSTR TAB =======================================================
            tabPanel(
                "HaMStR",
                sidebarLayout(
                    # * sidebar panel for hamstr input/options -----------------
                    sidebarPanel(
                        width = 3,
                        # ** hamstr location ===================================
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
                        
                        # ** fasta input =======================================
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
                        
                        # ** required options ==================================
                        strong("Required options"),
                        uiOutput("seqID.ui"),
                        bsPopover(
                            "seqID",
                            "",
                            paste(
                                "Specifie the sequence identifier of the seed",
                                "sequence in the reference protein set.",
                                "If not provided, the program will attempt to",
                                "determine it automatically."
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
                                "seed sequence was derived from."
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
                                "primer taxa for the core set compilation."
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
                                "compilation."
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
                                "core set."
                            ),
                            "top"
                        ),
                        
                        textInput("seqName", "Job name", value = ""),
                        bsPopover(
                            "seqName",
                            "",
                            paste(
                                "Specifies a name for the search. If not",
                                "defined, a random name will be set."
                            ),
                            "bottom"
                        ),
                        
                        hr(),
                        
                        # ** FAS on/off ========================================
                        checkboxInput(
                            "useFAS",
                            strong("Use FAS"),
                            value = TRUE,
                            width = NULL
                        ),
                        conditionalPanel(
                            condition = 
                                "input.useFAS
                                && output.checkFasStatus == 0",
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
                                "that do blebleble"
                            ),
                            "right"
                        ),
                        hr(),
                        
                        # ** optional options ==================================
                        checkboxInput(
                            "optOption",
                            strong("Other options"),
                            value = FALSE,
                            width = NULL
                        ),
                        
                        conditionalPanel(
                            condition = "input.optOption",
                            strong("Additional options"),
                            
                            textInput("coreTaxa", "Core taxa", value = ""),
                            bsPopover(
                                "coreTaxa",
                                "",
                                paste(
                                    "You can provide a list of primer taxa",
                                    "that should exclusively be used for the",
                                    "compilation of the core ortholog set."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "strict",
                                strong("Strict mode"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "strict",
                                "",
                                paste(
                                    "Run the final HaMStR search in strict",
                                    "mode. An ortholog is only then accepted",
                                    "when the reciprocity is fulfilled for",
                                    "each sequence in the core set."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "coreStrict",
                                strong("Strict mode for core set"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "coreStrict",
                                "",
                                paste(
                                    "Run the HaMStR for the compilation of",
                                    "the core set in strict mode."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "checkCoorthologsRef",
                                strong("Check co-orthologs"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "checkCoorthologsRef",
                                "",
                                paste(
                                    "During the final HaMStR search, accept",
                                    "an ortholog also when its best hit in",
                                    "the reverse search is not the core",
                                    "ortholog itself, but a co-ortholog of it."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "corecheckCoorthologsRef",
                                strong("Check co-orthologs for core set"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "corecheckCoorthologsRef",
                                "",
                                paste(
                                    "Invokes the checkCoorthologsRef",
                                    "behavior in the course of the core set",
                                    "compilation."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "rbh",
                                strong("Reciprocal best hit"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "rbh",
                                "",
                                paste(
                                    "Requires a reciprocal best hit during the",
                                    "HaMStR search to accept a new ortholog."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "rep",
                                strong("Only most similar sequence"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "rep",
                                "",
                                paste(
                                    "Set this flag to obtain only the sequence",
                                    "being most similar to the corresponding",
                                    "sequence in the core set rather than all",
                                    "putative co-orthologs."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "coreRep",
                                strong("Only most similar sequence for core set"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "coreRep",
                                "",
                                paste(
                                    "Set this flag to invoke the -rep",
                                    "behaviour for the core ortholog",
                                    "compilation."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "coreOnly",
                                strong("Compile only the core orthologs"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "coreOnly",
                                "",
                                paste(
                                    "Set this flag to compile only the core",
                                    "orthologs. These sets can later be used",
                                    "for a stand alone HaMStR search."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "reuse_core",
                                strong("Reuse existing core-set"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "reuse_core",
                                "",
                                paste(
                                    "Set this flag if the core set for your",
                                    "sequence is already existing. No check",
                                    "currently implemented."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "blast",
                                strong("Auto determin seqID and refspec"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "blast",
                                "",
                                paste(
                                    "Set this flag to determine sequence id",
                                    "and refspec automatically. Note, the",
                                    "chosen sequence id and reference species",
                                    "does not necessarily reflect the species",
                                    "the sequence was derived from."
                                ),
                                "bottom"
                            ),
                            
                            textInput(
                                "group", "Limit search in systematic group",
                                value = ""
                            ),
                            bsPopover(
                                "group",
                                "",
                                paste(
                                    "Allows to limit the search to a certain",
                                    "systematic group"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "evalBlast",
                                strong("Blast e-value cutoff (10^x)"),
                                value = -5,
                                min = -99,
                                max = 0,
                                step = 1
                            ),
                            bsPopover(
                                "evalBlast",
                                "",
                                paste(
                                    "This option allows to set the e-value",
                                    "cut-off for the Blast search.",
                                    "Default: 10^-5"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "evalHmmer",
                                strong("HMM search e-value cutoff (10^x)"),
                                value = -5,
                                min = -99,
                                max = 0,
                                step = 1
                            ),
                            bsPopover(
                                "evalHmmer",
                                "",
                                paste(
                                    "This option allows to set the e-value",
                                    "cut-off for the HMM search.",
                                    "Default: 10^-5"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "evalRelaxfac",
                                strong("Relax factor for e-value cutoff"),
                                value = 10,
                                min = 0,
                                max = 999,
                                step = 1
                            ),
                            bsPopover(
                                "evalRelaxfac",
                                "",
                                paste(
                                    "This options allows to set the factor to",
                                    "relax the e-value cut-off (Blast search",
                                    "and HMM search) for the final HaMStR run.",
                                    "Default: 10"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "hitLimit",
                                strong("Number of HMM hits"),
                                value = 10,
                                min = 0,
                                max = 999,
                                step = 1
                            ),
                            bsPopover(
                                "hitLimit",
                                "",
                                paste(
                                    "Provide an integer specifying the number",
                                    "of hits of the initial pHMM based search",
                                    "that should be evaluated via a reverse",
                                    "search. Default: 10"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "coreHitLimit",
                                strong("Number of HMM hits for core set"),
                                value = 3,
                                min = 0,
                                max = 999,
                                step = 1
                            ),
                            bsPopover(
                                "coreHitLimit",
                                "",
                                paste(
                                    "Provide an integer specifying the number",
                                    "of hits of the initial pHMM based search",
                                    "that should be evaluated via a reverse",
                                    "search. Default: 3"
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "autoLimit",
                                strong("Auto HMM hit limit"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "autoLimit",
                                "",
                                paste(
                                    "Setting this flag will invoke a lagPhase",
                                    "analysis on the score distribution from",
                                    "the hmmer search. This will determine",
                                    "automatically a hit limit for each query.",
                                    "Note, when setting this flag, it will be",
                                    "effective for both the core ortholog",
                                    "compilation and the final ortholog search."
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "scoreThreshold",
                                strong("Threshold for HMM score"),
                                value = 10,
                                min = 0,
                                max = 999,
                                step = 1
                            ),
                            bsPopover(
                                "scoreThreshold",
                                "",
                                paste(
                                    "Instead of setting an automatic hit",
                                    "limit, you can specify with this flag",
                                    "that only candidates with an hmm score no",
                                    "less than x percent of the hmm score of",
                                    "the best hit are further evaluated.",
                                    "Default is x = 10. You can change this",
                                    "cutoff with the option -scoreCutoff.",
                                    "Note, when setting this flag, it will be",
                                    "effective for both the core ortholog",
                                    "compilation and the final ortholog search."
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "scoreCutoff",
                                strong("Percent range of the hmm score"),
                                value = 10,
                                min = 0,
                                max = 999,
                                step = 1
                            ),
                            bsPopover(
                                "scoreCutoff",
                                "",
                                paste(
                                    "In combination with -scoreThreshold you",
                                    "can define the percent range of the hmms",
                                    "core of the best hit up to which a",
                                    "candidate of the hmmsearch will be",
                                    "subjected for further evaluation.",
                                    "Default: 10%"
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "ignoreDistance",
                                strong("Ignore taxon distance"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "ignoreDistance",
                                "",
                                paste(
                                    "Set this flag to ignore the distance",
                                    "between taxa and to choose orthologs",
                                    "only based on score"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "distDeviation",
                                strong("Distance deviation"),
                                value = 1,
                                min = 0,
                                max = 1,
                                step = 0.01
                            ),
                            bsPopover(
                                "distDeviation",
                                "",
                                paste(
                                    "Specify the deviation in score in percent",
                                    "(1=100%, 0=0%) allowed for two taxa to be",
                                    "considered similar"
                                ),
                                "bottom"
                            ),
                            
                            numericInput(
                                "cpu",
                                strong("Number of CPUs"),
                                value = 1,
                                min = 1,
                                max = 99,
                                step = 1
                            ),
                            bsPopover(
                                "cpu",
                                "",
                                paste(
                                    "Determine the number of threads to be run",
                                    "in parallel"
                                ),
                                "bottom"
                            ),
                            
                            selectInput(
                                "aligner", "Alignment tool",
                                choices = c(
                                    "mafft-linsi",
                                    "muscle"
                                ),
                                selected = "muscle"
                            ),
                            bsPopover(
                                "aligner",
                                "",
                                paste(
                                    "Choose between mafft-linsi or muscle for",
                                    "the multiple sequence alignment.",
                                    "DEFAULT: muscle"
                                ),
                                "top"
                            ),
                            
                            checkboxInput(
                                "force",
                                strong("Force mode"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "force",
                                "",
                                paste(
                                    "Force the final HaMStR search to create",
                                    "output file. Existing files will be",
                                    "overwritten."
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "append",
                                strong("Append output"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "append",
                                "",
                                paste(
                                    "Set this flag to append the output to",
                                    "existing output files"
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "silent",
                                strong("Silent run"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "silent",
                                "",
                                paste(
                                    "Surpress output to the command line"
                                ),
                                "bottom"
                            ),
                            
                            checkboxInput(
                                "cleanup",
                                strong("Clean up tmp files"),
                                value = FALSE,
                                width = NULL
                            ),
                            bsPopover(
                                "cleanup",
                                "",
                                paste(
                                    "Temporary output will be deleted."
                                ),
                                "bottom"
                            )
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
            
            # FAS TAB ==========================================================
            tabPanel(
            	"FAS"
            ),
            
            # PHYLOPROFILE TAB =================================================
            tabPanel(
            	"PhyloProfile"
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
