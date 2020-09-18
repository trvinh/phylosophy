#' MAIN UI ====================================================================
shinyUI(
    fluidPage(
        
        theme = shinytheme("yeti"),
        tags$style(type = "text/css", "body {padding-top: 80px;}"),
        useShinyjs(),
        useShinyalert(),
        
        # Application title
        titlePanel("", windowTitle = "Phylosophy"),
        
        # MAIN NARVARPAGE TABS -------------------------------------------------
        navbarPage(
            em(strong("phylosophy v0.1.0")),
            id = "tabs",
            collapsible = TRUE,
            inverse = TRUE,
            fluid = TRUE,
            position = "fixed-top",
            
            # INTRO TAB ==========================================================
            tabPanel(
              "INTRODUCTION",
              h1("WELCOME TO PHYLOSOPHY TOOL KIT!"),
              tags$img(src="wordcloud.png")
            ),
            
            # FAS TAB ==========================================================
            navbarMenu(
                "FAS",
                tabPanel(
                    "Annotation only",
                    annoFasAppUI("annoFasApp")
                ),
                tabPanel(
                    "Annotation and FAS calculation",
                    fasAppUI("fasApp")
                )
            ),
            
            # DCC TAB ==========================================================
            tabPanel(
                "dcc2",
                dccAppUI("dccApp")
            ),
            
            # HAMSTR TAB =======================================================
            navbarMenu(
                "h1s",
                tabPanel(
                    "Adding taxon to h1s",
                    hamstrPrepareAppUI("hamstrPrepareApp")
                ),
                tabPanel(
                    "HaMStR oneSeq",
                    hamstrAppUI("hamstrApp")
                )
            ),
            
            # PHYLOPROFILE TAB =================================================
            navbarMenu(
                "PhyloProfile",
                tabPanel(
                    "PhyloProfile Lite",
                    phyloprofileLiteUI("phyloprofileLite")
                ),
                tabPanel(
                    "PhyloProfile Full",
                    phyloprofileFullUI("phyloprofileFull")
                )
            ),

            # HELP TAB =========================================================
            tabPanel(
                "Help",
                a(
                    "Readme",
                    href = "https://BIONF.github.io/phylosophy/",
                    target = "_blank"
                ),
                br(),
                a(
                    "Contact us",
                    href = paste0(
                        "mailto:tran@bio.uni-frankfurt.de?",
                        "subject=phylosophy question"
                    ),
                    target = "_blank"
                )
            )
        )
    )
)
