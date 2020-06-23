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
            em(strong("phylosophy v0.0.2")),
            id = "tabs",
            collapsible = TRUE,
            inverse = TRUE,
            fluid = TRUE,
            position = "fixed-top",
            
            # DCC TAB ==========================================================
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
            	"DCCv2", dccAppUI("dccApp")
            ),
            
            # HAMSTR TAB =======================================================
            tabPanel(
                "HaMStR",
                hamstrAppUI("hamstrApp")
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
