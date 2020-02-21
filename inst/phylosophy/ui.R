#' MAIN UI ====================================================================
shinyUI(
    fluidPage(
        
        theme = shinytheme("yeti"),
        tags$style(type = "text/css", "body {padding-top: 80px;}"),
        useShinyjs(),

        # Application title
        titlePanel("", windowTitle = "Phylosophy"),
        
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
              "INTRODUCTION",
              h1("WELCOME TO PHYLOSOPHY WORLD!"),
              tags$img(src="wordcloud.png")
            ),
            
            # FAS TAB ==========================================================
            tabPanel(
                "FAS", fasAppUI("fasApp")
            ),
            
            # DCC TAB ==========================================================
            tabPanel(
            	"DCC",
            	h3("Hey Hannah, where is the DCC?")
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
