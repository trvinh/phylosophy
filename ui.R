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
              h1("WELCOME TO PHYLOSOPHY WORLD!")
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
            tabPanel(
            	"PhyloProfile",
            	bsButton(
            	    "runPhyloProfile", "Run PhyloProfile",
            	    onclick = "window.open('https://applbio.biologie.uni-frankfurt.de/phyloprofile/', '_blank')"
            	),
            	htmlOutput("phyloprofile")
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
