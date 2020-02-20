#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2 # size limit for input 9999mb
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    
    # FAS app
    callModule(fasApp, "fasApp")
	
	# HaMStR app
	callModule(hamstrApp, "hamstrApp")
	
	# PhyloProfile lite
	callModule(phyloprofileLite, "phyloprofileLite")
	
	# PhyloProfile full
	# callModule(phyloprofileFull, "phyloprofileFull")
	# check if phylosophy is run using rstudio or not
	output$runPP.btn <- renderUI({
	    if (rstudioapi::isAvailable() == FALSE) {
	        tagList(
	            bsButton(
	                "runPhyloProfile", "Run PhyloProfile full",
	                onclick = "window.open('https://applbio.biologie.uni-frankfurt.de/phyloprofile/', '_blank')"
	            ),
	            br(), br(),
	            em("You are not using RStudio. An online version of PhyloProfile will be opened!")
	        )
	    } else {
	        bsButton(
	            "runPhyloProfile", "Run PhyloProfile full"
	        )
	    }
	})
	
	# check runPhyloProfile btn status 
	v <- reactiveValues(runPPapp = FALSE)
	observeEvent(input$runPhyloProfile, {
	    v$runPPapp <- input$runPhyloProfile
	})
	
	# run phyloprofile app and return job ID
	jobIDv <- reactiveValues()
	runPhyloprofile <- reactive({
	    req(v$runPPapp)
	    if (rstudioapi::isAvailable() == TRUE) {
	        filePath <- system.file(
	            "runPhyloProfileApp.R", package = "phylosophy", mustWork = TRUE
	        )
	        job <- rstudioapi::jobRunScript(path = filePath)
	        # phyloprofileJob <- "RUN RUN RUN"
	        return(job)
	    } else {
	        return(NULL)
	    }
	})
	
	# get job ID for local phyloprofile job
	output$jobID <- renderText({
	    jobID <- runPhyloprofile()
	    if (!is.null(jobID)) {
	        return(paste("Job ID:", jobID))
	    }
	})
	
	# kill phyloprofile job
	output$stopPP.btn <- renderUI({
	    req(v$runPPapp)
	    if (rstudioapi::isAvailable() == TRUE) {
	        bsButton(
	            "stopPhyloProfile", "Kill job!"
	        )
	    }
	})
	
	observeEvent(input$stopPhyloProfile, {
	    jobRemove(jobIDv)
	})
})

