#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2 # size limit for input 9999mb
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    
    # DCC app
    callModule(dccApp, "dccApp")
    
    # annoFAS app
    callModule(annoFasApp, "annoFasApp")
    
    # FAS app
    callModule(fasApp, "fasApp")
	
	# HaMStR app
	hamstrOut <- callModule(hamstrApp, "hamstrApp")
	
	# PhyloProfile lite
	callModule(phyloprofileLite, "phyloprofileLite", hamstrOut)
	
	# PhyloProfile full
	callModule(phyloprofileFull, "phyloprofileFull")
})

