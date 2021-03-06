#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2, # size limit for input 9999mb
    scipen = 999 # disabling scientific notation
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    
    # get complete taxonomy names and ids
    pathToPhyloprofile <- paste0(
        path.package("PhyloProfile"), "/PhyloProfile"
    )
    nameFullFile <- paste0(
        pathToPhyloprofile, "/data/preProcessedTaxonomy.txt"
    )
    nameFullDf <- data.table::fread(nameFullFile, select = c(1:3))
    
    # DCC download oma app
    callModule(dccDownloadOmaApp, "dccDownloadOma")
    # DCC app
    callModule(dccApp, "dccApp")
    
    # annoFAS app
    callModule(annoFasApp, "annoFasApp")
    
    # FAS app
    callModule(fasApp, "fasApp")
    
    # hamstrPrepare app
    callModule(hamstrPrepareApp, "hamstrPrepareApp", nameFullDf)
	
	# HaMStR app
	hamstrOut <- callModule(hamstrApp, "hamstrApp", nameFullDf)
	
	# PhyloProfile lite
	callModule(phyloprofileLite, "phyloprofileLite", hamstrOut)
	
	# PhyloProfile full
	callModule(phyloprofileFull, "phyloprofileFull")
})

