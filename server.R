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
	
	# PhyloProfile app
	# observeEvent(input$runPhyloProfile, {
	#     rstudioapi::jobRunScript(path = "R/lauchPhyloProfile.R")
	# })
	# output$phyloprofile <- renderUI({
	#     # input$Member
	#     # link = "https://applbio.biologie.uni-frankfurt.de/phyloprofile/"
	#     link = "https://www.google.com"
	#     my_test <- tags$iframe(src=link, height=600, width=535)
	#     print(my_test)
	#     my_test
	# })
})

