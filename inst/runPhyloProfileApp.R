library(PhyloProfile)
pathToPhyloprofile <- paste0(path.package("PhyloProfile"), "/PhyloProfile")
shiny::runApp(pathToPhyloprofile, launch.browser = TRUE)#, port = 875)
