#' Startup script for PhyloProfile
#' 1) install and load packages
#' 2) start the PhyloProfile app

# source("R/functions.R")
sourceFiles = list.files( path = "R", pattern = "*.R$", full.names = TRUE)
lapply(sourceFiles, source, .GlobalEnv)

# List of dependent packages --------------------------------------------------
packages <- c(
    "shinythemes",
    "shinyFiles", "shinyjs", "shinyBS", "stringr"
    # "ape", "bioDist", "Biostrings", "colourpicker", "data.table", "energy",
    # "GenomeInfoDbData", "ggplot2", "GO.db", "grid", "gridExtra", "RColorBrewer",
    # "shiny", "OmaDB", "zoo"
)

# Load packages
lapply(packages, library, character.only = TRUE)

# add path
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "/share/applications/bin:/home/vinh/.local/bin:/share/project/vinh/HaMStR/bin", sep = ":"))

