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
)

# install missing packages
installPackages <- function(packages){
  missingPackages <-
    packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missingPackages))
    install.packages(
      missingPackages,
      dependencies = TRUE,
      repos = "http://cran.us.r-project.org"
    )
}
installPackages(packages)

# Load packages
lapply(packages, library, character.only = TRUE)

# remove old log files
system("rm *.log")

# add path
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "/opt/anaconda3/bin:/Users/bemun/.local/bin:/Users/bemun/Desktop/bionf/HaMStR/bin", sep = ":"))
Sys.setenv(PERL = "usr/bin/perl")
