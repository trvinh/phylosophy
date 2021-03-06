% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{replaceHomeCharacter}
\alias{replaceHomeCharacter}
\title{Replace ~ symbol by the name of home folder}
\usage{
replaceHomeCharacter(fullPath = NULL)
}
\arguments{
\item{fullPath}{complete path to input file or folder}
}
\value{
complete path of input file or folder without ~ symbol
}
\description{
Replace ~ symbol by the name of home folder
}
\examples{
replaceHomeCharacter("~/path/to/something")
}
\author{
Vinh Tran {tran@bio.uni-frankfurt.de}
}
