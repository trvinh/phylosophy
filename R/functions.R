
#' Create random strings
#' @export
#' @param n number of output strings
#' @return n strings created by 10 random characters
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @examples 
#' randFn(1)
randFn <- function (n = 5000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(
        a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)
    )
}

#' Get sequence IDs from fasta file
#' @export
#' @param file fasta file
#' @return list object contains sequence IDs from input file
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @import Biostrings
#' @examples 
#' file <- system.file(
#'     "extdata", "query.fa", package = "phylosophy", mustWork = TRUE
#' )
#' getSeqID(file)
getSeqID <- function (file = NULL){
    if (is.null(file)) stop("Input fasta file not provided!")
    faFile <- Biostrings::readAAStringSet(file)
    return(names(faFile))
}

#' Get file name from complete path
#' @export
#' @param filePath complete path to a file
#' @return file name and its extension
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @examples 
#' file <- system.file(
#'     "extdata", "query.fa", package = "phylosophy", mustWork = TRUE
#' )
#' getFileName(file)
getFileName <- function (filePath = NULL) {
    if (is.null(filePath)) stop ("No complete path given!")
    tmp <- strsplit(filePath, "/")[[1]]
    return(tmp[length(tmp)])
}

#' Replace ~ symbol by the name of home folder
#' @export
#' @param fullPath complete path to input file or folder
#' @return complete path of input file or folder without ~ symbol
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @import stringr
#' @examples 
#' replaceHomeCharacter("~/path/to/something")
replaceHomeCharacter <- function (fullPath = NULL) {
    homeName <- system("echo $HOME", intern = TRUE)
    stringr::str_replace(fullPath, "~", homeName)
}

