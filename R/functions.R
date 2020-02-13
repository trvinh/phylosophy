
#' Create random strings
#' @param n number of output strings
#' @return n strings created by 10 random characters
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @examples 
#' randFn(1)
randFn <- function(n = 5000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(
        a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)
    )
}

#' Get sequence IDs from fasta file
#' @param file fasta file
#' @return list object contains sequence IDs from input file
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
#' @import Biostrings
getSeqID <- function(file = NULL){
    if (is.null(file)) stop("Input fasta file not provided!")
    faFile <- Biostrings::readAAStringSet(file)
    return(names(faFile))
}

#' Get file name from complete path
#' @param filePath complete path to a file
#' @return file name and its extension
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
getFileName <- function(filePath = NULL) {
    if (is.null(filePath)) stop ("No complete path given!")
    tmp <- strsplit(filePath, "/")[[1]]
    return(tmp[length(tmp)])
}
