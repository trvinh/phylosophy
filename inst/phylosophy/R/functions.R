createSliderCutoff <- function(id, title, start, stop, varID){
    if (is.null(varID)) return()
    if (varID == "") {
        sliderInput(id, title,
                    min = 1,
                    max = 1,
                    step = 0.025,
                    value = 1,
                    width = 200)
    } else {
        sliderInput(id, title,
                    min = 0,
                    max = 1,
                    step = 0.025,
                    value = c(start, stop),
                    width = 200)
    }
}

updateSliderCutoff <- function(session, id, title, newVar, varID){
    if (is.null(varID) || varID == "") return()
    updateSliderInput(session, id, title,
                      value = newVar,
                      min = 0,
                      max = 1,
                      step = 0.025)
}

createPlotSize <- function(id, title, value) {
    numericInput(id,
                 title,
                 min = 100,
                 max = 3200,
                 step = 50,
                 value = value,
                 width = 100)
}

createTextSize <- function(id, title, value, width) {
    numericInput(id,
                 title,
                 min = 3,
                 max = 99,
                 step = 1,
                 value = value,
                 width = width)
}

msgPlot <- function() {
    msg <- paste(
        "No information about domain architecture!",
        "Please check:","if the domain file exists; or ",
        "if the selected genes (seed & ortholog) do exist in the domain file",
        sep = "\n"
    )
    x <- c(1,2,3,4,5)
    y <- c(1,2,3,4,5)
    g <- ggplot(data.frame(x, y), aes(x,y)) +
        geom_point(color = "white") +
        annotate(
            "text", label = msg, x = 3.5, y = 0.5, size = 5, colour = "red"
        ) +
        theme(axis.line = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), axis.title = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_blank()) +
        ylim(0,1)
    return(g)
}

substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft <- function(x, n) {
    substr(x, 1, n)
}

createAbbrName <- function(fullName) {
    fullName <- gsub("[^[:alnum:] ]", "", fullName)
    taxName <- str_split(fullName, " ")
    if (length(taxName[[1]]) > 1) {
        suggestName <- toupper(
            paste0(
                substrLeft(taxName[[1]][1], 3), 
                substrLeft(taxName[[1]][2], 2)
            )
        )
    } else {
        suggestName <- toupper(substrLeft(taxName[[1]][1], 5))
    }
    return(suggestName)
}