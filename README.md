# phylosophy

*phylosophy* is an R toolkit for generating and exploring complex phylogenetic profiles. It provides solutions for searching orthologs for a set of genes of interest, enhancing the functional linkages between query genes and their orthologs with feature similarity scores, and finally visually exploring the phylogenetic profiles of query genes.

# Table of Contents
* [Installation](#installation)
* [Usage](#usage)
* [Contributors](#contributors)
* [Contact](#contact)



# Installation

*phylosophy* requires [R](https://cran.r-project.org) (version ≥ 3.6.0). Please install or update R on your computer before continue.

* [R for Linux](https://cran.r-project.org/bin/linux/)
* [R for Mac OS](https://cran.r-project.org/bin/macosx/)
* [R for Windows](https://cran.r-project.org/bin/windows/base/)

Some components of *phylosophy* also requires [Python](https://www.python.org/downloads/) (version ≥ 3.0). Make sure you have python3 to use all functions of *phylosophy*.

*phylosophy* can be installed as same as other R packages using `devtools` library. Start `R` and use these command lines to install the tool

```
if (!requireNamespace("devtools"))
    install.packages("devtools")
devtools::install_github("BIONF/phylosophy", INSTALL_opts = c('--no-lock'), build_vignettes = TRUE)
```

# Usage

From the R terminal, enter:
```r
library(phylosophy)
runPhylosophy()
```
Check your web browser, *phylosophy* will be displayed there ;-)

# Contributors
* [Vinh](https://github.com/trvinh)

# Contact
Vinh Tran
tran@bio.uni-frankfurt.de
