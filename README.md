# phylosophy

*phylosophy* is an R toolkit for generating and exploring complex phylogenetic profiles. It provides solutions for searching orthologs for a set of genes of interest, enhancing the functional linkages between query genes and their orthologs with feature similarity scores, and finally visually exploring the phylogenetic profiles of query genes.

# Table of Contents
* [Installation](#installation)
* [Usage](#usage)
* [DCC components](#dcc-components)
* [Contributors](#contributors)
* [Contact](#contact)



# Installation

*phylosophy* requires [R](https://cran.r-project.org) (version ≥ 4.0). Please install or update R on your computer before continue.

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

Besides, some [components of DCC](#dcc-components) require an additional installation. Please follow their instruction.

# Usage
## Configuration


## Run phylospphy
From the R terminal, enter:
```r
library(phylosophy)
runPhylosophy()
```
Check your web browser, *phylosophy* will be displayed there ;-)

# DCC components

*phylosophy* contains the following apps:

* [MILTS](#milts-app):
* [BUSCO](#busco-app): Assessing gene set completeness
* [DCCv2](#dccv2-app): Compiling core sets for HaMStR
* [FAS](#fas-app): Calculating pairwise protein feature architecture similarity
* [HaMStR](#hamstr-app): Searching feature-aware orthologs
* [PhyloProfile](#phyloprofile-app): Exploring complex phylogenetic profiles

## MILTS app
blablabla

## BUSCO app
*[NAME]* is an R package for assessing the completenss of a given gene set. Similar to [BUSCO](https://busco.ezlab.org), *[NAME]* compares the user input gene set with its pre-calculated hierarchical core ortholog data and assign those core genes if they are present, missing or different in term of functional equivalence.

*[NAME]* can be used as a independent tool (install from its github page) or as an app included in *phylosophy* (no installation required). 

## DCCv2 app
*DCCv2* is a tool for compiling core set data for HaMStR using predicted orthologs from [OMA](https://omabrowser.org/oma/home/), both *OMA-browser* and *OMA-standalone*. Outputs of this tool are 3 (optional 4) folders required for a HaMStR run, including **(1) core_orthologs** (comprises of OMA orthologous group - OG, or OMA pairs - OP. Each OG/OP has its own directory, where a multiple fasta file and a corresponding profile HMM can be found), **(2) genome_dir** (contains gene sets of taxa, from which the orthologs are originated), **(3) blast_dir** (holds the blast databases of those gene sets within `genome_dir`), and an optional **(4) weight_dir** (contains feature architecure annotations of all gene sets).

For OMA-standalone, *DCCv2* requires the output orthoXML file from OMA, a taxon mapping file in tab-delimited format containing 3 columns (blabla) (blabla) (blabla). Protein set of included taxa can be either given as a folder, or automatically downloaded from OMA database.

To use *DCCv2* with OMA-browser, some data from OMA need to be downloaded and processed in advance. This can be done using the script `path/to/R/libraries/phylosophy/phylosophy/scripts/createOmaDic.py`:
```
python3 createOmaDic.py -o <out_put_directory>
```
For OMA-browser, *DCCv2* accept input as NCBI taxonomy IDs, taxon names or an OMA group ID. If only 2 taxa are selected, you can decided if OMA groups or OMA pairs should be taken into account.

*DCCv2* is delivered together with *phylosophy*. No additional installation needed. To find the `path/to/R/libraries`, where *phylosophy* has been installed, you can use this command in the R console:
```
.libpaths()
```

## FAS app

greedyFAS is a python tool for calculating the so called FAS-score, which is a measure of how similar the feature architectures of two proteins are.

greedyFAS (version ≥ 1.2.0) needs to be installed for using the *FAS app* and the complete function of *HaMStR app*. Please follow the instruction on its [github site](https://github.com/BIONF/FAS) for more information.

FAS app contains 2 main functions. One is used only for doing feature annotation for a multiple FASTA input. This function gets use of the `annoFAS` function of greedyFAS. The second one performs the architecture similarity calculation, which is done by the `calcFAS` function.

## HaMStR app

HaMStR-oneSeq is a advanced version of HaMStR ([Ebersberger *et al.* 2009](https://bmcevolbiol.biomedcentral.com/articles/10.1186/1471-2148-9-157)), an feature-aware orthology search tool.

Similar to greedyFAS, HaMStR-oneSeq needs to be manually installed using [this manual](https://github.com/BIONF/HaMStR).

HaMStR-oneSeq comes with a pre-calculated data set containing the default 78 taxa of the [Quest for Ortholog consortium](https://questfororthologs.org). A new taxon can be added to HaMStR-oneSeq, if it follows the naming schema of HaMStR-oneSeq and has some additional information such as the annotated features and its blast database. *HaMStR app* provides a function to simplify this process. One needs only submit the FASTA file of the species of interest and its taxon ID (can be the NCBI taxonomy, but it is not a must).

## PhyloProfile app

[PhyloProfile](https://github.com/BIONF/PhyloProfile) is an R package for integrating, visualizing and exploring complexed phylogenetic profile ([Tran *et al.* 2018](https://academic.oup.com/bioinformatics/article/34/17/3041/4962496)).

In *phylosophy*, there are 2 versions of *PhyloProfile*. For a quick visualizing and basic filtering of the phylogenetic profiles, a lite version of PhyloProfile has been directly implemented inside *phylosophy*. For utilize the complete functions of that package, a tab in *phylosophy* will open the full version of PhyloProfile's GUI tool.

PhyloProfile will be automatically installed together with *phylosophy*. However, if you are using PhyloProfile for the first time, you need to start the full version once to getting the required taxonomy data, even when you only need the lite version.

# Contributors
* [Vinh](https://github.com/trvinh)

# Contact
Vinh Tran
tran@bio.uni-frankfurt.de
