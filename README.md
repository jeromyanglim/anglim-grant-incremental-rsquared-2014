# Incremental Criterion Prediction of Personality Facets over Factors: Obtaining Unbiased Estimates and Confidence Intervals

Version: 13th June 2014

## Description
This repository includes data, a data processing script, R functions, and
analysis code for the manuscript: "Incremental Criterion Prediction of Personality Facets over Factors: Obtaining Unbiased Estimates and Confidence Intervals"

The latest version of this package can be obtained [here](https://github.com/jeromyanglim/anglim-grant-incremental-rsquared-2014).

The file organisation is built around [ProjectTemplate](http://projecttemplate.net/).

## Requirements
* R > 3.0 (possibly earlier versions will work)
* A working perl installation for running `gdata` to import xls files http://www.perl.org/get.html ; 
  OSX and Linux generally come with perl installed; Windows OS does not.
* `config/global.dcf` lists required packages; `knitr` is also required to
  compile the rmd file, and `ProjectTemplate` is also required. 
  
The following commands should install the required R packages:

    install.packages("ProjectTemplate")
    install.packages("knitr")
    install.packages("gdata")
    install.packages("car")
    install.packages("psych")
    install.packages("bootstrap")
    install.packages("MASS")
    install.packages("boot")
    install.packages("QuantPsyc")
    
Also install the `personalityfacets` package. If you don't already have devtools installed, then install it:

    install.packages("devtools")

And then run:

    library(devtools)
    install_github('personalityfacets', username='jeromyanglim')

Alternatively a copy of the `personalityfacets` package is available in the `local-copy` folder of this repository.

* RStudio may also make it easier to convert the rmd file into a HTML file.
  Although [scripts such as this could be used](http://stackoverflow.com/a/10654295/180892).

## How to use
* The simplest way to examine the data and analyses is have RStudio installed
  and then double click on `rsquare-facets-2014-analysis.Rproj`. This should open
  the project in RStudio.
* Open the `rmd` file `reports/explore.rmd`. Then click "Knit to HTML";
  This should import the data, load necessary libraries, import functions,
  prepare the data, and run all the analyses in the rmd file. If everything
  works, you will be presented with a compiled HTML file that shows analysis
  code and output interspersed. Note that the simulation chunk took around 20 hours to run on a modern desktop.
* Alternatively, to explore the data open `reports/explore.rmd` and run
  the first R code chunk to load the data, functions, and libraries. Each of the
  other code chunks can be run independently, or you could just explore the
  data.


## Outline of how the project runs
ProjectTemplate provides a standardized way of organising code and data (see
https://github.com/johnmyleswhite/ProjectTemplate ). The following command
completes loads data, processes data, and imports various libraries and required
code:

    library("ProjectTemplate"); load.project()

Important files and folders include:

* `config/global.dcf` stores a range of configuration options and also sets out
  which libraries need to be loaded
* `data` : Data in the `data` folder is automatically imported with names that
  generally match the file name. The data in the xls file is imported one data
  file per sheet.
* `lib` : Code in the `lib` folder provides various functions used during
  analyses or data preparation
* `munge` : Code in this folder is used to process the data after it is
  imported. This includes creating scale scores, processing categorical
  variables, recoding various variables. The code there also, as a by product,
  calculates reliabilities which are used in subsequent analysis. We also use
  a strategy of creating a list object called `v` which has elements
  representing various sets of variables that are often called together. 


##  License: 
* This data and analysis have been submitted for publication and is under review.
  While under review the repository is copyrighted and no permission is granted to re-analyse the
  data for publication or reuse any of the materials. After publication
  code and data will be licenced GPL-2; all measurement scales
  retain existing licences; 
* Any subsequent publication that re-uses the data should cite this publication. 

