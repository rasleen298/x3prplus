# x3prplus - Analying Bullet Striations From .x3p Files

## Introduction

This repository contains the R package `x3prplus`, along with supporting resources for manipulating bullet images to determine whether two bullets are a match (fired from the same gun barrel) The **repository branches** are organized as follows:

- **master** Contains the R package `x3prplus`
- **application** Contains a companion Shiny application to assess the probability that two bullets are a match
- **papers** Contains academic papers involving this package
- **presentations** Contains presentations involving this package

## Installation

The `x3prplus` package can be installed using `devtools`:

  `devtools::install_github("heike/x3prplus")`
  
## The Hamby Study

The Hamby study bullets can be assessed using this package, as detailed in Hare, Hofmann, Carriquiry (2016) (See `papers/jcgs-2016`). To do so, follow these steps:

1. In an R session, install `x3pr` (Petraco) with `install_github("npetraco/x3pr")` (See http://www.github.com/npetraco/x3pr for more information)
2. Install `x3prplus` with `devtools::install_github("heike/x3prplus")`
3. Install more supporting packages with `install.packages(c("dplyr", "ggplot2", "gridExtra", "zoo"))`
4. Download the Hamby study images from the [NIST Database](http://www.nist.gov/forensics/ballisticsdb/hamby-consecutively-rifled-barrels.cfm)
5. Unzip the folders of images to a directory of your choosing. For instance, a path such as `~/Desktop/Hamby252_3DX3P1of2` should contain all the images of the form "Br1 Bullet 1-1.x3p", and a path such as `~/Desktop/Hamby252_3DX3P2of2` should contain all the images of the form "Ukn Bullet B-1.x3p"
6. Create a folder `csvs` in the same location that contains the Hamby folders
7. Execute the code stored in `papers/jcgs-2016/code/checkBullets.R` Make sure to set the variables `knowndatadir` and `unknowndatadir` to the two directories from step 5.
8. Execute the code stored in `papers/jcgs-2016/code/complete_hamby.R`. Again, make sure to set the variables `knowndatadir` and `unknowndatadir` to the two directories from step 5.
9. Execute the code stored in `papers/jcgs-2016/code/evaluation_hamby.R`. 
10. The complete results will be stored in the csvs folder, in the file name `bullet-stats.csv`