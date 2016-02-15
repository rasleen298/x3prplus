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

The Hamby study bullets can be assessed using this package, as detailed in Hare, Hofmann, Carriquiry (2016) (See papers/jcsg-2016). To do so, follow these steps:

1. In an R session, install `x3pr` (Petraco) with `install_github("npetraco/x3pr")` (See http://www.github.com/npetraco/x3pr for more information)
2. Install `x3prplus` with `devtools::install_github("heike/x3prplus")`
3. Download the Hamby study images from the [NIST Database](http://www.nist.gov/forensics/ballisticsdb/hamby-consecutively-rifled-barrels.cfm)
