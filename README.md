
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rflirtool

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/rflirtool)](https://CRAN.R-project.org/package=rflirtool)
<!-- badges: end -->

# rflirtool: FLIR Data Processing Toolkit for vegetation plots at RMBL

## Overview

The **rflirtool** package is a comprehensive toolkit for processing and
analyzing thermal data from FLIR camera at RMBL gradient, with a
particular focus on vegetation research and analysis. It simplifies the
handling of CSV data files generated by FLIR Tools software, making it
easier to extract valuable insights from thermal images.

## Features

- Load and process thermal data from CSV files generated with FLIR
  Tools.
- Calculate the basic statistic data for temperature data.
- Perform statistical analysis on thermal datasets.
- Simplify the workflow for vegetation analysis with FLIR cameras.

## Installation

To install the latest development version from GitHub, you can use the
`pak` package:

``` r
pak::pkg_install("PaulESantos/rflirtool")
```

## Getting Started

Here’s a quick example of how to use rflirtools to load and analyze
thermal data:
