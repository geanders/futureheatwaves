<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/geanders/futureheatwaves.svg?branch=master)](https://travis-ci.org/geanders/futureheatwaves)

Overview
========

The `futureheatwaves` package takes a directory of climate projection files and, for each, identifies and characterizes all of a certain type of multi-day extreme events (e.g., heat waves). The definition used to identify extreme events can be customized. Characterizations include several metrics of event length, intensity, and timing in the year. The identified extreme events can be explored by applying custom functions across all generated heat wave files.

Installing
==========

You can download the latest stable version of this package directly from CRAN:

``` r
install.packages("futureheatwaves")
```

The development version of this package can be installed directly from GitHub using `install_github` from the `devtools` package:

``` r
library(devtools)
install_github("geanders/futureheatwaves", build_vignettes = TRUE)
library(futureheatwaves)
```

Because this package includes some C++ functions, this installation from GitHub requires the compiling of some code. If you get an error when trying to install this package from GitHub, it may be because your computer lacks the required tools to compile that part of the code. In this case, you can either download and install the required tools or email me to get a source version of the code for your operating system (where the C++ code has already been compiled).

Using the package
=================

There are extensive details on using this package in the package vignette, which can be accessed once you install the package using:

``` r
vignette("futureheatwaves")
```

A version of this vignette is also available through CRAN [here](https://CRAN.R-project.org/package=futureheatwaves/vignettes/futureheatwaves.html).

The development version of the package also includes a vignette with details on how to prepare CMIP5 netCDF climate model output files for use with this package. If you have downloaded the development version of the package directly from GitHub, you can access that vignette with:

``` r
vignette("starting_from_netcdf")
```
