<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
========

The `futureheatwaves` package takes a directory of climate projection files and, for each, identifies and characterizes all heat waves. The definition used to identify heat waves can be customized. Characterizations include several metrics of heat wave length, intensity, and timing in the year. Heat waves can be explored by applying custom functions across all generated heat wave files.

Installing
==========

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

An uncompiled version of the vignette can be found, without having to install the package, [here](https://github.com/geanders/futureheatwaves/blob/master/vignettes/futureheatwaves.Rmd).
