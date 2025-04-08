
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scAgeComShiny

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

scAgeComShiny provides a shiny app to explore how intercellular
communication changes with age in 23 mouse tissues.

An online version of this app can be accessed here:
[scagecom.org](https://scagecom.org/).

A docker image of the app is available
[here](https://hub.docker.com/r/ursueugen/scagecom).

## Installation

You can run a local version of the website locally as follows.

- Git clone this repo locally
- Launch R and install the package

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("CyrilLagger/scAgeComShiny")
```

- Download the associated data available at
  [figshare](http://doi.org/10.6084/m9.figshare.17075375) and save the
  file scAgeCom_data.rda under scAgeComShiny/data/
- Run

``` r
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
scAgeComShiny::run_app()
```

## Reference

This shiny app has been built with [golem](https://golemverse.org/).
