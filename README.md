
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CYTOGLO <img src="inst/app/www/CYTOGLO_icon.png" align="right" height="139"/>

CYTOGLO (CYTOmetry Gating & LOgical post-processsing) is a novel shiny
app for the elaboration of the CytOF files. From the raw .fcs files is
it possibile to apply a PeacoQC filtering, a customized gating and
include panel files and metadata files in order to create a final
SingleCellExperiment object. From this object you can perform
clustering, dimensionality reduction and differential expression
analysis.

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-succes.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/ShinyFabio/CYTOGLO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ShinyFabio/CYTOGLO/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

You can install the development version of `{CYTOGLO}` like so:

``` r
devtools::install_github("ShinyFabio/CYTOGLO")
```

## Run

You can launch the application by running:

``` r
CYTOGLO::run_app()
```

## Authors & Contributors

CYTOGLO was designed and envisioned by the following authors:

Fabio Della Rocca – Main Developer & Maintainer (aut, cre)

Olga Lanzetta – Main Developer & Data Contributor (aut, dtc)

Claudia Angelini – Supervision & Design (aut, rev)

## Funding

This work was supported by the project “MIGLIORA” – “Misurazione delle
Immunoglobuline a Domicilio: monitoraggio per l’Ottimizzazione dei
Protocolli diagnostici e Terapeutici in pazienti con CVID mediante l’uso
di nuovi dispositivi domiciliari e Intelligenza Artificiale” (Progetto
ID 237).
