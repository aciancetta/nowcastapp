
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nowcastapp

<!-- badges: start -->
<!-- badges: end -->

The goal of nowcastapp is to provide simple access to the
`nowcast_gmi.R` app. The app is a dashboard for real-time forecasts of
the European industrial production. As the user selects the country of
interest, the app automatically retrieves up-to-date time series from
Eurostat and Google (mobility indexes, GMI). The app estimates a Dynamic
Factor Model using the [`emDFM`
package](https://github.com/aciancetta/emDFM). The user can select which
mobility indexes should be introduced in the model, and whether the
indexes should be inserted as an aggregated monthly time series or
directly as a daily indicator. In the latter case, a column for each day
of the month is created in the dataset, each with one observation per
month. The app also allows to choose between the four most common
estimation methods for DFMs: PCA+VAR, Kalman filter, Kalman smoother, EM
algorithm. The results of a sliding-window forecast evaluation can also
be plotted to study the model’s forecast accuracy.

## Installation

You can install the development version of nowcastapp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aciancetta/nowcastapp")
```

## Launch the app

``` r
# devtools::install_github("aciancetta/nowcastapp")
library(nowcastapp)
nowcastapp::app()
```

# Credits

The package and the app have been developed during my internship at
[IRPET](http://www.irpet.it/) (Regional Institute for Economic Planning
of Tuscany).
