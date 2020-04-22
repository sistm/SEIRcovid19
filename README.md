
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEIRcovid19FR

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install `SEIRcovid19FR` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sistm/SEIRcovid19")
```

## Updating the Hospitalization data for Monolix :

1.  download the new SurSaUD file in `data/raw` from
    [data.gouv](https://www.data.gouv.fr/fr/datasets/r/941ff2b4-ea24-4cdf-b0a7-655f2a332fb2)
2.  update the file name and run the commented code in example of
    `R/sursaud_covid19.R`
3.  reload the package
4.  run the example script of `R/prepare_data_4monolix.R`

Mélanie Prague, Dan Dutartre & Boris Hejblum
