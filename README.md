
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LGrafEU

<!-- badges: start -->

[![R-CMD-check](https://github.com/pogoyoly/LGrafEU/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pogoyoly/LGrafEU/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/pogoyoly/LGrafEU/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pogoyoly/LGrafEU?branch=master)
[![CircleCI build
status](https://circleci.com/gh/pogoyoly/LGrafEU.svg?style=svg)](https://circleci.com/gh/pogoyoly/LGrafEU)
<!-- badges: end -->

LGrafEU is an artificial landcover generator that is designed to ease
the integration of artificial agricultural landcover maps in the work
flow of ecological modellers. The package is designed to generate
landscapes using different algorithms, and then store the information in
an output file that includes both a raster layer, and a list containing
all information of the fields. This package allows to generate
landscapes in a systematic reproducable way while controlling for
different variables.

## Installation

You can install the development version of LGrafEU from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pogoyoly/LGrafEU")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LGrafEU)
## basic example code
r<-generate_perlin_noise(200,200,1,2,3,0.01,TRUE, "land_percentage", percetange = 90)
output<-establish_by_place_conquer(potential_space= r,
                         cell_size=1,
                         includsion_value = 1,
                         mean_field_size = 500,
                         sd_field_size = 100,
                         distribution = "norm",
                         mean_shape_index = 1,
                         sd_shape_index = 0.3,
                         percent = 90,
                         assign_farmers = TRUE,
                         assign_mode = 2,
                         mean_fields_per_farm = 3,
                         sd_fields_per_farm = 3)


return_by_field(output)
```

<img src="man/figures/README-example-1.png" width="100%" />
