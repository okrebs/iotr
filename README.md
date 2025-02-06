
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iotr

<!-- badges: start -->

[![R-CMD-check](https://github.com/okrebs/iotr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/okrebs/iotr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

Package for working with Input Output Tables in quantitative economics.

Download and convert I-O-data from different sources.

Currently supported databases:

- OECD’s ICIO v2 (see OECD 2023):
  - download tables
  - convert to standardized long format
- OECD’s ICIO (see OECD 2021):
  - download tables
  - convert to standardized long format
- WIOD (see Timmer et al. 2015):
  - download tables
  - convert to standardized long format
- Eora26 (see Lenzen et al. 2012, 2013; EORA account/data required)
  - download tables
  - convert to standardized long format

Planned support:

- USITC ITPD-IO
- Eurostat
- RHOMOLO
- Full EORA

Additional functionality:

- remove ‘dynamic’ demand categories, e.g. inventory or GFCF, following
  the method proposed in Costino and Rodríguez-Clare (2014)

- remove entries with negative value added from and IO-table by scaling
  demand

- remove entries where exports but no “own trade” exists by replacing
  them with small entries

References: - OECD (2023) OECD Inter-Country Input-Output Tables.
<http://oe.cd/icio>

- OECD (2021). OECD Inter-Country Input-Output Database.
  <http://oe.cd/icio>

- Timmer, Marcel P., Dietzenbacher, Erik, Los, Bart, Stehrer, Robert and
  de Vries, Gaaitzen J. (2015). An Illustrated User Guide to the World
  Input–Output Database: the Case of Global Automotive Production.
  Review of International Economics, 23: 575–605.

- Costinot, Arnaud and Rodríguez-Clare, Andrés (2014). Trade Theory with
  Numbers: Quantifying the Consequences of Globalization. Handbook of
  International Economics, 4:197:261.

- Lenzen M, Kanemoto K; Moran D, and Geschke A (2012) Mapping the
  structure of the world economy. Environmental Science & Technology 46(15)
  pp 8374–8381.

- Lenzen, M., Moran, D., Kanemoto, K., Geschke, A. (2013) Building Eora: A
  Global Multi-regional Input-Output Database at High Country and Sector
  Resolution. Economic Systems Research, 25:1, 20-49.

## Installation

<!-- You can install the released version of iotr from [CRAN](https://CRAN.R-project.org) with:
&#10;``` r
install.packages("iotr")
``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("okrebs/iotr")
```

Early development version, all functionality is subject to change!

## Examples

See “man/examples/wiod.R”, “man/examples/icio.R”, and
“man/examples/icio_v2.R

``` r
library(iotr)
## basic example code
```
