
# `modeling.earthquake.damage` <img src='https://i.imgur.com/m7lDBGD.png' align="right" height="45"/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/data-science-competitions/modeling.earthquake.damage.svg?branch=master)](https://travis-ci.org/data-science-competitions/modeling.earthquake.damage)
[![Code coverage
status](https://codecov.io/gh/data-science-competitions/modeling.earthquake.damage/branch/master/graph/badge.svg)](https://codecov.io/github/data-science-competitions/modeling.earthquake.damage/?branch=master)
[![Launch Rstudio
Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/data-science-competitions/modeling.earthquake.damage/master?urlpath=rstudio)
<!-- badges: end -->

<!-- Package Title -->

Richter’s Predictor: Modeling Earthquake
Damage

-----

<img src="https://i.imgur.com/2sfOQSa.png" width="100%" style="display: block; margin: auto;" />

<!-- Package Description -->

## Description

Predict the level of damage to buildings that were hit by the Gorkha
earthquake.

## Installation

`modeling.earthquake.damage` accommodates two stages in the project
life-cycle: Development and Production.

### Working in Pseudo-Package Mode (Advised During Development Stage)

1.  Download the project to local computer
2.  Launch the project via `modeling.earthquake.damage.Rproj`
3.  Optional: Install all package
dependencies

<!-- end list -->

``` r
remotes::install_local(dependencies = TRUE, force = TRUE, upgrade = "always")
devtools::uninstall()
```

4.  Load the complete project as a package via `devtools::load_all()` or
    Ctrl+Shift+L

### Working in Package Mode (Advised During Production Stage)

    install.packages("devtools")
    devtools::install_github("data-science-competitions/modeling.earthquake.damage")

## Data Pipeline

The template includes a database abstraction layer (DAL) that separates
data sources and analytic applications. The DAL has three stages with
the following functionality:

1.  `Ingest`

<!-- end list -->

  - Pull data from external sources; and
  - Matching schema, organizing, indexing, encoding and compressing the
    data.

<!-- end list -->

2.  `Prepare`

<!-- end list -->

  - Type conversion of variables if necessary; and
  - Data cleansing;

<!-- end list -->

3.  `Store`

<!-- end list -->

  - Create a data model if relational tables exist;
  - Introduce new features and ready-for-modelling tables; and
  - Make the data available for query.

The template provides skeletons for `Ingest`, `Prepare` and `Store`
interfaces. In addition, the template includes a toy example which
demonstrates the data flow between all three interfaces.

<!--
### Ingest Layer Interface and its Implementation


```r
ingest_abstract <- Ingest$new(path = tempdir())
class(ingest_abstract)

ingest_concrete <- IngestDAO$new(path = tempdir())
class(ingest_concrete)
```

### Prepare Layer Interface and its Implementation


```r
prepare_abstract <- Prepare$new()
class(prepare_abstract)

prepare_concrete <- PrepareDAO$new()
class(prepare_concrete)
```

### DataStore Object


```r
ds <- DataStore$new()
class(ds)
```
-->

## Datasets

### Accessing the Project Data

``` r
ds <- DataStore$new()
ds$data_model %>% dm::cdm_get_tables() %>% str()
```

    ## List of 3
    ##  $ historical_data  :Classes 'tbl_df', 'tbl' and 'data.frame':   22 obs. of  12 variables:
    ##   ..$ UID : chr [1:22] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
    ##   ..$ MPG : num [1:22] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
    ##   ..$ CYL : num [1:22] 6 6 4 6 8 6 8 4 4 6 ...
    ##   ..$ DISP: num [1:22] 160 160 108 258 360 ...
    ##   ..$ HP  : num [1:22] 110 110 93 110 175 105 245 62 95 123 ...
    ##   ..$ DRAT: num [1:22] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
    ##   ..$ WT  : num [1:22] 2.62 2.88 2.32 3.21 3.44 ...
    ##   ..$ QSEC: num [1:22] 16.5 17 18.6 19.4 17 ...
    ##   ..$ VS  : num [1:22] 0 0 1 1 0 1 0 1 1 1 ...
    ##   ..$ AM  : num [1:22] 1 1 1 0 0 0 0 0 0 0 ...
    ##   ..$ GEAR: num [1:22] 4 4 4 3 3 3 3 4 4 4 ...
    ##   ..$ CARB: num [1:22] 4 4 1 1 2 1 4 2 2 4 ...
    ##  $ new_data         :Classes 'tbl_df', 'tbl' and 'data.frame':   10 obs. of  12 variables:
    ##   ..$ UID : chr [1:10] "AMC Javelin" "Camaro Z28" "Pontiac Firebird" "Fiat X1-9" ...
    ##   ..$ MPG : num [1:10] 15.2 13.3 19.2 27.3 26 30.4 15.8 19.7 15 21.4
    ##   ..$ CYL : num [1:10] 8 8 8 4 4 4 8 6 8 4
    ##   ..$ DISP: num [1:10] 304 350 400 79 120 ...
    ##   ..$ HP  : num [1:10] 150 245 175 66 91 113 264 175 335 109
    ##   ..$ DRAT: num [1:10] 3.15 3.73 3.08 4.08 4.43 3.77 4.22 3.62 3.54 4.11
    ##   ..$ WT  : num [1:10] 3.44 3.84 3.85 1.94 2.14 ...
    ##   ..$ QSEC: num [1:10] 17.3 15.4 17.1 18.9 16.7 ...
    ##   ..$ VS  : num [1:10] 0 0 0 1 0 1 0 0 0 1
    ##   ..$ AM  : num [1:10] 0 0 0 1 1 1 1 1 1 1
    ##   ..$ GEAR: num [1:10] 3 3 3 4 5 5 5 5 5 4
    ##   ..$ CARB: num [1:10] 2 4 2 1 2 2 4 6 8 2
    ##  $ submission_sample:Classes 'tbl_df', 'tbl' and 'data.frame':   10 obs. of  1 variable:
    ##   ..$ UID: chr [1:10] "AMC Javelin" "Camaro Z28" "Pontiac Firebird" "Fiat X1-9" ...

### Data Overview

<img src="README_files/figure-gfm/project-data-overview-1.png" width="100%" style="display: block; margin: auto;" />

<!--
### Data Model


-->

<!--
## Function Dependencies


-->
