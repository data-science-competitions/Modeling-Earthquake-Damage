
# `template.project` <img src='https://i.imgur.com/cLcAYfz.png' align="right" height="50"/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/data-science-competitions/template.project.svg?branch=master)](https://travis-ci.org/data-science-competitions/template.project)
[![Code coverage
status](https://codecov.io/gh/data-science-competitions/template.project/branch/master/graph/badge.svg)](https://codecov.io/github/data-science-competitions/template.project/?branch=master)
[![Launch Rstudio
Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/data-science-competitions/template.project/master?urlpath=rstudio)
<!-- badges: end -->

<!-- Package Title -->

R Project Template for Analytic
Applications

-----

<img src="https://i.imgur.com/RLEQkhe.png" width="100%" style="display: block; margin: auto;" />

<!-- Package Description -->

## Description

Using this template reduces:  
\* Unnecessary variance between projects configurations; and  
\* Development time spent on making a barebone project working for the
first time.  
This is possible as the boilerplate comes with:  
\* Fully configured test-suite, including code-coverage; and  
\* Fully configured continuous-integration (CI) script for Travis.

## Useage

1.  Create a new repo on GitHub.
2.  Use the
    [`git-flow`](https://blog.sourcetreeapp.com/2012/08/01/smart-branching-with-sourcetree-and-git-flow/)
    approach in your development cycle.
3.  Create a new release named `inception`.
4.  Copy `template.project` content to the new reposetory.
5.  Change the `template.project.Rproj` file to `<package-name>.Rproj`.
6.  Open the `DESCRIPTION` file, and edit the following fields:
7.  **Package** modify the package name while using the `tidylab.`
    prefix.
8.  **Title** modify the package title; use uppercase words with no
    period (‘.’).
9.  **URL** modify the package URL such that it leads to its GitHub
    repo.
10. **BugReports** edit the URL such that it leads to the package issue
    page.
11. **Description** modify the package decription.
12. In `README.Rmd` delete the **Useage** Section.
13. Render `README.Rmd` by clicking the **Knit** button.
14. Push changed on the `inception` branch.
15. Go to [Travis website](https://travis-ci.org/account/repositories),
    add the project and enable its integration.

## Installation

`template.project` accommodates two stages in the project life-cycle:
Development and Production.

### Working in Pseudo-Package Mode (Advised During Development Stage)

1.  Download the project to local computer
2.  Launch the project via `template.project.Rproj`
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
    devtools::install_github("data-science-competitions/template.project")

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
