
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Transparency & Equity in Property Assessments

This package analyzes the accuracy of property assessments and produces
graphs, tables, and reports designed for general use. This package is
produced by the [Center for Municipal
Finance](https://harris.uchicago.edu/research-impact/centers-institutes/center-municipal-finance),
a research institute at the Harris School of Public Policy, University
of Chicago.

For a detailed guide on how to use this package check out our [**Get
started
page**](https://cmf-uchicago.github.io/cmfproperty/articles/cmfproperty.html).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cmf-uchicago/cmfproperty")
```

## Basic Usage

First import `cmfproperty`.

``` r
library(cmfproperty)
```

To conduct our study, we need data where every roll is a unique
property’s sale price and assessed value for a given year.

``` r
head(cmfproperty::example_data)
#>              PIN SALE_YEAR SALE_PRICE ASSESSED_VALUE
#> 1 17273100931118      2015      53000          33860
#> 2 18013090421010      2018      80000          60390
#> 3 12111190201042      2018     118000         108300
#> 4 13093160601015      2017     125500          87200
#> 5 14322110150000      2018    3705000        3670740
#> 6 27021200080000      2016     345000         267280
```

Then, preprocess your data with `reformat_data` and call `make_report`.
The report from the example below can be found
[here](https://cmf-uchicago.github.io/Cook%20County,%20Illinois.html).

``` r
df <- cmfproperty::example_data

ratios <-
  cmfproperty::reformat_data(
    df,
    sale_col = "SALE_PRICE",
    assessment_col = "ASSESSED_VALUE",
    sale_year_col = "SALE_YEAR",
  )

cmfproperty::make_report(ratios, 
                         jurisdiction_name = "Cook County, Illinois",
                         output_dir = "~/../Documents/GitHub/cmf-uchicago.github.io/") 

#output_dir is the directory in which report is saved; default is working directory
```
