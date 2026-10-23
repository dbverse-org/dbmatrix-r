# dbMatrix

[dbMatrix](https://github.com/dbverse-org/dbmatrix-r) is a core package
in the [`dbverse`](https://dbverse-org.github.io/dbverse/) library.

The goal of [dbMatrix](https://github.com/dbverse-org/dbmatrix-r) is to
support larger-than-memory matrix computations.

**Note: Work in progress! Bugs or unexpected behavior are likely to
occur. We welcome user feedback and reporting issues on the [Github
page](https://github.com/dbverse-org/dbmatrix-r/).** \## Installation

Install the released version from CRAN:

``` r

install.packages("dbMatrix")
```

Or the development version from GitHub:

``` r

# install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
pak::pak("dbverse-org/dbmatrix-r")
```
