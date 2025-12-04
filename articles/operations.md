# Operations

## Introduction

This vignette introduces the generic functions and operations supported
by `dbMatrix`.

## Loading library

``` r
library(dbMatrix)
library(Matrix)
```

## dbMatrix generics

`dbMatrix` objects currently support several statistical matrix
operations listed below with support for more coming soon.

✅ - implemented 🟧 - not yet implemented

|          | dbSparseMatrix | dbDenseMatrix |
|----------|----------------|---------------|
| colSums  | ✅             | ✅            |
| rowSums  | ✅             | ✅            |
| colMeans | ✅             | ✅            |
| rowMeans | ✅             | ✅            |
| colSds   | 🟧             | ✅            |
| rowSds   | 🟧             | ✅            |
| t        | ✅             | ✅            |
| mean     | ✅             | ✅            |
| nrow     | ✅             | ✅            |
| ncol     | ✅             | ✅            |
| dims     | ✅             | ✅            |
| head     | ✅             | ✅            |
| tail     | ✅             | ✅            |
| …        |                |               |

## dbSparse Matrix Operations

### Get test data

The test file is a `dgCMatrix`or compressed sparse column matrix
representing a single cell gene expression matrix. The file is in the
`data` directory of the package.

Let’s load the .rds file and preview the object.

``` r
dgc <- readRDS("../data/dgc.rds")

dplyr::glimpse(dgc)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:170625] 0 6 10 17 21 22 25 31 33 35 ...
    ##   ..@ p       : int [1:625] 0 227 510 758 980 1293 1631 1976 2223 2434 ...
    ##   ..@ Dim     : int [1:2] 634 624
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:634] "Gna12" "Ccnd2" "Btbd17" "Sox9" ...
    ##   .. ..$ : chr [1:624] "AAAGGGATGTAGCAAG-1" "AAATGGCATGTCTTGT-1" "AAATGGTCAATGTGCC-1" "AAATTAACGGGTAGCT-1" ...
    ##   ..@ x       : num [1:170625] 1 1 1 1 1 1 6 2 1 1 ...
    ##   ..@ factors : list()

``` r
# create dbSparseMatrix from the same dgc
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

sparse <- dbMatrix(value = dgc, 
                   con = con, 
                   name = 'visium', 
                   class = "dbSparseMatrix",
                   overwrite = TRUE)

# preview 
# show function aims to emulate the show method for dgCMatrix
head(sparse)
```

    ## 6 x 624  dbMatrix of class "dbSparseMatrix"

    ## [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]

    ##                                                                               
    ## Gna12    1.0000000 2.0000000 1.0000000 1.0000000 9.0000000 1.0000000 3.0000000
    ##                                       
    ## Gna12    5.0000000 3.0000000         .
    ##  [ reached 'max' / getOption("max.print") -- omitted 5 rows ]

### transpose

``` r
dbMatrix::t(sparse)
```

    ## 624 x 634  dbMatrix of class "dbSparseMatrix"

    ## [[ Colnames 'Gna12', 'Ccnd2', 'Btbd17' ... suppressing 628 ...'Gm19935', '9630013A20Rik', '2900040C04Rik' ]]

    ##                                                                       
    ## AAAGGGATGTAGCAAG-1 1.0000000         .         .         .         . .
    ##                                                   
    ## AAAGGGATGTAGCAAG-1 1.0000000 .         .         .
    ## 
    ## ......suppressing 624 columns and 618 rows
    ## 
    ##  [ reached 'max' / getOption("max.print") -- omitted 5 rows ]
    ## NA
    ## NA

### colMeans

``` r
dbMatrix::colMeans(sparse)
```

    ## 624 x 1 dbMatrix of class "dbDenseMatrix"
    ##                             
    ## AAAGGGATGTAGCAAG-1 0.7413249
    ## AAATGGCATGTCTTGT-1 1.3296530
    ## AAATGGTCAATGTGCC-1 1.1435331
    ## 
    ## ...suppressing 618 elements
    ## 
    ## TTGTCGTTCAGTTACC-1 0.6624606
    ## TTGTGGCCCTGACAGT-1 0.6908517
    ## TTGTTCAGTGTGCTAC-1 0.7965300

### colSums

``` r
dbMatrix::colSums(sparse)
```

    ## 624 x 1 dbMatrix of class "dbDenseMatrix"
    ##                               
    ## AAAGGGATGTAGCAAG-1 470.0000000
    ## AAATGGCATGTCTTGT-1 843.0000000
    ## AAATGGTCAATGTGCC-1 725.0000000
    ## 
    ## ...suppressing 618 elements
    ## 
    ## TTGTCGTTCAGTTACC-1 420.0000000
    ## TTGTGGCCCTGACAGT-1 438.0000000
    ## TTGTTCAGTGTGCTAC-1 505.0000000

### rowMeans

``` r
dbMatrix::rowMeans(sparse)
```

    ## 634 x 1 dbMatrix of class "dbDenseMatrix"
    ##                        
    ## Gna12         2.7179487
    ## Ccnd2         1.7323718
    ## Btbd17        0.5528846
    ## 
    ## ...suppressing 628 elements
    ## 
    ## Gm19935       0.2083333
    ## 9630013A20Rik 0.1714744
    ## 2900040C04Rik 0.1554487

### rowSums

``` r
dbMatrix::rowSums(sparse)
```

    ## 634 x 1 dbMatrix of class "dbDenseMatrix"
    ##                           
    ## Gna12         1696.0000000
    ## Ccnd2         1081.0000000
    ## Btbd17         345.0000000
    ## 
    ## ...suppressing 628 elements
    ## 
    ## Gm19935        130.0000000
    ## 9630013A20Rik  107.0000000
    ## 2900040C04Rik   97.0000000

### dim

``` r
dim(sparse)
```

    ## [1] 634 624

``` r
dim(dgc)
```

    ## [1] 634 624

### Check results are equivalent

Click to expand

``` r
  all.equal(dbMatrix::colMeans(sparse, memory = TRUE, names = TRUE), Matrix::colMeans(dgc))
```

      ## [1] TRUE

``` r
  all.equal(dbMatrix::colSums(sparse, memory = TRUE, names = TRUE), Matrix::colSums(dgc))
```

      ## [1] TRUE

``` r
  all.equal(dbMatrix::rowMeans(sparse, memory = TRUE, names = TRUE), Matrix::rowMeans(dgc))
```

      ## [1] TRUE

``` r
  all.equal(dbMatrix::rowSums(sparse, memory = TRUE, names = TRUE), Matrix::rowSums(dgc))
```

      ## [1] TRUE

## dbDenseMatrix Operations

``` r
# below is a convenience function to simulate a dbDenseMatrix
dense = dbMatrix::sim_dbDenseMatrix()

# preview
dense
```

    ## 50 x 50  dbMatrix of class "dbDenseMatrix"

    ##                                                                        
    ## row1   1.3709584  0.3219253  1.2009654 -0.0406985 -2.0009292 -1.0961562
    ##                                                  
    ## row1  -0.0046208  0.7241738  1.3349126 -1.3038212
    ## 
    ## ......suppressing 40 columns and 44 rows
    ## 
    ##  [ reached 'max' / getOption("max.print") -- omitted 5 rows ]
    ## NA
    ## NA

### transpose

``` r
dbMatrix::t(dense)
```

    ## 50 x 50  dbMatrix of class "dbDenseMatrix"

    ## [[ Colnames 'row1', 'row2', 'row3' ... suppressing 44 ...'row48', 'row49', 'row50' ]]

    ##                                                                        
    ## col1   1.3709584 -0.5646982  0.3631284  0.6328626  0.4042683 -0.1061245
    ##                                                  
    ## col1   1.5115220 -0.0946590  2.0184237 -0.0627141
    ## 
    ## ......suppressing 40 columns and 44 rows
    ## 
    ##  [ reached 'max' / getOption("max.print") -- omitted 5 rows ]
    ## NA
    ## NA

### colMeans

``` r
dbMatrix::colMeans(dense)
```

    ## 50 x 1 dbMatrix of class "dbDenseMatrix"
    ##                 
    ## col1  -0.0356718
    ## col2   0.1007014
    ## col3  -0.1512511
    ## 
    ## ...suppressing 44 elements
    ## 
    ## col48  0.0178273
    ## col49  0.2073369
    ## col50 -0.1635492

### colSums

``` r
dbMatrix::colSums(dense)
```

    ## 50 x 1 dbMatrix of class "dbDenseMatrix"
    ##                 
    ## col1  -1.7835891
    ## col2   5.0350707
    ## col3  -7.5625544
    ## 
    ## ...suppressing 44 elements
    ## 
    ## col48  0.8913639
    ## col49 10.3668441
    ## col50 -8.1774605

### rowMeans

``` r
dbMatrix::rowMeans(dense)
```

    ## 50 x 1 dbMatrix of class "dbDenseMatrix"
    ##                 
    ## row1   0.0536436
    ## row2   0.0024946
    ## row3   0.0313340
    ## 
    ## ...suppressing 44 elements
    ## 
    ## row48 -0.1010421
    ## row49  0.0592996
    ## row50  0.0215431

### rowSums

``` r
dbMatrix::rowSums(dense)
```

    ## 50 x 1 dbMatrix of class "dbDenseMatrix"
    ##                 
    ## row1   2.6821819
    ## row2   0.1247321
    ## row3   1.5667017
    ## 
    ## ...suppressing 44 elements
    ## 
    ## row48 -5.0521070
    ## row49  2.9649814
    ## row50  1.0771549

### mean

``` r
dbMatrix::mean(dense)
```

    ## [1] -0.009673541

### dim

``` r
dim(dense)
```

    ## [1] 50 50

## Session Info

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8    LC_NUMERIC=C        LC_TIME=C.UTF-8    
    ##  [4] LC_COLLATE=C.UTF-8  LC_MONETARY=C.UTF-8 LC_MESSAGES=C.UTF-8
    ##  [7] LC_PAPER=C.UTF-8    LC_NAME=C           LC_ADDRESS=C       
    ## [10] LC_TELEPHONE=C     
    ##  [ reached 'max' / getOption("max.print") -- omitted 2 entries ]
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] Matrix_1.7-4        dbMatrix_0.0.0.9124
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] jsonlite_2.0.0       dplyr_1.1.4          compiler_4.5.2      
    ##  [4] tidyselect_1.2.1     blob_1.2.4           dbProject_0.0.0.9000
    ##  [7] jquerylib_0.1.4      systemfonts_1.3.1    textshaping_1.0.4   
    ## [10] yaml_2.3.11         
    ##  [ reached 'max' / getOption("max.print") -- omitted 36 entries ]
