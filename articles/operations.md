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

### Create test data

Let’s create a sparse matrix for demonstration:

``` r
set.seed(42)
dgc <- Matrix::rsparsematrix(100, 50, density = 0.1, rand.x = function(n) rpois(n, 5) + 1)
rownames(dgc) <- paste0("gene_", seq_len(100))
colnames(dgc) <- paste0("cell_", seq_len(50))

dplyr::glimpse(dgc)
```

    ## Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   ..@ i       : int [1:500] 1 7 15 26 32 38 48 81 90 93 ...
    ##   ..@ p       : int [1:51] 0 12 22 33 38 46 54 66 80 89 ...
    ##   ..@ Dim     : int [1:2] 100 50
    ##   ..@ Dimnames:List of 2
    ##   .. ..$ : chr [1:100] "gene_1" "gene_2" "gene_3" "gene_4" ...
    ##   .. ..$ : chr [1:50] "cell_1" "cell_2" "cell_3" "cell_4" ...
    ##   ..@ x       : num [1:500] 4 7 5 7 3 8 4 5 4 7 ...
    ##   ..@ factors : list()

``` r
# create dbSparseMatrix from the same dgc
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

sparse <- dbMatrix(
  value = dgc,
  con = con,
  name = "test_matrix",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# preview
# show function aims to emulate the show method for dgCMatrix
head(sparse)
```

    ## 6 x 50  dbMatrix of class "dbSparseMatrix"

    ## [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]

    ##                                                                   
    ## gene_1         .         . . . .         . 5.0000000         . . .
    ## gene_2 4.0000000         . . . .         .         .         . . .
    ## gene_3         . 7.0000000 . . . 8.0000000         . 3.0000000 . .
    ## gene_4         .         . . . .         .         .         . . .
    ## gene_5         .         . . . .         . 6.0000000 5.0000000 . .
    ## gene_6         .         . . . .         .         .         . . .

### transpose

``` r
dbMatrix::t(sparse)
```

    ## 50 x 100  dbMatrix of class "dbSparseMatrix"

    ## [[ Colnames 'gene_1', 'gene_2', 'gene_3' ... suppressing 94 ...'gene_98', 'gene_99', 'gene_100' ]]

    ##                                                                            
    ## cell_1  . 4.0000000         . . .         .         . 7.0000000 .         .
    ## cell_2  .         . 7.0000000 . .         .         .         . . 7.0000000
    ## cell_3  .         .         . . .         .         .         . .         .
    ## 
    ## ......suppressing 90 columns and 44 rows
    ## 
    ## cell_48 .         .         . . .         . 8.0000000         . .         .
    ## cell_49 .         .         . . . 2.0000000         . 8.0000000 .         .
    ## cell_50 .         .         . . .         .         .         . .         .

### colMeans

``` r
dbMatrix::colMeans(sparse)
```

    ##  cell_1  cell_2  cell_3  cell_4  cell_5  cell_6  cell_7  cell_8  cell_9 cell_10 
    ##    0.66    0.63    0.62    0.24    0.48    0.45    0.66    0.69    0.57    0.77 
    ##  [ reached 'max' / getOption("max.print") -- omitted 40 entries ]

### colSums

``` r
dbMatrix::colSums(sparse)
```

    ##  cell_1  cell_2  cell_3  cell_4  cell_5  cell_6  cell_7  cell_8  cell_9 cell_10 
    ##      66      63      62      24      48      45      66      69      57      77 
    ##  [ reached 'max' / getOption("max.print") -- omitted 40 entries ]

### rowMeans

``` r
dbMatrix::rowMeans(sparse)
```

    ##  gene_1  gene_2  gene_3  gene_4  gene_5  gene_6  gene_7  gene_8  gene_9 gene_10 
    ##    0.50    0.20    0.68    0.08    0.32    0.24    0.58    0.76    0.88    0.62 
    ##  [ reached 'max' / getOption("max.print") -- omitted 90 entries ]

### rowSums

``` r
dbMatrix::rowSums(sparse)
```

    ##  gene_1  gene_2  gene_3  gene_4  gene_5  gene_6  gene_7  gene_8  gene_9 gene_10 
    ##      25      10      34       4      16      12      29      38      44      31 
    ##  [ reached 'max' / getOption("max.print") -- omitted 90 entries ]

### dim

``` r
dim(sparse)
```

    ## [1] 100  50

``` r
dim(dgc)
```

    ## [1] 100  50

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
# Create a dense matrix directly
set.seed(42)
mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
rownames(mat) <- paste0("row_", 1:10)
colnames(mat) <- paste0("col_", 1:10)

# Create dbDenseMatrix
con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
dense <- dbMatrix(
  value = mat,
  con = con2,
  name = "dense_matrix",
  class = "dbDenseMatrix",
  overwrite = TRUE
)

# preview
dense
```

    ## 10 x 10  dbMatrix of class "dbDenseMatrix"

    ##                                                                         
    ## row_1   1.3709584  1.3048697 -0.3066386  0.4554501  0.2059986  0.3219253
    ## row_2  -0.5646982  2.2866454 -1.7813084  0.7048373 -0.3610573 -0.7838389
    ## row_3   0.3631284 -1.3888607 -0.1719174  1.0351035  0.7581632  1.5757275
    ## 
    ## ...... suppressing 4 rows ......
    ## 
    ## row_8  -0.0946590 -2.6564554 -1.7631631 -0.8509076  1.4441013  0.0898329
    ## row_9   2.0184237 -2.4404669  0.4600974 -2.4142076 -0.4314462 -2.9930901
    ## row_10 -0.0627141  1.3201133 -0.6399949  0.0361226  0.6556479  0.2848830

### transpose

``` r
dbMatrix::t(dense)
```

    ## 10 x 10  dbMatrix of class "dbDenseMatrix"

    ## [[ Colnames 'row_1', 'row_2', 'row_3' ... suppressing 4 ...'row_8', 'row_9', 'row_10' ]]

    ##                                                                         
    ## col_1   1.3709584 -0.5646982  0.3631284  0.6328626  0.4042683 -0.1061245
    ## col_2   1.3048697  2.2866454 -1.3888607 -0.2787888 -0.1333213  0.6359504
    ## col_3  -0.3066386 -1.7813084 -0.1719174  1.2146747  1.8951935 -0.4304691
    ## 
    ## ...... suppressing 4 rows ......
    ## 
    ## col_8  -1.0431189 -0.0901864  0.6235182 -0.9535234 -0.5428288  0.5809965
    ## col_9   1.5127070  0.2579214  0.0884402 -0.1208965 -1.1943289  0.6119969
    ## col_10  1.3921164 -0.4761739  0.6503486  1.3911105 -1.1107889 -0.8607926

### colMeans

``` r
dbMatrix::colMeans(dense)
```

    ##       col_1       col_2       col_3       col_4       col_5       col_6 
    ##  0.54729677 -0.16345673 -0.17807953 -0.36390406 -0.02021535  0.01839391 
    ##       col_7       col_8       col_9      col_10 
    ##  0.53907680 -0.21787537  0.25110630 -0.08719458

### colSums

``` r
dbMatrix::colSums(dense)
```

    ##      col_1      col_2      col_3      col_4      col_5      col_6      col_7 
    ##  5.4729677 -1.6345673 -1.7807953 -3.6390406 -0.2021535  0.1839391  5.3907680 
    ##      col_8      col_9     col_10 
    ## -2.1787537  2.5110630 -0.8719458

### rowMeans

``` r
dbMatrix::rowMeans(dense)
```

    ##       row_1       row_2       row_3       row_4       row_5       row_6 
    ##  0.48470333 -0.06226284  0.41154753  0.25924440 -0.21826635  0.07264603 
    ##       row_7       row_8       row_9      row_10 
    ## -0.01914153 -0.39709480 -0.47524086  0.26901325

### rowSums

``` r
dbMatrix::rowSums(dense)
```

    ##      row_1      row_2      row_3      row_4      row_5      row_6      row_7 
    ##  4.8470333 -0.6226284  4.1154753  2.5924440 -2.1826635  0.7264603 -0.1914153 
    ##      row_8      row_9     row_10 
    ## -3.9709480 -4.7524086  2.6901325

### mean

``` r
dbMatrix::mean(dense)
```

    ## [1] 0.03251482

### dim

``` r
dim(dense)
```

    ## [1] 10 10

## Cleanup

``` r
DBI::dbDisconnect(con, shutdown = TRUE)
DBI::dbDisconnect(con2, shutdown = TRUE)
```

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
    ## [1] Matrix_1.7-4        dbMatrix_0.0.0.9126
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] bit_4.6.0         jsonlite_2.0.0    dplyr_1.1.4       compiler_4.5.2   
    ##  [5] tidyselect_1.2.1  Rcpp_1.1.1        blob_1.2.4        nanoarrow_0.7.0-2
    ##  [9] pins_1.4.1        assertthat_0.2.1 
    ##  [ reached 'max' / getOption("max.print") -- omitted 45 entries ]
