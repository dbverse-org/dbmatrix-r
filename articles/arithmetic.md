# Arithmetic

``` r

library(dbMatrix)
```

## dbMatrix arithmetic

`dbMatrix` objects support `Arith` and `Ops` operations. We will
demonstrate how to perform arithmetic operations on `dbSparseMatrix`
objects.

**Note on sparse matrices:** a `dbSparseMatrix` stores only non-zero
values. Operations that turn 0 into something else, such as `x + 1`,
`1 / x`, `exp(x)` or `x == 0`, first fill in the zeros, which can use a
lot of space for large matrices. This only happens when
`options(dbMatrix.allow_densify = TRUE)` is set; otherwise these
operations give an error. [`log()`](https://rdrr.io/r/base/Log.html),
[`log2()`](https://rdrr.io/r/base/Log.html) and
[`log10()`](https://rdrr.io/r/base/Log.html) are the exception: they
transform only the stored values and leave zeros at 0. See
[`?dbMatrix_options`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix_options.md)
and `?Math,dbMatrix-method`.

Please report problems on
[GitHub](https://github.com/dbverse-org/dbmatrix-r/issues).

### Create test data

Let’s create a simple sparse matrix for demonstration:

``` r

# Create a sparse matrix
set.seed(42)
dgc <- Matrix::rsparsematrix(100, 50, density = 0.1, rand.x = function(n) rpois(n, 5) + 1)
rownames(dgc) <- paste0("gene_", seq_len(100))
colnames(dgc) <- paste0("cell_", seq_len(50))

dplyr::glimpse(dgc)
#> Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
#>   ..@ i       : int [1:500] 1 7 15 26 32 38 48 81 90 93 ...
#>   ..@ p       : int [1:51] 0 12 22 33 38 46 54 66 80 89 ...
#>   ..@ Dim     : int [1:2] 100 50
#>   ..@ Dimnames:List of 2
#>   .. ..$ : chr [1:100] "gene_1" "gene_2" "gene_3" "gene_4" ...
#>   .. ..$ : chr [1:50] "cell_1" "cell_2" "cell_3" "cell_4" ...
#>   ..@ x       : num [1:500] 4 7 5 7 3 8 4 5 4 7 ...
#>   ..@ factors : list()
```

The matrix contains 100 rows (genes) and 50 columns (cells). Like most
single-cell RNA-seq data, the matrix is sparse.

### Create a dbMatrix object

Let’s create a `dbSparseMatrix` object from the above `dgc` object.

``` r

# Note: by default the constructor creates a dbMatrix object in-memory
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/Rtmp8ZL3eF/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.

dbsm <- dbMatrix(
  value = dgc,
  con = con,
  name = "test_matrix",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# preview the object
dbsm
#> 100 x 50  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]
#>                                                                         
#> gene_1           .         .         . . .         . 5.0000000         .
#> gene_2   4.0000000         .         . . .         .         .         .
#> gene_3           . 7.0000000         . . . 8.0000000         . 3.0000000
#> 
#> ......suppressing 40 columns and 94 rows
#> 
#> gene_98          .         .         . . .         .         .         .
#> gene_99          .         . 5.0000000 . .         .         .         .
#> gene_100 7.0000000         .         . . .         .         .         .
```

### Scalar Arithmetic

`dbMatrix` emulates scalar arithmetic in the `Matrix` package.

Note: Addition or subtraction with non-zero addends on a
`dbSparseMatrix` results in a `dbDenseMatrix`.

``` r

dbsm + 1
#> ℹ Performing on-the-fly densification (cold path). See ?dbMatrix_options for details.
#> 100 x 50  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]
#>                                                                               
#> gene_1   1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 6.0000000
#> gene_2   5.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
#> gene_3   1.0000000 8.0000000 1.0000000 1.0000000 1.0000000 9.0000000 1.0000000
#> 
#> ......suppressing 40 columns and 94 rows
#> 
#> gene_98  1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
#> gene_99  1.0000000 1.0000000 6.0000000 1.0000000 1.0000000 1.0000000 1.0000000
#> gene_100 8.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000

dbsm * 100
#> 100 x 50  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]
#>                                                                         
#> gene_1             .           .           . . .           . 500.0000000
#> gene_2   400.0000000           .           . . .           .           .
#> gene_3             . 700.0000000           . . . 800.0000000           .
#> 
#> ......suppressing 40 columns and 94 rows
#> 
#> gene_98            .           .           . . .           .           .
#> gene_99            .           . 500.0000000 . .           .           .
#> gene_100 700.0000000           .           . . .           .           .
```

### Matrix Arithmetic

`dbMatrix` also supports matrix arithmetic for `dbMatrix` objects that
are [conformable](https://en.wikipedia.org/wiki/Conformable_matrix).

``` r

dbsm + dbsm
#> 100 x 50  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]
#>                                                                               
#> gene_1            .          .          . . .          . 10.0000000          .
#> gene_2    8.0000000          .          . . .          .          .          .
#> gene_3            . 14.0000000          . . . 16.0000000          .  6.0000000
#> 
#> ......suppressing 40 columns and 94 rows
#> 
#> gene_98           .          .          . . .          .          .          .
#> gene_99           .          . 10.0000000 . .          .          .          .
#> gene_100 14.0000000          .          . . .          .          .          .
```

### Matrix Multiplication

#### Hadamard product

``` r

dbsm * dbsm
#> 100 x 50  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'cell_1', 'cell_2', 'cell_3' ... suppressing 44 ...'cell_48', 'cell_49', 'cell_50' ]]
#>                                                                               
#> gene_1            .          .          . . .          . 25.0000000          .
#> gene_2   16.0000000          .          . . .          .          .          .
#> gene_3            . 49.0000000          . . . 64.0000000          .  9.0000000
#> 
#> ......suppressing 40 columns and 94 rows
#> 
#> gene_98           .          .          . . .          .          .          .
#> gene_99           .          . 25.0000000 . .          .          .          .
#> gene_100 49.0000000          .          . . .          .          .          .
```

#### Matrix product

The matrix product (`%*%`) is not supported yet.

### Cleanup

``` r

DBI::dbDisconnect(con, shutdown = TRUE)
options(old_options)
```

### Session Info

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dbMatrix_0.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10           generics_0.1.4        lattice_0.22-9       
#>  [4] digest_0.6.39         magrittr_2.0.5        evaluate_1.0.5       
#>  [7] grid_4.6.1            nanoarrow_0.9.0       blob_1.3.0           
#> [10] fastmap_1.2.0         jsonlite_2.0.0        Matrix_1.7-5         
#> [13] pins_1.4.2            DBI_1.3.0             purrr_1.2.2          
#> [16] textshaping_1.0.5     jquerylib_0.1.4       duckdb_1.5.6         
#> [19] cli_3.6.6             rlang_1.3.0           dbplyr_2.6.0         
#> [22] rscontract_0.1.2      bit64_4.8.6           connections_0.2.1    
#> [25] withr_3.0.3           cachem_1.1.0          yaml_2.3.12          
#> [28] otel_0.2.0            tools_4.6.1           dplyr_1.2.1          
#> [31] assertthat_0.2.1      vctrs_0.7.3           R6_2.6.1             
#> [34] matrixStats_1.5.0     lifecycle_1.0.5       fs_2.1.0             
#> [37] dbProject_0.1.1       bit_4.6.0             ragg_1.5.2           
#> [40] arrow_25.0.1          pkgconfig_2.0.3       desc_1.4.3           
#> [43] pkgdown_2.2.1         bslib_0.12.0          pillar_1.11.1        
#> [46] data.table_1.18.6.1   glue_1.8.1            Rcpp_1.1.2           
#> [49] systemfonts_1.3.2     xfun_0.62             tibble_3.3.1         
#> [52] tidyselect_1.2.1      MatrixGenerics_1.24.0 knitr_1.52           
#> [55] htmltools_0.5.9       rmarkdown_2.32        compiler_4.6.1
```
