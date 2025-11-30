# Arithmetic

``` r
library(dbMatrix)
```

## dbMatrix arithmetic

`dbMatrix` objects support `Arith` and `Ops` operations. We will
demonstrate how to perform arithmetic operations on `dbSparseMatrix`
objects.

**Note:** Some operations with zero values are not yet supported with
dbMatrix objects. In addition, certain arithmetic operations between
`dbMatrix` objects are also not yet supported. We welcome user feedback
and reporting issues on the [Github
page](https://github.com/dbverse-org/dbmatrix-r/).

### Get test data

The test file is a `dgCMatrix`or compressed sparse column matrix
representing a single cell gene expression matrix. The file is in the
`data` directory of the package.

Let’s load the .rds file and preview the object.

``` r
dgc <- readRDS("../data/dgc.rds")

dplyr::glimpse(dgc)
#> Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
#>   ..@ i       : int [1:170625] 0 6 10 17 21 22 25 31 33 35 ...
#>   ..@ p       : int [1:625] 0 227 510 758 980 1293 1631 1976 2223 2434 ...
#>   ..@ Dim     : int [1:2] 634 624
#>   ..@ Dimnames:List of 2
#>   .. ..$ : chr [1:634] "Gna12" "Ccnd2" "Btbd17" "Sox9" ...
#>   .. ..$ : chr [1:624] "AAAGGGATGTAGCAAG-1" "AAATGGCATGTCTTGT-1" "AAATGGTCAATGTGCC-1" "AAATTAACGGGTAGCT-1" ...
#>   ..@ x       : num [1:170625] 1 1 1 1 1 1 6 2 1 1 ...
#>   ..@ factors : list()
```

The file contains 634 rows and 624 columns. The rows represent gene
names and the columns represent cell names. The values are integers and
represent the number of times a gene is detected in a cell. Like most
single-cell RNA-seq data, the matrix is sparse.

### Create a dbMatrix object

Let’s create a `dbSparseMatrix` object from the above `dgc` object.

``` r
# Note: by default the constructor creates a dbMatrix object in-memory
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm <- dbMatrix(value = dgc, 
                 con = con, 
                 name = 'visium', 
                 class = "dbSparseMatrix",
                 overwrite = TRUE)

# preview the object
dbsm
#> 634 x 624  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                          
#> Gna12         1.0000000 2.0000000 1.0000000 1.0000000 9.0000000 1.0000000
#> Ccnd2                 . 1.0000000 1.0000000         .         . 1.0000000
#> Btbd17                . 1.0000000 1.0000000 1.0000000         .         .
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935               . 1.0000000         .         .         .         .
#> 9630013A20Rik         .         .         .         .         .         .
#> 2900040C04Rik 1.0000000         .         .         .         .         .
```

### Scalar Arithmetic

`dbMatrix` emulates scalar arithmetic in the `Matrix` package.

Note: Addition or subtraction with non-zero addends on a
`dbSparseMatrix` results in a `dbDenseMatrix`.

``` r
dbsm + 1
#> ℹ Computing new dense COO table with 634 rows and 624 columns...
#> 634 x 624  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                                
#> Gna12          2.0000000  3.0000000  2.0000000  2.0000000 10.0000000  2.0000000
#> Ccnd2          1.0000000  2.0000000  2.0000000  1.0000000  1.0000000  2.0000000
#> Btbd17         1.0000000  2.0000000  2.0000000  2.0000000  1.0000000  1.0000000
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935        1.0000000  2.0000000  1.0000000  1.0000000  1.0000000  1.0000000
#> 9630013A20Rik  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000
#> 2900040C04Rik  2.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000

dbsm * 100
#> 634 x 624  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                          
#> Gna12         100.0000000 200.0000000 100.0000000 100.0000000 900.0000000
#> Ccnd2                   . 100.0000000 100.0000000           .           .
#> Btbd17                  . 100.0000000 100.0000000 100.0000000           .
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935                 . 100.0000000           .           .           .
#> 9630013A20Rik           .           .           .           .           .
#> 2900040C04Rik 100.0000000           .           .           .           .
```

### Matrix Arithmetic

`dbMatrix` also supports matrix arithmetic for `dbMatrix` objects that
are [conformable](https://en.wikipedia.org/wiki/Conformable_matrix).

``` r
dbsm + dbsm
#> 634 x 624  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                                
#> Gna12          2.0000000  4.0000000  2.0000000  2.0000000 18.0000000  2.0000000
#> Ccnd2                  .  2.0000000  2.0000000          .          .  2.0000000
#> Btbd17                 .  2.0000000  2.0000000  2.0000000          .          .
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935                .  2.0000000          .          .          .          .
#> 9630013A20Rik          .          .          .          .          .          .
#> 2900040C04Rik  2.0000000          .          .          .          .          .
```

### Matrix Multiplication

#### Hadamard product

``` r
dbsm * dbsm
#> 634 x 624  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                                
#> Gna12          1.0000000  4.0000000  1.0000000  1.0000000 81.0000000  1.0000000
#> Ccnd2                  .  1.0000000  1.0000000          .          .  1.0000000
#> Btbd17                 .  1.0000000  1.0000000  1.0000000          .          .
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935                .  1.0000000          .          .          .          .
#> 9630013A20Rik          .          .          .          .          .          .
#> 2900040C04Rik  1.0000000          .          .          .          .          .
```

#### Matrix product

TODO

### Session Info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
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
#> [1] dbMatrix_0.0.0.9124
#> 
#> loaded via a namespace (and not attached):
#>  [1] bit_4.6.0             Matrix_1.7-4          jsonlite_2.0.0       
#>  [4] dplyr_1.1.4           compiler_4.5.2        tidyselect_1.2.1     
#>  [7] blob_1.2.4            dbProject_0.0.0.9000  jquerylib_0.1.4      
#> [10] systemfonts_1.3.1     textshaping_1.0.4     yaml_2.3.10          
#> [13] fastmap_1.2.0         lattice_0.22-7        R6_2.6.1             
#> [16] generics_0.1.4        knitr_1.50            tibble_3.3.0         
#> [19] desc_1.4.3            MatrixGenerics_1.22.0 DBI_1.2.3            
#> [22] bslib_0.9.0           pillar_1.11.1         rlang_1.1.6          
#> [25] cachem_1.1.0          xfun_0.54             fs_1.6.6             
#> [28] sass_0.4.10           bit64_4.6.0-1         cli_3.6.5            
#> [31] pkgdown_2.2.0         withr_3.0.2           magrittr_2.0.4       
#> [34] digest_0.6.39         grid_4.5.2            dbplyr_2.5.1         
#> [37] lifecycle_1.0.4       vctrs_0.6.5           evaluate_1.0.5       
#> [40] glue_1.8.0            data.table_1.17.8     duckdb_1.4.2         
#> [43] ragg_1.5.0            purrr_1.2.0           rmarkdown_2.30       
#> [46] matrixStats_1.5.0     tools_4.5.2           pkgconfig_2.0.3      
#> [49] htmltools_0.5.8.1
```
