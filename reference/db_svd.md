# Perform Streaming SVD on a dbMatrix

Perform Streaming SVD on a dbMatrix

## Usage

``` r
db_svd(
  dbm,
  k = 10,
  center = TRUE,
  scale = FALSE,
  center_rows = NULL,
  memory_limit = getOption("dbMatrix.svd_memory", 500 * 1024^2),
  return_format = c("svd", "pca")
)
```

## Arguments

- dbm:

  A dbSparseMatrix object

- k:

  Number of singular values to compute

- center:

  Logical, center rows (default TRUE)

- scale:

  Logical, scale rows (default FALSE)

- center_rows:

  Logical, center rows vs columns (default TRUE for standard PCA)

- memory_limit:

  Bytes for Fast Path. Set 0 to force BPCells path. Default 500 MB.

- return_format:

  "svd" (d, u, v) or "pca" (eigenvalues, loadings, coords)

## Value

List with SVD or PCA components
