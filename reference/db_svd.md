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
  memory_limit = getOption("dbMatrix.svd_memory", 8 * 1024^3),
  return_format = c("svd", "pca"),
  tol = getOption("dbMatrix.svd_tol", 1e-05),
  maxit = getOption("dbMatrix.svd_maxit", 1000L),
  ncv = getOption("dbMatrix.svd_ncv", NULL),
  score_threads = getOption("dbMatrix.svd_score_threads", 1L),
  operator_threads = getOption("dbMatrix.svd_operator_threads", 1L)
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

  Bytes for Fast Path. Default 8 GB.

- return_format:

  "svd" (d, u, v) or "pca" (eigenvalues, loadings, coords)

- tol:

  Eigensolver convergence tolerance.

- maxit:

  Maximum eigensolver iterations.

- ncv:

  Number of Lanczos basis vectors. Defaults to
  `max(ceiling(1.5 * k), 40)`.

- score_threads:

  Number of threads used to calculate the cell scores.

- operator_threads:

  Number of threads used for each Gram-matrix product.

## Value

List with SVD or PCA components
