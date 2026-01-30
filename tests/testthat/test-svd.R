test_that("db_svd handles integer-typed x (Fast Path)", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("nanoarrow")

  ijx <- sim_ijx_matrix(mat_type = "sparse", num_rows = 30, num_cols = 40)
  ijx <- as.data.frame(ijx)
  ijx$x <- as.integer(round(abs(ijx$x) * 10))

  tbl <- sim_duckdb(value = ijx, name = "int64_sparse", memory = TRUE)
  con <- dbplyr::remote_con(tbl)

  dbm <- dbMatrix(
    value = tbl,
    con = con,
    name = "int64_sparse",
    dims = c(30L, 40L),
    dim_names = list(
      as.factor(paste0("row", seq_len(30))),
      as.factor(paste0("col", seq_len(40)))
    ),
    overwrite = TRUE,
    class = "dbSparseMatrix",
    init = TRUE
  )

  res <- db_svd(
    dbm = dbm,
    k = 5,
    center = FALSE,
    scale = FALSE,
    memory_limit = 10^12
  )

  testthat::expect_type(res$d, "double")
  testthat::expect_length(res$d, 5)
  testthat::expect_equal(dim(res$u), c(30L, 5L))
  testthat::expect_equal(dim(res$v), c(40L, 5L))
})

test_that("db_svd supports row-wise scaling+centering (GiottoDB PCA case)", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("nanoarrow")
  # This test uses the Fast Path to keep runtime and external dependencies low.

  dbm <- sim_dbSparseMatrix(num_rows = 30, num_cols = 40, memory = TRUE)

  res <- db_svd(
    dbm = dbm,
    k = 5,
    center = TRUE,
    scale = TRUE,
    center_rows = TRUE,
    memory_limit = 10^12
  )

  testthat::expect_type(res$d, "double")
  testthat::expect_length(res$d, 5)
  testthat::expect_equal(dim(res$u), c(30L, 5L))
  testthat::expect_equal(dim(res$v), c(40L, 5L))
})

test_that("db_svd errors on k < 1", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("nanoarrow")

  dbm <- sim_dbSparseMatrix(num_rows = 10, num_cols = 10, memory = TRUE)
  testthat::expect_error(db_svd(dbm, k = 0), "k must be at least 1")
})

test_that("db_svd warns and reduces k when exceeding max allowed", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("nanoarrow")

  dbm <- sim_dbSparseMatrix(num_rows = 10, num_cols = 20, memory = TRUE)
  testthat::expect_warning(
    res <- db_svd(dbm, k = 15, center = FALSE, scale = FALSE, memory_limit = 10^12),
    "k=15 exceeds max allowed"
  )
  # max_k = min(10-1, 20) = 9
  testthat::expect_length(res$d, 9)
})

test_that("db_svd errors on center_rows=FALSE with centering", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("nanoarrow")

  dbm <- sim_dbSparseMatrix(num_rows = 10, num_cols = 10, memory = TRUE)
  testthat::expect_error(
    db_svd(dbm, k = 5, center = TRUE, center_rows = FALSE),
    "center_rows=FALSE.*not yet supported"
  )
})
