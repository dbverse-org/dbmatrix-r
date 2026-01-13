test_that("as.matrix works for dbDenseMatrix", {
  # Setup
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  dimnames(mat) <- list(paste0("r", 1:4), paste0("c", 1:3))
  dbm <- as.dbMatrix(mat)

  # Test default (dense matrix)
  res <- as.matrix(dbm)
  expect_true(is.matrix(res))
  expect_equal(res, mat, ignore_attr = TRUE)

  # Test with names = FALSE
  res_no_names <- as.matrix(dbm, names = FALSE)
  expect_true(is.matrix(res_no_names))
  expect_null(dimnames(res_no_names))
  expect_equal(res_no_names, mat, ignore_attr = TRUE)
})

test_that("as.matrix works for dbSparseMatrix", {
  # Setup
  dgc <- Matrix::rsparsematrix(5, 5, 0.5)
  dimnames(dgc) <- list(paste0("r", 1:5), paste0("c", 1:5))
  dbsm <- as.dbMatrix(dgc)

  # Test default (dense matrix) - defaults to names=TRUE
  res <- as.matrix(dbsm)
  expect_true(is.matrix(res))
  expect_equal(res, as.matrix(dgc), ignore_attr = TRUE)

  # Test sparse = TRUE (dgCMatrix) - defaults to names=TRUE
  res_sparse <- as.matrix(dbsm, sparse = TRUE)
  expect_s4_class(res_sparse, "dgCMatrix")
  expect_equal(res_sparse, dgc, ignore_attr = TRUE)

  # Test with names = FALSE
  res_no_names <- as.matrix(dbsm, names = FALSE)
  expect_null(dimnames(res_no_names))
})

test_that("as.vector works for dbDenseMatrix", {
  # Setup 1D matrix (column vector)
  mat_col <- matrix(1:5, nrow = 5, ncol = 1)
  rownames(mat_col) <- paste0("r", 1:5)
  dbm_col <- as.dbMatrix(mat_col)

  res_col <- as.vector(dbm_col)
  expect_true(is.vector(res_col))
  expect_equal(res_col, as.vector(mat_col), ignore_attr = TRUE)
  expect_equal(names(res_col), rownames(mat_col))

  # Setup 1D matrix (row vector)
  mat_row <- matrix(1:5, nrow = 1, ncol = 5)
  colnames(mat_row) <- paste0("c", 1:5)
  dbm_row <- as.dbMatrix(mat_row)

  res_row <- as.vector(dbm_row)
  expect_true(is.vector(res_row))
  expect_equal(res_row, as.vector(mat_row), ignore_attr = TRUE)
  expect_equal(names(res_row), colnames(mat_row))

  # Fail for 2D matrix
  mat_2d <- matrix(1:4, nrow = 2)
  dbm_2d <- as.dbMatrix(mat_2d)
  expect_error(as.vector(dbm_2d), "Use `as.matrix\\(\\)`")
})

test_that("setAs coercion works", {
  mat <- matrix(1:6, nrow = 2)
  dbm <- as.dbMatrix(mat)

  # as(x, "matrix")
  res_mat <- as(dbm, "matrix")
  expect_true(is.matrix(res_mat))
  expect_equal(res_mat, mat, ignore_attr = TRUE)

  # as(x, "dgCMatrix")
  res_dgc <- as(dbm, "dgCMatrix")
  expect_s4_class(res_dgc, "dgCMatrix")
  expect_equal(as.matrix(res_dgc), mat, ignore_attr = TRUE)

})

test_that("setAs coercion works for simulated tbl-backed dbSparseMatrix", {
  dbsm <- sim_dbSparseMatrix(num_rows = 10, num_cols = 10, memory = TRUE)
  res_dgc <- as(dbsm, "dgCMatrix")
  expect_s4_class(res_dgc, "dgCMatrix")
})
