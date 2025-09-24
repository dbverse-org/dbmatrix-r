# Test colRanks and rowRanks for dbMatrix objects

# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

skip_if_not_installed("dbMatrix")
skip_if_not_installed("Matrix")
skip_if_not_installed("MatrixGenerics")

library(testthat)
library(dbMatrix)
library(Matrix)
library(MatrixGenerics)
library(dplyr)
library(duckdb)
library(DBI)

# ---------------------------------------------------------------------------- #
# Setup data & Objects

# Create temp file and connection, ensure cleanup
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
# Ensure disconnection on exit/error
on.exit({
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
}, add = TRUE)

# Create test matrices
mat <- dbMatrix:::sim_denseMat()
dgc <- dbMatrix:::sim_dgc()

# Create dbMatrix objects
dbm_dense <- dbMatrix::dbMatrix(
  value = mat,
  con = con,
  name = "dense_mat",
  class = "dbDenseMatrix",
  overwrite = TRUE
)

dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con,
  name = "sparse_mat",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# Define ties methods to test
ties_methods <- c("max", "average", "min", "dense")

# ---------------------------------------------------------------------------- #
# Tests for colRanks

for (ties in ties_methods) {
  test_that(paste("colRanks works for dbDenseMatrix, ties =", ties, "memory=TRUE"), {
    expected <- colRanks(mat, ties.method = ties)
    result <- colRanks(dbm_dense, ties.method = ties, memory = TRUE)
    expect_equal(result, expected)
  })

  test_that(paste("colRanks works for dbDenseMatrix, ties =", ties, "memory=FALSE"), {
    expected <- MatrixGenerics::colRanks(mat, ties.method = ties)
    result_dbm <- dbMatrix::colRanks(dbm_dense, ties.method = ties, memory = FALSE)
    # Convert dbMatrix result back to R matrix for comparison
    result <- as.matrix(result_dbm, names = TRUE)
    expect_equal(result, expected)
    # Check class of returned object
    expect_s4_class(result_dbm, "dbDenseMatrix")
  })

  test_that(paste("colRanks works for dbSparseMatrix, ties =", ties, "memory=TRUE"), {
    # dbMatrix implementation densifies, so compare against dense equivalent
    expected <- MatrixGenerics::colRanks(dgc_dense_equiv, ties.method = ties)
    result <- dbMatrix::colRanks(dbsm, ties.method = ties, memory = TRUE)
    expect_equal(result, expected)
  })

  test_that(paste("colRanks works for dbSparseMatrix, ties =", ties, "memory=FALSE"), {
    # dbMatrix implementation densifies, so compare against dense equivalent
    expected <- MatrixGenerics::colRanks(dgc_dense_equiv, ties.method = ties)
    result_dbm <- dbMatrix::colRanks(dbsm, ties.method = ties, memory = FALSE)
    # Convert dbMatrix result back to R matrix for comparison
    result <- as.matrix(result_dbm, names = TRUE)
    expect_equal(result, expected)
    # Check class of returned object (should be dense)
    expect_s4_class(result_dbm, "dbDenseMatrix")
  })
}

# ---------------------------------------------------------------------------- #
# Tests for rowRanks

for (ties in ties_methods) {
  test_that(paste("rowRanks works for dbDenseMatrix, ties =", ties, "memory=TRUE"), {
    expected <- MatrixGenerics::rowRanks(mat, ties.method = ties)
    result <- dbMatrix::rowRanks(dbm_dense, ties.method = ties, memory = TRUE)
    expect_equal(result, expected)
  })

  test_that(paste("rowRanks works for dbDenseMatrix, ties =", ties, "memory=FALSE"), {
    expected <- MatrixGenerics::rowRanks(mat, ties.method = ties)
    result_dbm <- dbMatrix::rowRanks(dbm_dense, ties.method = ties, memory = FALSE)
    # Convert dbMatrix result back to R matrix for comparison
    result <- as.matrix(result_dbm, names = TRUE)
    expect_equal(result, expected)
    # Check class of returned object
    expect_s4_class(result_dbm, "dbDenseMatrix")
  })

  test_that(paste("rowRanks works for dbSparseMatrix, ties =", ties, "memory=TRUE"), {
    # dbMatrix implementation densifies, so compare against dense equivalent
    expected <- MatrixGenerics::rowRanks(dgc_dense_equiv, ties.method = ties)
    result <- dbMatrix::rowRanks(dbsm, ties.method = ties, memory = TRUE)
    expect_equal(result, expected)
  })

  test_that(paste("rowRanks works for dbSparseMatrix, ties =", ties, "memory=FALSE"), {
    # dbMatrix implementation densifies, so compare against dense equivalent
    expected <- MatrixGenerics::rowRanks(dgc_dense_equiv, ties.method = ties)
    result_dbm <- dbMatrix::rowRanks(dbsm, ties.method = ties, memory = FALSE)
    # Convert dbMatrix result back to R matrix for comparison
    result <- as.matrix(result_dbm, names = TRUE)
    expect_equal(result, expected)
    # Check class of returned object (should be dense)
    expect_s4_class(result_dbm, "dbDenseMatrix")
  })
}

# ---------------------------------------------------------------------------- #
# Optional: Add tests for edge cases if needed (e.g., single row/col)

# mat_single_row_data <- matrix(c(1, 3, 2), nrow = 1, dimnames = list("R1", c("C1", "C2", "C3")))
# mat_single_row <- Matrix(mat_single_row_data)
# dbm_single_row <- dbMatrix::dbMatrix(mat_single_row, con, "single_row", class = "dbDenseMatrix", overwrite = TRUE)
#
# test_that("colRanks works for single row matrix", {
#   expected <- MatrixGenerics::colRanks(mat_single_row, ties.method = "average")
#   result <- dbMatrix::colRanks(dbm_single_row, ties.method = "average", memory = TRUE)
#   expect_equal(result, expected)
# })
#
# test_that("rowRanks works for single row matrix", {
#   expected <- MatrixGenerics::rowRanks(mat_single_row, ties.method = "average")
#   result <- dbMatrix::rowRanks(dbm_single_row, ties.method = "average", memory = TRUE)
#   expect_equal(result, expected)
# })
