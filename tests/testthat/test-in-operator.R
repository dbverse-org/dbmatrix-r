# Helper function to set up test environment
setup_test_env <- function() {
  # Create a connection to an in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test matrices
  dense_matrix <- matrix(1:9, 3, 3)
  colnames(dense_matrix) <- paste0("col", 1:3)
  rownames(dense_matrix) <- paste0("row", 1:3)

  sparse_matrix <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 1, 3),
    j = c(1, 2, 3, 3, 1),
    x = c(1, 2, 3, 4, 5),
    dims = c(3, 3),
    dimnames = list(paste0("row", 1:3), paste0("col", 1:3))
  )

  # Convert to dbMatrix objects
  db_dense <- dbMatrix(
    value = dense_matrix,
    con = con,
    name = "dense_matrix",
    class = "dbDenseMatrix",
    overwrite = TRUE
  )

  db_sparse <- dbMatrix(
    value = sparse_matrix,
    con = con,
    name = "sparse_matrix",
    class = "dbSparseMatrix",
    overwrite = TRUE
  )

  return(list(
    con = con,
    dense_matrix = dense_matrix,
    sparse_matrix = sparse_matrix,
    db_dense = db_dense,
    db_sparse = db_sparse
  ))
}

# Tests for dbMatrix %in% ANY
test_that("dbDenseMatrix %in% vector works correctly", {
  env <- setup_test_env()

  # Test dbDenseMatrix %in% vector - returns logical vector
  result <- env$db_dense %in% c(1, 5, 9)
  expected <- env$dense_matrix %in% c(1, 5, 9)

  expect_equal(length(result), length(expected))
  expect_equal(result, expected)
})

test_that("dbSparseMatrix %in% vector throws error", {
  env <- setup_test_env()

  # Test dbSparseMatrix %in% vector - should throw error
  expect_error(env$db_sparse %in% c(1, 3, 5), "'match' requires vector arguments")
})

test_that("dbDenseMatrix %in% empty vector returns logical vector of FALSE values", {
  env <- setup_test_env()

  # Test with empty vector
  result <- env$db_dense %in% numeric(0)

  # Result should be logical vector of the same length as the matrix
  expect_equal(length(result), length(env$db_dense))
  expect_true(all(!result))
})

# Tests for ANY %in% dbMatrix
test_that("vector %in% dbDenseMatrix works correctly", {
  env <- setup_test_env()

  # Test vector %in% dbDenseMatrix
  result <- c(1, 5, 9, 10) %in% env$db_dense
  expected <- c(1, 5, 9, 10) %in% env$dense_matrix

  expect_equal(result, expected)
})

test_that("empty vector %in% dbMatrix returns empty logical vector", {
  env <- setup_test_env()

  # Test with empty vector
  result <- numeric(0) %in% env$db_dense

  # Should return empty logical vector
  expect_equal(result, logical(0))
})

# Edge cases
test_that("NA values are handled correctly", {
  env <- setup_test_env()

  # Create a matrix with NA values
  na_matrix <- matrix(c(1, 2, NA, 4, NA, 6, 7, 8, 9), 3, 3)
  db_na <- dbMatrix(
    value = na_matrix,
    con = env$con,
    name = "na_matrix",
    class = "dbDenseMatrix",
    overwrite = TRUE
  )

  # Test NA %in% dbMatrix
  result_1 <- NA %in% db_na
  expected_1 <- NA %in% na_matrix
  expect_equal(result_1, expected_1)

  # Test dbMatrix %in% NA
  result_2 <- db_na %in% NA
  expected_2 <- na_matrix %in% NA
  expect_equal(result_2, expected_2)

  # Test dbMatrix %in% c(NA, 1, 2)
  result_3 <- db_na %in% c(NA, 1, 2)
  expected_3 <- na_matrix %in% c(NA, 1, 2)
  expect_equal(result_3, expected_3)
})

# Clean up
test_that("Cleanup works", {
  env <- setup_test_env()
  expect_silent(DBI::dbDisconnect(env$con))
})
