# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# Skip all tests if sparseMatrixStats is not available
# (required for MatrixGenerics::rowVars/colVars on dgCMatrix)
skip_if_not_installed("sparseMatrixStats")

# Helper to compare values ignoring names
expect_equal_values <- function(actual, expected) {
  names(actual) <- NULL
  names(expected) <- NULL
  expect_equal(actual, expected)
}

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc <- readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix"))

# Connect to the database
con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbSparseMatrix
dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'mat',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# rowVars

test_that("rowVars equal for dbSparseMatrix (memory=TRUE)", {
  res_mat <- MatrixGenerics::rowVars(dgc)
  res_dbsm <- rowVars(dbsm, memory = TRUE)
  expect_equal_values(res_mat, res_dbsm)
})

test_that("rowVars equal for dbSparseMatrix (memory=FALSE)", {
  res_mat <- MatrixGenerics::rowVars(dgc)
  res_dbsm <- rowVars(dbsm, memory = FALSE)
  res_dbsm_vec <- suppressWarnings(res_dbsm |> as.vector())
  expect_equal_values(res_mat, res_dbsm_vec)
})

# ---------------------------------------------------------------------------- #
# colVars

test_that("colVars equal for dbSparseMatrix (memory=TRUE)", {
  res_mat <- MatrixGenerics::colVars(dgc)
  res_dbsm <- colVars(dbsm, memory = TRUE)
  expect_equal_values(res_mat, res_dbsm)
})

test_that("colVars equal for dbSparseMatrix (memory=FALSE)", {
  res_mat <- MatrixGenerics::colVars(dgc)
  res_dbsm <- colVars(dbsm, memory = FALSE)
  res_dbsm_vec <- suppressWarnings(res_dbsm |> as.vector())
  expect_equal_values(res_mat, res_dbsm_vec)
})

# Close the database connection
DBI::dbDisconnect(con1)
