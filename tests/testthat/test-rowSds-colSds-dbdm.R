# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# Helper to compare values ignoring names
expect_equal_values <- function(actual, expected) {
  names(actual) <- NULL
  names(expected) <- NULL
  expect_equal(actual, expected)
}

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc <- readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix"))
mat <- as.matrix(dgc)

# Connect to the database
con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbDenseMatrix
dbdm <- dbMatrix::dbMatrix(
  value = mat,
  con = con1,
  name = 'mat',
  class = "dbDenseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# rowSds

test_that("rowSds equal for dbDenseMatrix (memory=TRUE)", {
  res_mat <- MatrixGenerics::rowSds(mat)
  res_dbdm <- rowSds(dbdm, memory = TRUE)
  expect_equal_values(res_mat, res_dbdm)
})

test_that("rowSds equal for dbDenseMatrix (memory=FALSE)", {
  res_mat <- MatrixGenerics::rowSds(mat)
  res_dbdm <- rowSds(dbdm, memory = FALSE)
  res_dbdm_vec <- suppressWarnings(res_dbdm |> as.vector())
  expect_equal_values(res_mat, res_dbdm_vec)
})

# ---------------------------------------------------------------------------- #
# colSds

test_that("colSds equal for dbDenseMatrix (memory=TRUE)", {
  res_mat <- MatrixGenerics::colSds(mat)
  res_dbdm <- colSds(dbdm, memory = TRUE)
  expect_equal_values(res_mat, res_dbdm)
})

test_that("colSds equal for dbDenseMatrix (memory=FALSE)", {
  res_mat <- MatrixGenerics::colSds(mat)
  res_dbdm <- colSds(dbdm, memory = FALSE)
  res_dbdm_vec <- suppressWarnings(res_dbdm |> as.vector())
  expect_equal_values(res_mat, res_dbdm_vec)
})

# Close the database connection
DBI::dbDisconnect(con1)
