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

# Connect to the database
con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbSparseMatrix
dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'dgc',
  class = "dbSparseMatrix",
  overwrite = TRUE
)
# ---------------------------------------------------------------------------- #
# rowSums

test_that("rowSums equal (memory=TRUE)", {
  res_dgc <- rowSums(dgc)
  res_dbsm <- rowSums(dbsm, memory = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("rowSums equal (memory=FALSE)", {
  res_dgc <- rowSums(dgc)
  res_dbsm <- rowSums(dbsm, memory = FALSE) |> as.vector()
  expect_equal_values(res_dgc, res_dbsm)
})

# ---------------------------------------------------------------------------- #
# colSums

test_that("colSums equal (memory=TRUE)", {
  res_dgc <- colSums(dgc)
  res_dbsm <- colSums(dbsm, memory = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("colSums equal (memory=FALSE)", {
  res_dgc <- colSums(dgc)
  res_dbsm <- colSums(dbsm, memory = FALSE) |> as.vector()
  expect_equal_values(res_dgc, res_dbsm)
})

# ---------------------------------------------------------------------------- #
# rowMeans

test_that("rowMeans equal (memory=TRUE)", {
  res_dgc <- rowMeans(dgc)
  res_dbsm <- rowMeans(dbsm, memory = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("rowMeans equal (memory=FALSE)", {
  res_dgc <- rowMeans(dgc)
  res_dbsm <- rowMeans(dbsm, memory = FALSE) |> as.vector()
  expect_equal_values(res_dgc, res_dbsm)
})

# ---------------------------------------------------------------------------- #
# colMeans

test_that("colMeans equal (memory=TRUE)", {
  res_dgc <- colMeans(dgc)
  res_dbsm <- colMeans(dbsm, memory = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("colMeans equal (memory=FALSE)", {
  res_dgc <- colMeans(dgc)
  res_dbsm <- colMeans(dbsm, memory = FALSE) |> as.vector()
  expect_equal_values(res_dgc, res_dbsm)
})

# Close the database connection
DBI::dbDisconnect(con1)
