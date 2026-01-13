# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
# Enable densification for tests
options(dbMatrix.allow_densify = TRUE)

# Helper to compare results ignoring dimnames differences
expect_equal_values <- function(actual, expected) {
  if (inherits(actual, "Matrix") || inherits(actual, "sparseMatrix") || 
      inherits(actual, "dgCMatrix")) {
    actual <- as.matrix(actual)
  }
  if (inherits(expected, "Matrix") || inherits(expected, "sparseMatrix") ||
      inherits(expected, "dgCMatrix")) {
    expected <- as.matrix(expected)
  }
  dimnames(actual) <- NULL
  dimnames(expected) <- NULL
  expect_equal(actual, expected)
}

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc <- dbMatrix:::sim_dgc(3, 5, 10)

dbsm <- as.dbMatrix(dgc)
# ---------------------------------------------------------------------------- #
# Test scalar arithmetic
test_that("+ 1 equal", {
  res_dgc <- dgc + c(1, 2, 3)
  res_dgc <- res_dgc |> as.matrix() #dgeMatrix casting
  res_dbsm <- dbsm + c(1, 2, 3)
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("-1 equal", {
  res_dgc <- dgc - c(1, 2, 3)
  res_dgc <- as.matrix(res_dgc) #dgeMatrix casting
  res_dbsm <- dbsm - c(1, 2, 3)
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("* 10 equal", {
  res_dgc <- dgc * c(1, 2, 3)
  res_dbsm <- dbsm * c(1, 2, 3)
  res_dbsm <- as.matrix(res_dbsm, sparse = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("+ vector equal", {
  res_dgc <- dgc + c(1, 2, 3)
  res_dgc <- as.matrix(res_dgc) #dgeMatrix casting
  res_dbsm <- dbsm + c(1, 2, 3)
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal_values(res_dgc, res_dbsm)
})

test_that("/ vector equal", {
  res_dgc <- dgc / c(1, 2, 3)
  res_dbsm <- dbsm / c(1, 2, 3)
  res_dbsm <- as.matrix(res_dbsm, sparse = TRUE)
  expect_equal_values(res_dgc, res_dbsm)
})
