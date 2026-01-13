# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# Enable densification for tests
options(dbMatrix.allow_densify = TRUE)

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
dbsm <- dbMatrix::dbMatrix(
  value = readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix")),
  con = con1,
  name = 'dgc',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

dgc <- as(dbsm, "dgCMatrix")

# ---------------------------------------------------------------------------- #
# Test scalar arithmetic

test_that("+ 1 equal", {
  res_dgc <- dgc + 1
  res_dgc <- res_dgc |> as.matrix() #dgeMatrix casting
  res_dbsm <- dbsm + 1
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("-1 equal", {
  res_dgc <- dgc - 1
  res_dgc <- res_dgc |> as.matrix() #dgeMatrix casting
  res_dbsm <- dbsm - 1
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("* 10 equal", {
  res_dgc <- dgc * 10
  res_dbsm <- dbsm * 10
  res_dbsm <- as.matrix(res_dbsm, sparse = TRUE)
  expect_equal(as.matrix(res_dgc), as.matrix(res_dbsm))
})

test_that("+0 equal", {
  res_dgc <- dgc + 0
  res_dbsm <- dbsm + 0
  res_dbsm <- as.matrix(res_dbsm, sparse = TRUE)
  expect_equal(as.matrix(res_dgc), as.matrix(res_dbsm))
})

test_that("/10 equal", {
  res_dgc <- dgc / 10
  res_dbsm <- dbsm / 10
  res_dbsm <- as.matrix(res_dbsm, sparse = TRUE)
  expect_equal(as.matrix(res_dgc), as.matrix(res_dbsm))
})

test_that("/ 0 equal", {
  res_dgc <- dgc / 0
  res_dgc <- as.matrix(res_dgc)
  res_dbsm <- dbsm / 0
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("^ 0 equal", {
  res_dgc <- dgc^0
  res_dgc <- as.matrix(res_dgc)
  res_dbsm <- dbsm^0
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("0 / equal", {
  res_dgc <- 0 / dgc
  res_dgc <- as.matrix(res_dgc)
  res_dbsm <- 0 / dbsm
  res_dbsm <- as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

# Cleanup
DBI::dbDisconnect(con1, shutdown = TRUE)