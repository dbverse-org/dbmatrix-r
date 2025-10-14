# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = sim_dgc(3, 5, 10)

dbsm = as.dbMatrix(dgc)
# ---------------------------------------------------------------------------- #
# Test scalar arithmetic
test_that("+ 1 equal", {
  res_dgc = dgc + c(1, 2, 3)
  res_dgc = res_dgc |> as.matrix() #dgeMatrix casting
  res_dbsm = dbsm + c(1, 2, 3)
  res_dbsm = as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("-1 equal", {
  res_dgc = dgc - c(1, 2, 3)
  res_dgc = as.matrix(res_dgc) #dgeMatrix casting
  res_dbsm = dbsm - c(1, 2, 3)
  res_dbsm = as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("* 10 equal", {
  res_dgc = dgc * c(1, 2, 3)
  res_dbsm = dbsm * c(1, 2, 3)
  res_dbsm = as.matrix(res_dbsm, sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that("+ vector equal", {
  res_dgc = dgc + c(1, 2, 3)
  res_dgc = as.matrix(res_dgc) #dgeMatrix casting
  res_dbsm = dbsm + c(1, 2, 3)
  res_dbsm = as.matrix(res_dbsm)
  expect_equal(res_dgc, res_dbsm)
})

test_that("/ vector equal", {
  res_dgc = dgc / c(1, 2, 3)
  res_dbsm = dbsm / c(1, 2, 3)
  res_dbsm = as.matrix(res_dbsm, sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})
