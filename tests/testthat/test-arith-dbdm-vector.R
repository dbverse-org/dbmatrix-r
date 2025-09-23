# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = sim_dgc(3,5,10)
mat = as.matrix(dgc + 1)

dbdm = as.dbMatrix(mat)

# ---------------------------------------------------------------------------- #
# Test dbMatrix-vector arithmetic
test_that("+ 1 equal", {
  res_mat = mat + c(1,2,3)
  res_dbdm = dbdm + c(1,2,3)
  res_dbdm = as.matrix(res_dbdm)
  expect_equal(res_mat, res_dbdm)
})

test_that("-1 equal", {
  res_mat = mat - c(1,2,3)
  res_dbdm = dbdm - c(1,2,3)
  res_dbdm = as.matrix(res_dbdm)
  expect_equal(res_mat, res_dbdm)
})

test_that("* 10 equal", {
  res_mat = mat * c(1,2,3)
  res_dbdm = dbdm * c(1,2,3)
  res_dbdm = as.matrix(res_dbdm)
  expect_equal(res_mat, res_dbdm)
})

test_that("+0 equal", {
  res_mat = mat + c(1,2,3)
  res_dbdm = dbdm + c(1,2,3)
  res_dbdm = as.matrix(res_dbdm)
  expect_equal(res_mat, res_dbdm)
})

test_that("/10 equal", {
  res_mat = mat / c(1,2,3)
  res_dbdm = dbdm / c(1,2,3)
  res_dbdm = as.matrix(res_dbdm)
  expect_equal(res_mat, res_dbdm)
})
