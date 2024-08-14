# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))
mat = as.matrix(dgc + 1)

con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbdm = dbMatrix::dbMatrix(value = mat,
                          con = con1,
                          name = 'mat',
                          class = "dbDenseMatrix",
                          overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# Test scalar arithmetic

res_mat = mat + 1
res_mat = res_mat |> as.matrix() #dgeMatrix casting
res_dbdm = dbdm + 1
res_dbdm = as_matrix(res_dbdm)

test_that("+ 1 equal", {
  expect_equal(res_mat, res_dbdm)
})


res_mat = mat - 1
res_mat = res_mat |> as.matrix() #dgeMatrix casting
res_dbdm = dbdm - 1
res_dbdm = as_matrix(res_dbdm)

test_that("-1 equal", {
  expect_equal(res_mat, res_dbdm)
})


res_mat = mat * 10
res_dbdm = dbdm * 10
res_dbdm = as_matrix(res_dbdm)

test_that("* 10 equal", {
  expect_equal(res_mat, res_dbdm)
})


res_mat = mat + 0
res_dbdm = dbdm + 0
res_dbdm = as_matrix(res_dbdm)

test_that("+0 equal", {
  expect_equal(res_mat, res_dbdm)
})

res_mat = mat / 10
res_dbdm = dbdm / 10
res_dbdm = as_matrix(res_dbdm)

test_that("/10 equal", {
  expect_equal(res_mat, res_dbdm)
})

# res_mat = mat / 0
# res_dbdm = dbdm / 0
# res_dbdm = as_matrix(res_dbdm)
#
# test_that("/ 0 equal", {
#   expect_equal(res_mat, res_dbdm)
# })
