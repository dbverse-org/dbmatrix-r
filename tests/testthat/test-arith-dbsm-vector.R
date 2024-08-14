# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))

con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm = dbMatrix::dbMatrix(value = dgc,
                          con = con1,
                          name = 'dgc',
                          class = "dbSparseMatrix",
                          overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# Test scalar arithmetic

res_dgc = dgc + c(1,2,3)
res_dgc = res_dgc |> as.matrix() #dgeMatrix casting
res_dbsm = dbsm + c(1,2,3)
res_dbsm = as_matrix(res_dbsm)

test_that("+ 1 equal", {
  expect_equal(res_dgc, res_dbsm)
})


res_dgc = dgc - c(1,2,3)
res_dgc = as.matrix(res_dgc) #dgeMatrix casting
res_dbsm = dbsm - c(1,2,3)
res_dbsm = as_matrix(res_dbsm)

test_that("-1 equal", {
  expect_equal(res_dgc, res_dbsm)
})


res_dgc = dgc * c(1,2,3)
res_dbsm = dbsm * c(1,2,3)
res_dbsm = as_matrix(res_dbsm)

test_that("* 10 equal", {
  expect_equal(res_dgc, res_dbsm)
})


res_dgc = dgc + c(1,2,3)
res_dgc = as.matrix(res_dgc) #dgeMatrix casting
res_dbsm = dbsm + c(1,2,3)
res_dbsm = as_matrix(res_dbsm)

test_that("+0 equal", {
  expect_equal(res_dgc, res_dbsm)
})

res_dgc = dgc / c(1,2,3)
res_dbsm = dbsm / c(1,2,3)
res_dbsm = as_matrix(res_dbsm)

test_that("/10 equal", {
  expect_equal(res_dgc, res_dbsm)
})

#FIXME: support for division by zero
# res_dgc = dgc / 0
# res_dbsm = dbsm / 0
# res_dbsm = as_matrix(res_dbsm)
#
# test_that("/ 0 equal", {
#   expect_equal(res_dgc, res_dbsm)
# })
