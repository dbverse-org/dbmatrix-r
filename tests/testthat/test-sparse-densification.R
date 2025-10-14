rlang::local_options(lifecycle_verbosity = "quiet")

dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))

con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm = dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'dgc',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

test_that("sparse + non-zero scalar becomes dense", {
  res_dbsm = dbsm + 1
  expect_s4_class(res_dbsm, "dbDenseMatrix")
})

test_that("sparse - non-zero scalar becomes dense", {
  res_dbsm = dbsm - 1
  expect_s4_class(res_dbsm, "dbDenseMatrix")
})

test_that("sparse + zero stays sparse", {
  res_dbsm = dbsm + 0
  expect_s4_class(res_dbsm, "dbSparseMatrix")
})

test_that("sparse * zero stays sparse", {
  res_dbsm = dbsm * 0
  expect_s4_class(res_dbsm, "dbSparseMatrix")
})

test_that("sparse * non-zero stays sparse", {
  res_dbsm = dbsm * 2
  expect_s4_class(res_dbsm, "dbSparseMatrix")
})

test_that("sparse / non-zero stays sparse", {
  res_dbsm = dbsm / 2
  expect_s4_class(res_dbsm, "dbSparseMatrix")
})

test_that("non-zero + sparse becomes dense", {
  res_dbsm = 1 + dbsm
  expect_s4_class(res_dbsm, "dbDenseMatrix")
})

test_that("zero + sparse stays sparse", {
  res_dbsm = 0 + dbsm
  expect_s4_class(res_dbsm, "dbSparseMatrix")
})

test_that("sparse + non-zero preserves values correctly", {
  res_dgc = dgc + 1
  res_dbsm = dbsm + 1
  res_dbsm = as.matrix(res_dbsm, names = TRUE)
  res_dgc = as.matrix(res_dgc)
  expect_equal(res_dgc, res_dbsm)
})

test_that("sparse + zero preserves sparsity and values", {
  res_dgc = dgc + 0
  res_dbsm = dbsm + 0
  res_dbsm = as.matrix(res_dbsm, names = TRUE, sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})
