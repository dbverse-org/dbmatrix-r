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

res_dgc = dgc + dgc
res_dbsm = dbsm + dbsm
res_dbsm = as.matrix(res_dbsm, sparse = TRUE, names = TRUE)

test_that("+ matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

res_dgc = dgc - dgc
res_dbsm = dbsm - dbsm
res_dbsm = as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("- matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

res_dgc = dgc * dgc
res_dbsm = dbsm * dbsm
res_dbsm = as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("* matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

dgc2 = dgc * 2
dbsm2 = dbMatrix::dbMatrix(value = dgc2,
                           con = con1,
                           name = 'dgc2',
                           class = "dbSparseMatrix",
                           overwrite = TRUE)

res_dgc = dgc - dgc2
res_dbsm = dbsm - dbsm2
res_dbsm = as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("- different matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

# FIXME:
# Support for division by 0
# res_dgc = dgc / dgc
# res_dbsm = dbsm / dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("/ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: NaN and 1 logic
# res_dgc = dgc ^ dgc
# res_dbsm = dbsm ^ dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("^ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, NaN and 1 logic
# res_dgc = dgc %% dgc
# res_dbsm = dbsm %% dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("%% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, Nan and 1 logic
# res_dgc = dgc %/% dgc
# res_dbsm = dbsm %/% dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("%/% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })
