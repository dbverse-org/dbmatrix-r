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
res_dbsm = as_matrix(res_dbsm)

test_that("+ matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})


# FIXME:
# Interesting edge case where the result is a zero matrix
# How to handle this?
# res_dgc = dgc - dgc
# res_dbsm = dbsm - dbsm
# res_dbsm = as_matrix(res_dbsm)
# test_that("- matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

res_dgc = dgc * dgc
res_dbsm = dbsm * dbsm
res_dbsm = as_matrix(res_dbsm)
test_that("* matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

# FIXME:
# Support for division by 0
# res_dgc = dgc / dgc
# res_dbsm = dbsm / dbsm
# res_dbsm = as_matrix(res_dbsm)
# test_that("/ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: NaN and 1 logic
# res_dgc = dgc ^ dgc
# res_dbsm = dbsm ^ dbsm
# res_dbsm = as_matrix(res_dbsm)
# test_that("^ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, NaN and 1 logic
# res_dgc = dgc %% dgc
# res_dbsm = dbsm %% dbsm
# res_dbsm = as_matrix(res_dbsm)
# test_that("%% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, Nan and 1 logic
# res_dgc = dgc %/% dgc
# res_dbsm = dbsm %/% dbsm
# res_dbsm = as_matrix(res_dbsm)
# test_that("%/% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })
