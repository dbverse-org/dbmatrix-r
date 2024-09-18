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
res_mat = mat + mat
res_dbdm = dbdm + dbdm
res_dbdm = as.matrix(res_dbdm)
test_that("+ matrix equal", {
  expect_equal(res_mat, res_dbdm)
})


# FIXME:
# Interesting edge case where the result is a zero matrix
# How to handle this?
# res_mat = mat - mat
# res_dbdm = dbdm - dbdm
# res_dbdm = as.matrix(res_dbdm)
# test_that("- matrix equal", {
#   expect_equal(res_mat, res_dbdm)
# })

res_mat = mat * mat
res_dbdm = dbdm * dbdm
res_dbdm = as.matrix(res_dbdm)
test_that("* matrix equal", {
  expect_equal(res_mat, res_dbdm)
})

# FIXME:
# Support for division by 0
# res_mat = mat / mat
# res_dbdm = dbdm / dbdm
# res_dbdm = as.matrix(res_dbdm)
# test_that("/ matrix equal", {
#   expect_equal(res_mat, res_dbdm)
# })

# FIXME: NaN and 1 logic
# res_mat = mat ^ mat
# res_dbdm = dbdm ^ dbdm
# res_dbdm = as.matrix(res_dbdm)
# test_that("^ matrix equal", {
#   expect_equal(res_mat, res_dbdm)
# })

# FIXME: division by 0, NaN and 1 logic
# res_mat = mat %% mat
# res_dbdm = dbdm %% dbdm
# res_dbdm = as.matrix(res_dbdm)
# test_that("%% matrix equal", {
#   expect_equal(res_mat, res_dbdm)
# })

# FIXME: division by 0, Nan and 1 logic
# res_mat = mat %/% mat
# res_dbdm = dbdm %/% dbdm
# res_dbdm = as.matrix(res_dbdm)
# test_that("%/% matrix equal", {
#   expect_equal(res_mat, res_dbdm)
# })
