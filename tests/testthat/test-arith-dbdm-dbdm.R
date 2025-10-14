# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))
mat = as.matrix(dgc + 1)

con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbdm = dbMatrix::dbMatrix(
  value = mat,
  con = con1,
  name = 'mat',
  class = "dbDenseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# Test scalar arithmetic
res_mat = mat + mat
res_dbdm = dbdm + dbdm
res_dbdm = as.matrix(res_dbdm, names = TRUE)
test_that("+ matrix equal", {
  expect_equal(res_mat, res_dbdm)
})

res_mat = mat - mat
res_dbdm = dbdm - dbdm
res_dbdm = as.matrix(res_dbdm, names = TRUE)
test_that("- matrix equal", {
  expect_equal(res_mat, res_dbdm)
})

res_mat = mat * mat
res_dbdm = dbdm * dbdm
res_dbdm = as.matrix(res_dbdm, names = TRUE)
test_that("* matrix equal", {
  expect_equal(res_mat, res_dbdm)
})

mat2 = mat * 2
dbdm2 = dbMatrix::dbMatrix(
  value = mat2,
  con = con1,
  name = 'mat2',
  class = "dbDenseMatrix",
  overwrite = TRUE
)

res_mat = mat - mat2
res_dbdm = dbdm - dbdm2
res_dbdm = as.matrix(res_dbdm, names = TRUE)
test_that("- different matrix equal", {
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
