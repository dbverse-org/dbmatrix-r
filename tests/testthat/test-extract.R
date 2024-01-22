# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))

dbsm = dbMatrix::createDBMatrix(value = dgc,
                                db_path = ":temp:",
                                name = 'dgc',
                                class = "dbSparseMatrix",
                                overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# Perform integer indexing

dgc_subset = dgc[1:10,]
dbsm_subset = dbsm[1:10,]
dgc_db_subset = as_matrix(dbsm_subset)

test_that("integer row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset = dgc[,1:10]
dbsm_subset = dbsm[,1:10]
dgc_db_subset = as_matrix(dbsm_subset)

test_that("integer col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset = dgc[1:10,1:10]
dbsm_subset = dbsm[1:10,1:10]
dgc_db_subset = as_matrix(dbsm_subset)

test_that("integer row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform character indexing

row_char_index = rownames(dgc)[1:10]
dgc_subset = dgc[row_char_index,]
dbsm_subset = dbsm[row_char_index,]
dgc_db_subset = as_matrix(dbsm_subset)

test_that("character row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

col_char_index = colnames(dgc)[1:10]
dgc_subset = dgc[,col_char_index]
dbsm_subset = dbsm[,col_char_index]
dgc_db_subset = as_matrix(dbsm_subset)

test_that("character col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset = dgc[row_char_index,col_char_index]
dbsm_subset = dbsm[row_char_index,col_char_index]
dgc_db_subset = as_matrix(dbsm_subset)
test_that("character row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #

