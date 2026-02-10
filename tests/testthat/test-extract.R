# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc <- readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix"))

con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'dgc',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# Perform integer indexing

dgc_subset <- dgc[1:10, ]
dbsm_subset <- dbsm[1:10, ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("integer row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[, 1:10]
dbsm_subset <- dbsm[, 1:10]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("integer col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[1:10, 1:10]
dbsm_subset <- dbsm[1:10, 1:10]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("integer row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform character indexing

row_char_index <- rownames(dgc)[1:10]
dgc_subset <- dgc[row_char_index, ]
dbsm_subset <- dbsm[row_char_index, ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("character row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

col_char_index <- colnames(dgc)[1:10]
dgc_subset <- dgc[, col_char_index]
dbsm_subset <- dbsm[, col_char_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("character col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[row_char_index, col_char_index]
dbsm_subset <- dbsm[row_char_index, col_char_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)
test_that("character row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform boolean indexing

boolean_row_index <- c(rep(FALSE, nrow(dgc) - 5), rep(TRUE, 5))
dgc_subset <- dgc[boolean_row_index, ]
dbsm_subset <- dbsm[boolean_row_index, ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("boolean row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

boolean_col_index <- c(rep(FALSE, ncol(dgc) - 5), rep(TRUE, 5))
dgc_subset <- dgc[, boolean_col_index]
dbsm_subset <- dbsm[, boolean_col_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("boolean col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[boolean_row_index, boolean_col_index]
dbsm_subset <- dbsm[boolean_row_index, boolean_col_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("boolean row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform negative indexing

dgc_subset <- dgc[-c(1:5), ]
dbsm_subset <- dbsm[-c(1:5), ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("negative row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[, -c(1:5)]
dbsm_subset <- dbsm[, -c(1:5)]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("negative col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[-c(1:5), -c(1:5)]
dbsm_subset <- dbsm[-c(1:5), -c(1:5)]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("negative row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform out-of-order indexing

ooo_row_index <- c(10, 5, 1, 8, 3)
ooo_col_index <- c(8, 2, 6, 1)

dgc_subset <- dgc[ooo_row_index, ]
dbsm_subset <- dbsm[ooo_row_index, ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("out-of-order row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[, ooo_col_index]
dbsm_subset <- dbsm[, ooo_col_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("out-of-order col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[ooo_row_index, ooo_col_index]
dbsm_subset <- dbsm[ooo_row_index, ooo_col_index]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("out-of-order row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

# ---------------------------------------------------------------------------- #
# Perform empty indexing (regression: avoid invalid SQL like `VALUES )`)

dgc_subset <- dgc[integer(0), ]
dbsm_subset <- dbsm[integer(0), ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty integer row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[, integer(0)]
dbsm_subset <- dbsm[, integer(0)]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty integer col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[integer(0), integer(0)]
dbsm_subset <- dbsm[integer(0), integer(0)]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty integer row/col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[integer(0), 1:10]
dbsm_subset <- dbsm[integer(0), 1:10]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty rows with non-empty cols works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

dgc_subset <- dgc[1:10, integer(0)]
dbsm_subset <- dbsm[1:10, integer(0)]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("non-empty rows with empty cols works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

false_rows <- rep(FALSE, nrow(dgc))
dgc_subset <- dgc[false_rows, ]
dbsm_subset <- dbsm[false_rows, ]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty logical row indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})

false_cols <- rep(FALSE, ncol(dgc))
dgc_subset <- dgc[, false_cols]
dbsm_subset <- dbsm[, false_cols]
dgc_db_subset <- as.matrix(dbsm_subset, sparse = TRUE, names = TRUE)

test_that("empty logical col indexing works", {
  expect_equal(dgc_subset, dgc_db_subset)
})
