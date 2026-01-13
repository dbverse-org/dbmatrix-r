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
# Test name equivalence

names_dgc <- names(dgc)
names_dbsm <- names(dbsm)

test_that("names() works", {
  expect_equal(names_dgc, names_dbsm)
})


names_dgc <- rownames(dgc)
names_dbsm <- rownames(dbsm)

test_that("rownames() works", {
  expect_equal(names_dgc, names_dbsm)
})

names_dgc <- colnames(dgc)
names_dbsm <- colnames(dbsm)

test_that("colnames() works", {
  expect_equal(names_dgc, names_dbsm)
})

names_dgc <- dimnames(dgc)
names_dbsm <- dimnames(dbsm)

test_that("dimnames() works", {
  expect_equal(names_dgc, names_dbsm)
})

# Test auto-generated dimnames when none provided
test_that("dimnames auto-generated when NULL", {
  dgc_nonames <- Matrix::sparseMatrix(
    i = 1:5, j = 1:5, x = 1:5, dims = c(5, 5)
  )
  expect_null(rownames(dgc_nonames))
  expect_null(colnames(dgc_nonames))
  
  dbm <- as.dbMatrix(dgc_nonames, path = tempfile(fileext = ".duckdb"), name = "test")
  
  # Should have auto-generated names
  expect_equal(rownames(dbm), c("row1", "row2", "row3", "row4", "row5"))
  expect_equal(colnames(dbm), c("col1", "col2", "col3", "col4", "col5"))
})

# Test dimnames are character (not factor) when returned
test_that("rownames() returns character, not factor", {
  expect_true(is.character(rownames(dbsm)))
  expect_false(is.factor(rownames(dbsm)))
})

test_that("colnames() returns character, not factor", {
  expect_true(is.character(colnames(dbsm)))
  expect_false(is.factor(colnames(dbsm)))
})

test_that("dimnames() returns list of characters, not factors", {
  dn <- dimnames(dbsm)
  expect_true(is.character(dn[[1]]))
  expect_true(is.character(dn[[2]]))
  expect_false(is.factor(dn[[1]]))
  expect_false(is.factor(dn[[2]]))
})

# Test dimnames preserved after extraction
test_that("rownames preserved after row subset", {
  sub <- dbsm[1:5, ]
  expected <- rownames(dgc)[1:5]
  expect_equal(rownames(sub), expected)
  expect_true(is.character(rownames(sub)))
})

test_that("colnames preserved after col subset", {
  sub <- dbsm[, 1:5]
  expected <- colnames(dgc)[1:5]
  expect_equal(colnames(sub), expected)
  expect_true(is.character(colnames(sub)))
})

test_that("dimnames preserved after row/col subset", {
  sub <- dbsm[1:5, 1:5]
  expect_equal(rownames(sub), rownames(dgc)[1:5])
  expect_equal(colnames(sub), colnames(dgc)[1:5])
  expect_true(is.character(rownames(sub)))
  expect_true(is.character(colnames(sub)))
})

test_that("dimnames preserved after character indexing", {
  row_sel <- rownames(dgc)[1:3]
  col_sel <- colnames(dgc)[1:3]
  sub <- dbsm[row_sel, col_sel]
  expect_equal(rownames(sub), row_sel)
  expect_equal(colnames(sub), col_sel)
})
