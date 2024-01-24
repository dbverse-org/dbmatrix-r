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
# Test name equivalence

names_dgc = names(dgc)
names_dbsm = names(dbsm)

test_that("names() works", {
  expect_equal(names_dgc, names_dbsm)
})


names_dgc = rownames(dgc)
names_dbsm = rownames(dbsm)

test_that("rownames() works", {
  expect_equal(names_dgc, names_dbsm)
})

names_dgc = colnames(dgc)
names_dbsm = colnames(dbsm)

test_that("colnames() works", {
  expect_equal(names_dgc, names_dbsm)
})

names_dgc = dimnames(dgc)
names_dbsm = dimnames(dbsm)

test_that("dimnames() works", {
  expect_equal(names_dgc, names_dbsm)
})
