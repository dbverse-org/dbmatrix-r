# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))

# Connect to the database
con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbSparseMatrix
dbsm = dbMatrix::dbMatrix(value = dgc,
                          con = con1,
                          name = 'mat',
                          class = "dbSparseMatrix",
                          overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# rowSds

test_that("rowSds equal for dbSparseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::rowSds(dgc)
  res_dbsm = rowSds(dbsm, memory = TRUE)
  expect_equal(res_mat, res_dbsm)
})

test_that("rowSds equal for dbSparseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::rowSds(dgc)
  res_dbsm = rowSds(dbsm, memory = FALSE)
  res_dbsm_vec = suppressWarnings(res_dbsm |> as.vector())
  expect_equal(res_mat, res_dbsm_vec)
})

# ---------------------------------------------------------------------------- #
# colSds

test_that("colSds equal for dbSparseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::colSds(dgc)
  res_dbsm = colSds(dbsm, memory = TRUE)
  expect_equal(res_mat, res_dbsm)
})

test_that("colSds equal for dbSparseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::colSds(dgc)
  res_dbsm = colSds(dbsm, memory = FALSE)
  res_dbsm_vec = suppressWarnings(res_dbsm |> as.vector())
  expect_equal(res_mat, res_dbsm_vec)
})

# Close the database connection
DBI::dbDisconnect(con1)
