# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))

# Connect to the database
con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbSparseMatrix
dbsm = dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'mat',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# rowVars

test_that("rowVars equal for dbSparseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::rowVars(dgc)
  res_dbsm = rowVars(dbsm, memory = TRUE)
  expect_equal(res_mat, res_dbsm)
})

test_that("rowVars equal for dbSparseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::rowVars(dgc)
  res_dbsm = rowVars(dbsm, memory = FALSE)
  res_dbsm_vec = suppressWarnings(res_dbsm |> as.vector())
  expect_equal(res_mat, res_dbsm_vec)
})

# ---------------------------------------------------------------------------- #
# colVars

test_that("colVars equal for dbSparseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::colVars(dgc)
  res_dbsm = colVars(dbsm, memory = TRUE)
  expect_equal(res_mat, res_dbsm)
})

test_that("colVars equal for dbSparseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::colVars(dgc)
  res_dbsm = colVars(dbsm, memory = FALSE)
  res_dbsm_vec = suppressWarnings(res_dbsm |> as.vector())
  expect_equal(res_mat, res_dbsm_vec)
})

# Close the database connection
DBI::dbDisconnect(con1)
