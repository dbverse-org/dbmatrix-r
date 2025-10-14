# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))
mat = as.matrix(dgc)

# Connect to the database
con1 = DBI::dbConnect(duckdb::duckdb(), ":memory:")

# Create dbDenseMatrix
dbdm = dbMatrix::dbMatrix(
  value = mat,
  con = con1,
  name = 'mat',
  class = "dbDenseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# rowVars

test_that("rowVars equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::rowVars(mat)
  res_dbdm = rowVars(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("rowVars equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::rowVars(mat)
  res_dbdm = rowVars(dbdm, memory = FALSE)
  res_dbdm_vec = suppressWarnings(res_dbdm |> as.vector())
  expect_equal(res_mat, res_dbdm_vec)
})

# ---------------------------------------------------------------------------- #
# colVars

test_that("colVars equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = MatrixGenerics::colVars(mat)
  res_dbdm = colVars(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("colVars equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = MatrixGenerics::colVars(mat)
  res_dbdm = colVars(dbdm, memory = FALSE)
  res_dbdm_vec = suppressWarnings(res_dbdm |> as.vector())
  expect_equal(res_mat, res_dbdm_vec)
})

# Close the database connection
DBI::dbDisconnect(con1)
