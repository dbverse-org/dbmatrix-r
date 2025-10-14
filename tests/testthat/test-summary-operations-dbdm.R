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
# rowSums

test_that("rowSums equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = rowSums(mat)
  res_dbdm = rowSums(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("rowSums equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = rowSums(mat)
  res_dbdm = rowSums(dbdm, memory = FALSE) |> as.vector()
  expect_equal(res_mat, res_dbdm)
})

# ---------------------------------------------------------------------------- #
# colSums

test_that("colSums equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = colSums(mat)
  res_dbdm = colSums(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("colSums equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = colSums(mat)
  res_dbdm = colSums(dbdm, memory = FALSE) |> as.vector()
  expect_equal(res_mat, res_dbdm)
})

# ---------------------------------------------------------------------------- #
# rowMeans

test_that("rowMeans equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = rowMeans(mat)
  res_dbdm = rowMeans(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("rowMeans equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = rowMeans(mat)
  res_dbdm = rowMeans(dbdm, memory = FALSE) |> as.vector()
  expect_equal(res_mat, res_dbdm)
})

# ---------------------------------------------------------------------------- #
# colMeans

test_that("colMeans equal for dbDenseMatrix (memory=TRUE)", {
  res_mat = colMeans(mat)
  res_dbdm = colMeans(dbdm, memory = TRUE)
  expect_equal(res_mat, res_dbdm)
})

test_that("colMeans equal for dbDenseMatrix (memory=FALSE)", {
  res_mat = colMeans(mat)
  res_dbdm = colMeans(dbdm, memory = FALSE) |> as.vector()
  expect_equal(res_mat, res_dbdm)
})

# Close the database connection
DBI::dbDisconnect(con1)
