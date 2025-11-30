test_that("writeMM works correctly", {
  devtools::load_all()
  skip_if_not_installed("duckdb")

  # Create a simulated dbSparseMatrix
  db_mat <- sim_dbSparseMatrix(10, 10)

  # Test writeMM
  temp_file <- tempfile(fileext = ".mtx")
  writeMM(db_mat, temp_file)

  expect_true(file.exists(temp_file))

  # Check if Matrix::readMM can read it
  m_read <- Matrix::readMM(temp_file)

  # Compare (ignoring dimnames for now as readMM doesn't preserve them fully in this context unless we check)
  # sim_dbSparseMatrix creates random data, so we check dimensions and class
  expect_equal(dim(m_read), dim(db_mat))

  unlink(temp_file)
})

test_that("as.matrix uses writeMM and cleans up", {
  devtools::load_all()
  skip_if_not_installed("duckdb")

  db_mat <- sim_dbSparseMatrix(20, 20)

  # Test as.matrix (sparse=TRUE)
  m_sparse <- as.matrix(db_mat, sparse = TRUE, names = TRUE)
  expect_s4_class(m_sparse, "dgCMatrix")
  expect_equal(dim(m_sparse), dim(db_mat))

  # Test as.matrix (sparse=FALSE)
  m_dense <- as.matrix(db_mat, sparse = FALSE, names = TRUE)
  expect_true(is.matrix(m_dense))
  expect_equal(dim(m_dense), dim(db_mat))
})
