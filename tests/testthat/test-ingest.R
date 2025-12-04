test_that("as.dbMatrix works for dense matrix", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m <- matrix(1:9, nrow = 3)
  colnames(m) <- paste0("c", 1:3)
  rownames(m) <- paste0("r", 1:3)

  db_mat <- as.dbMatrix(m, con = con)
  expect_s4_class(db_mat, "dbDenseMatrix")
  expect_equal(dim(db_mat), c(3, 3))
  expect_equal(as.matrix(db_mat, names = TRUE), m)
})

test_that("as.dbMatrix handles duplicate column names", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m <- matrix(1:6, nrow = 2)
  colnames(m) <- c("A", "A", "B")
  rownames(m) <- c("r1", "r2")

  db_mat <- as.dbMatrix(m, con = con)
  expect_s4_class(db_mat, "dbDenseMatrix")
  expect_equal(colnames(db_mat), c("A", "A", "B"))
  expect_equal(as.matrix(db_mat, names = TRUE), m)
})

test_that("as.dbMatrix works for sparse matrix", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m <- Matrix::rsparsematrix(10, 10, 0.1)
  # Ensure dimnames for comparison
  dimnames(m) <- list(paste0("r", 1:10), paste0("c", 1:10))

  db_mat <- as.dbMatrix(m, con = con)
  expect_s4_class(db_mat, "dbSparseMatrix")
  expect_equal(as.matrix(db_mat, names = TRUE), as.matrix(m))
})

test_that("as.dbMatrix works for dgeMatrix (dense Matrix)", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m <- Matrix::Matrix(1:9, nrow = 3, sparse = FALSE)
  dimnames(m) <- list(paste0("r", 1:3), paste0("c", 1:3))
  db_mat <- as.dbMatrix(m, con = con)
  expect_s4_class(db_mat, "dbDenseMatrix")
  expect_equal(as.matrix(db_mat, names = TRUE), as.matrix(m))
})

test_that("as.dbMatrix works for dgRMatrix (row-oriented sparse)", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m <- Matrix::rsparsematrix(10, 10, 0.1)
  dimnames(m) <- list(paste0("r", 1:10), paste0("c", 1:10))
  m_r <- as(m, "RsparseMatrix") # dgRMatrix

  # Should be coerced to dgCMatrix and then ingested
  db_mat <- as.dbMatrix(m_r, con = con)
  expect_s4_class(db_mat, "dbSparseMatrix")
  expect_equal(as.matrix(db_mat, names = TRUE), as.matrix(m))
})

test_that("as.dbMatrix fails for unsupported types", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_error(
    as.dbMatrix(data.frame(a = 1), con = con),
    "Unsupported object class"
  )
})

test_that("as.dbMatrix works for lMatrix (logical Matrix)", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  m_log <- Matrix::Matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2)
  dimnames(m_log) <- list(c("r1", "r2"), c("c1", "c2"))

  db_mat <- as.dbMatrix(m_log, con = con)
  expect_s4_class(db_mat, "dbDenseMatrix")

  # Should be ingested as boolean (logical) or 1/0 depending on path
  expected <- as.matrix(m_log) * 1
  expect_equal(as.matrix(db_mat, names = TRUE), expected)
})
