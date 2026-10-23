# log() on a sparse dbMatrix transforms only the stored values and leaves the
# zeros at 0. Log normalization (e.g. in Giotto) adds a pseudocount to the
# stored values and then calls log(), so it relies on this.
test_that("log() on a sparse matrix transforms only the stored values", {
  old <- options(dbMatrix.allow_densify = FALSE, dbMatrix.verbose = FALSE)
  on.exit(options(old))
  m <- matrix(c(1, 2, 0, 4, 0, 8), 2)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  x <- dbMatrix(value = as(m, "CsparseMatrix"), con = con, name = "sparse_log",
                class = "dbSparseMatrix", overwrite = TRUE)

  expected <- ifelse(m == 0, 0, log(m))
  expect_equal(unname(as.matrix(log(x))), expected)
  expect_equal(unname(as.matrix(log2(x))), expected / log(2))
})
