rlang::local_options(lifecycle_verbosity = "quiet")

.write_tiny_mtx <- function(path) {
  writeLines(
    c(
      "%%MatrixMarket matrix coordinate integer general",
      "% tiny matrix",
      "2 2 2",
      "1 1 3",
      "2 2 4"
    ),
    path
  )
}

.duckdb_table_type <- function(con, name) {
  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT table_type FROM information_schema.tables ",
      "WHERE table_name = ", DBI::dbQuoteString(con, name)
    )
  )$table_type
}

test_that("mtx temporary ingestion can use table or view", {
  old <- getOption("dbMatrix.readMM.temporary_table")
  on.exit(options(dbMatrix.readMM.temporary_table = old), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  mtx <- file.path(temp_dir, "matrix.mtx")
  features <- file.path(temp_dir, "features.tsv")
  barcodes <- file.path(temp_dir, "barcodes.tsv")
  .write_tiny_mtx(mtx)
  writeLines(c("gene1", "gene2"), features)
  writeLines(c("cell1", "cell2"), barcodes)

  options(dbMatrix.readMM.temporary_table = TRUE)
  db_table <- dbMatrix::dbMatrix(
    value = mtx,
    class = "dbSparseMatrix",
    con = con,
    name = "mtx_table",
    overwrite = TRUE,
    mtx_rowname_file_path = features,
    mtx_colname_file_path = barcodes
  )
  expect_equal(.duckdb_table_type(con, "mtx_table"), "LOCAL TEMPORARY")

  options(dbMatrix.readMM.temporary_table = FALSE)
  db_view <- dbMatrix::dbMatrix(
    value = mtx,
    class = "dbSparseMatrix",
    con = con,
    name = "mtx_view",
    overwrite = TRUE,
    mtx_rowname_file_path = features,
    mtx_colname_file_path = barcodes
  )
  expect_equal(.duckdb_table_type(con, "mtx_view"), "VIEW")

  expect_equal(
    as.matrix(db_table, sparse = FALSE, names = FALSE),
    as.matrix(db_view, sparse = FALSE, names = FALSE)
  )
})
