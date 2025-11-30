test_that("compute() works for dbMatrix", {
  # Setup
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # 1. Test dbSparseMatrix
  dbsm <- new(
    "dbSparseMatrix",
    value = dplyr::tbl(con, dplyr::sql("SELECT 1 as i, 1 as j, 1.0 as x")),
    name = "sparse_test",
    dims = as.integer(c(10, 10)),
    dim_names = list(paste0("row", 1:10), paste0("col", 1:10)),
    init = TRUE
  )

  # Call compute
  res <- compute(dbsm, name = "my_computed_table")

  # Checks
  expect_s4_class(res, "dbSparseMatrix")
  expect_equal(res@name, "my_computed_table")
  expect_true(DBI::dbExistsTable(con, "my_computed_table"))

  # Check dimnames persistence
  expect_true(DBI::dbExistsTable(con, "my_computed_table_rownames"))
  expect_true(DBI::dbExistsTable(con, "my_computed_table_colnames"))

  # Test dbLoad reconstruction
  loaded_dbm <- dbLoad(con, "my_computed_table", class = "dbSparseMatrix")
  expect_s4_class(loaded_dbm, "dbSparseMatrix")
  expect_equal(dim(loaded_dbm), dim(dbsm))

  # Check content
  res_df <- res@value |> dplyr::collect()
  expect_equal(nrow(res_df), 1)
  expect_equal(res_df$x, 1.0)

  # 2. Test dbDenseMatrix
  dbdm <- new(
    "dbDenseMatrix",
    value = dplyr::tbl(con, dplyr::sql("SELECT 1 as i, 1 as j, 1.0 as x")),
    name = "dense_test",
    dims = as.integer(c(10, 10)),
    dim_names = list(NULL, NULL),
    init = TRUE
  )

  res_dense <- compute(dbdm, name = "my_dense_table")

  expect_s4_class(res_dense, "dbDenseMatrix")
  expect_equal(res_dense@name, "my_dense_table")
  expect_true(DBI::dbExistsTable(con, "my_dense_table"))

  # 3. Test temporary = FALSE (Persistence)
  res_persist <- compute(dbsm, name = "my_persistent_table", temporary = FALSE)
  expect_true(DBI::dbExistsTable(con, "my_persistent_table"))

  # Verify persists after reconnect
  info <- DBI::dbGetQuery(
    con,
    "SELECT table_type FROM information_schema.tables WHERE table_name = 'my_persistent_table'"
  )
  expect_equal(info$table_type, "BASE TABLE")

  # Verify temp table is temporary
  info_temp <- DBI::dbGetQuery(
    con,
    "SELECT table_type FROM information_schema.tables WHERE table_name = 'my_computed_table'"
  )
  expect_equal(info_temp$table_type, "LOCAL TEMPORARY")
})
