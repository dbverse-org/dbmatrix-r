test_that("dbMatrix_from_tbl creates dbSparseMatrix from tbl", {
  # Setup: Create a simple test table
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  test_data <- data.frame(
    gene = c("gene1", "gene1", "gene2", "gene2", "gene3"),
    cell = c("cell1", "cell2", "cell1", "cell3", "cell1")
  )

  tbl <- dplyr::copy_to(con, test_data, name = "test_data", overwrite = TRUE)

  # Test: Create dbMatrix from tbl
  result <- dbMatrix_from_tbl(
    tbl = tbl,
    rownames_colName = "gene",
    colnames_colName = "cell",
    name = "test_matrix",
    overwrite = TRUE
  )

  # Assertions
  expect_s4_class(result, "dbSparseMatrix")
  expect_equal(dim(result), c(3L, 3L)) # 3 genes x 3 cells
  expect_equal(rownames(result), c("gene1", "gene2", "gene3"))
  expect_equal(colnames(result), c("cell1", "cell2", "cell3"))

  # Check the counts are correct
  mat <- as.matrix(result, sparse = TRUE, names = TRUE)
  expect_equal(mat["gene1", "cell1"], 1)
  expect_equal(mat["gene1", "cell2"], 1)
  expect_equal(mat["gene2", "cell1"], 1)
  expect_equal(mat["gene2", "cell3"], 1)
  expect_equal(mat["gene3", "cell1"], 1)

  # Cleanup
  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("dbMatrix_from_tbl handles multiple occurrences", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create data with repeated gene-cell combinations
  test_data <- data.frame(
    gene = c("gene1", "gene1", "gene1", "gene2"),
    cell = c("cell1", "cell1", "cell2", "cell1")
  )

  tbl <- dplyr::copy_to(con, test_data, overwrite = TRUE)

  result <- dbMatrix_from_tbl(
    tbl = tbl,
    rownames_colName = "gene",
    colnames_colName = "cell",
    overwrite = TRUE
  )

  # Assertions: counts should reflect multiple occurrences
  mat <- as.matrix(result, sparse = TRUE, names = TRUE)
  expect_equal(mat["gene1", "cell1"], 2) # gene1-cell1 appears twice
  expect_equal(mat["gene1", "cell2"], 1)
  expect_equal(mat["gene2", "cell1"], 1)

  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("dbMatrix_from_tbl validates inputs", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  test_data <- data.frame(gene = "gene1", cell = "cell1")
  tbl <- dplyr::copy_to(con, test_data, overwrite = TRUE)

  # Test missing rownames_colName
  expect_error(
    dbMatrix_from_tbl(tbl, rownames_colName = NULL, colnames_colName = "cell"),
    "rownames_colName and colnames_colName must be provided"
  )

  # Test invalid column names
  expect_error(
    dbMatrix_from_tbl(
      tbl,
      rownames_colName = "invalid",
      colnames_colName = "cell"
    ),
    "rownames_colName and colnames_colName must be present in tbl colnames"
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
})
