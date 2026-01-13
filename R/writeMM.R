#' @importFrom Matrix writeMM
#' @export
#' @param obj dbMatrix object
#' @param file path to file
#' @param ... additional arguments
#' @rdname dbMatrix-methods
setMethod("writeMM", signature(obj = "dbMatrix"), function(obj, file, ...) {
  # 1. Get dimensions and nnz
  dims <- dim(obj)
  n_rows <- dims[1]
  n_cols <- dims[2]

  # Count non-zeros (this might be expensive if not cached, but necessary for header)
  # For dbSparseMatrix, it's the count of rows in the table.
  # For dbDenseMatrix, it's also the count (since we store triplets).
  # We can use tally() which is fast in DuckDB.
  nnz <- obj[] |>
    dplyr::tally() |>
    dplyr::pull(n)

  # 2. Prepare header
  # Standard Matrix Market header for sparse coordinate real matrix
  header <- c(
    "%%MatrixMarket matrix coordinate real general",
    "%",
    sprintf("%d %d %d", n_rows, n_cols, nnz)
  )

  # 3. Write data using DuckDB COPY
  # We write to a temporary file first because COPY overwrites
  temp_data_file <- tempfile(fileext = ".mtx_part")

  con <- get_con(obj)
  sql <- dbplyr::sql_render(obj[])

  # DuckDB COPY command
  # We want space delimiter, no header, no quotes
  copy_sql <- glue::glue(
    "COPY ({sql}) TO '{temp_data_file}' (FORMAT CSV, DELIMITER ' ', HEADER FALSE, QUOTE '');"
  )

  DBI::dbExecute(con, copy_sql)

  # 4. Combine header and data
  # Write header to target file
  writeLines(header, file)

  # Append data
  # Efficiently append using file.append (binary copy)
  # Note: file.append works on file paths
  file.append(file, temp_data_file)

  # 5. Cleanup
  unlink(temp_data_file)

  return(invisible(TRUE))
})
