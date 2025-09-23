#' @keywords internal
#' @noRd
.get_dbMatrix_info <- function(db_sparse) {
  dims <- dim(db_sparse)
  list(
    con = dbplyr::remote_con(db_sparse[]),
    dims = dims,
    dim_names = dimnames(db_sparse),
    remote_name = db_sparse@name,
    n_rows = bit64::as.integer64(dims[1]),
    n_cols = bit64::as.integer64(dims[2])
  )
}

#' @keywords internal
#' @noRd
#' @return string of precompute table name with largest product of dimensions
.find_precompute_table <- function(con, n_rows, n_cols) {
  tables <- DBI::dbListTables(con)
  precompute_names <- tables[grep("^precomp_", tables)]

  if (length(precompute_names) == 0) {
    return(NULL)
  }

  # find name with largest product of dimensions
  dimensions <- strsplit(gsub("precomp_", "", precompute_names), "x")
  # Convert list to matrix for vectorized operations
  dim_matrix <- do.call(rbind, lapply(dimensions, as.integer))

  # Create logical mask for valid dimensions
  valid_dims <- dim_matrix[,1] <= n_rows & dim_matrix[,2] <= n_cols

  # If no valid dimensions found, return NULL
  if (!any(valid_dims)) {
    return(NULL)
  }

  # Calculate Manhattan distance for valid dimensions only
  valid_matrix <- dim_matrix[valid_dims, , drop = FALSE]
  distances <- abs(valid_matrix[,1] - n_rows) +
    abs(valid_matrix[,2] - n_cols)

  # Find index of minimum distance
  min_idx <- which.min(distances)
  res <- paste0('precomp_', valid_matrix[min_idx,][1], 'x',
                valid_matrix[min_idx,][2])

  # Return table name
  return(res)
}


#' @description
#' This function will create a dense COO table if one does not already exist
#' or if the existing table is not large enough for dbMatrix operations.
#' @keywords internal
#' @noRd
#' @return A [`tbl`] object representing the precomputed dense table
.initialize_precompute_matrix <- function(con, n_rows, n_cols) {

  # helper functions ----------------------------------------------------------
  .can_use_existing_matrix <- function(n_rows, n_cols, dims) {
    n_rows <= dims$rows & n_cols <= dims$cols
  }

  .can_use_transposed_matrix <- function(n_rows, n_cols, dims) {
    n_rows <= dims$cols & n_cols <= dims$rows
  }

  .create_new_precompute <- function(con, n_rows, n_cols) {
    cli::cli_alert_info(c(
      "Computing new dense COO table with {n_rows} rows and {n_cols} columns..."
      #"\n See {.help .initialize_precompute_matrix} for details.\n"
    ))
    precompute(conn = con, m = n_rows, n = n_cols)
  }

  .create_transposed_matrix <- function(con, precompute_name, dims) {
    new_precompute_name <- glue::glue("precomp_{dims$cols}x{dims$rows}")
    sql <- glue::glue("
        CREATE OR REPLACE TEMPORARY VIEW {new_precompute_name} AS
        SELECT j AS i, i AS j FROM {precompute_name}
    ")
    invisible(DBI::dbExecute(con, sql))
    dplyr::tbl(con, new_precompute_name)
  }

  .parse_dimensions <- function(precompute_name) {
    precomp_dim <- regmatches(
      precompute_name,
      regexpr("\\d+x\\d+", precompute_name)
    )
    dims <- strsplit(precomp_dim, "x")[[1]]
    list(
      rows = bit64::as.integer64(dims[1]),
      cols = bit64::as.integer64(dims[2])
    )
  }

  # main function --------------------------------------------------------------
  precompute_name <- .find_precompute_table(con, n_rows, n_cols)

  if (is.null(precompute_name)) {
    res <- .create_new_precompute(con, n_rows, n_cols)
  } else {
    dims <- .parse_dimensions(precompute_name)
    if (.can_use_existing_matrix(n_rows, n_cols, dims)) {
      res <- dplyr::tbl(con, precompute_name)
    } else if (.can_use_transposed_matrix(n_rows, n_cols, dims)) {
      res <- .create_transposed_matrix(con, precompute_name, dims)
    } else {
      res <- .create_new_precompute(con, n_rows, n_cols)
    }
  }

  return(res)
}

#' Compute a dense COO table in a database connection
#'
#' @param conn duckdb database connection
#' @param m number of rows of precomputed dbMatrix table
#' @param n number of columns of precomputed dbMatrix table
#' @param verbose logical, print progress messages. default: FALSE.
#' @description
#' Precomputes a COO list table in a specificied database connection in column-
#' major order.
#' This can speed up operations that involve breaking
#' sparsity of a \code{dbSparseMatrix},
#' such as in cases when performing + or - arithmetic operations.
#'
#' @details
#' The \code{m} and \code{n} parameters must exceed the
#' maximum row and column indices of the \code{dbMatrix} in order to be used for
#' densifying any \code{dbMatrix}. If these params are less than the maximum
#' row and column indices, a new precomputed table will be automatically
#' generated with the name 'precomp_mXn'.
#'
#' In such cases, run this function again with a larger
#' \code{n_rows} and \code{num_cols}, or to manually remove the precomputed
#' table set \code{options(dbMatrix.precomp = NULL)} in the R console.
#'
#' @return tbl_dbi
#' @keywords internal
#' @concept dbMatrix
#' @examples
#' con = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' precompute(con = con , m = 100, n = 100)
precompute <- function(conn, m, n, verbose = FALSE){
  # input validation
  .check_con(conn = conn)

  if (!(is.numeric(m)) || !(is.numeric(n))) {
    stop("m and n must be integers or numerics")
  }

  # to prevent R-duckDB integer passing errors and permit >int32 indices
  n_rows = bit64::as.integer64(m)
  n_cols = bit64::as.integer64(n)
  total = n_rows * n_cols

  # Note: this name pattern is parsed in .toDbDense(), do not modify
  name <- paste0("precomp_",n_rows,"x", n_cols)

  # check if we need BIGINT based on INT32 limit
  int32_limit <- bit64::as.integer64(2147483647) # 2^31 - 1
  index_type <- if (n_rows > int32_limit | n_cols > int32_limit) "BIGINT" else "INT"

  # Note: implicit order by j,i for downstream operations
  # TODO: row-major order
  sql <- glue::glue("
  COPY (
    SELECT
      CAST(((row_id.generate_series - 1) % {n_rows} + 1) AS {index_type}) AS i,
      CAST(FLOOR((row_id.generate_series - 1) / {n_rows}) + 1 AS {index_type}) AS j,
      row_id.generate_series - 1 AS idx -- Add row_id as idx directly
    FROM generate_series(1, {total}) AS row_id
  )
  TO '{name}.parquet' (
    FORMAT PARQUET,
    ROW_GROUP_SIZE 1000000,
    COMPRESSION LZ4_RAW
  );
  ")
  invisible(DBI::dbExecute(conn, sql))

  sql <- glue::glue("
    CREATE OR REPLACE TABLE {name} AS
    SELECT * FROM read_parquet('{name}.parquet');
  ")
  invisible(DBI::dbExecute(conn, sql))
  file.remove(paste0(name,'.parquet'))

  key <- dplyr::tbl(conn, name)

  # set global variable for precomputed matrix
  options(dbMatrix.precomp = name)

  if (verbose) {
    str <- glue::glue("Precomputed tbl '{name}' with
                    {n_rows} rows and {n_cols} columns")
    cat(str, "\n")
  }

  return(key)
}
