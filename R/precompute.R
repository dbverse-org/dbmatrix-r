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
.attach_precomp_db <- function(con) {
  db_path <- getOption("dbMatrix.precomp_db")
  if (is.null(db_path)) {
    return(FALSE)
  }

  if (!file.exists(db_path)) {
    cli::cli_alert_warning(
      "Option 'dbMatrix.precomp_db' is set but file not found: {db_path}"
    )
    return(FALSE)
  }

  # Check if already attached
  dbs <- DBI::dbGetQuery(con, "PRAGMA database_list")
  if ("dbmatrix_precomp" %in% dbs$name) {
    return(TRUE)
  }

  tryCatch(
    {
      DBI::dbExecute(
        con,
        glue::glue("ATTACH '{db_path}' AS dbmatrix_precomp (READ_ONLY)")
      )
      return(TRUE)
    },
    error = function(e) {
      cli::cli_alert_warning("Failed to attach precomputed DB: {e$message}")
      return(FALSE)
    }
  )
}

#' @keywords internal
#' @noRd
.find_precompute_table <- function(con, n_rows, n_cols) {
  # 1. Try to attach external DB if configured
  .attach_precomp_db(con)

  # 2. Search for tables in ALL catalogs
  query <- "SELECT table_catalog, table_schema, table_name FROM information_schema.tables WHERE table_name LIKE 'precomp_%'"

  tables <- tryCatch(
    DBI::dbGetQuery(con, query),
    error = function(e) NULL
  )

  if (is.null(tables) || nrow(tables) == 0) {
    return(NULL)
  }

  tables$full_name <- paste(
    tables$table_catalog,
    tables$table_schema,
    tables$table_name,
    sep = "."
  )

  # 3. Parse dimensions and find best fit
  # table_name format: precomp_ROWSxCOLS
  dims_str <- gsub("precomp_", "", tables$table_name)
  dims_list <- strsplit(dims_str, "x")

  valid_fmt <- lengths(dims_list) == 2
  if (!any(valid_fmt)) {
    return(NULL)
  }

  candidates <- tables[valid_fmt, ]
  dims_list <- dims_list[valid_fmt]

  # Use numeric to handle large numbers
  dim_matrix <- do.call(rbind, lapply(dims_list, function(x) as.numeric(x)))

  # 4. Filter: Must be >= requested dimensions
  valid_mask <- dim_matrix[, 1] >= n_rows & dim_matrix[, 2] >= n_cols

  if (!any(valid_mask)) {
    return(NULL)
  }

  # 5. Select Best Fit (Manhattan distance)
  valid_matrix <- dim_matrix[valid_mask, , drop = FALSE]
  valid_candidates <- candidates[valid_mask, ]

  distances <- abs(valid_matrix[, 1] - n_rows) + abs(valid_matrix[, 2] - n_cols)
  best_table <- valid_candidates$full_name[which.min(distances)]

  # 6. Validate Structure
  tryCatch(
    {
      cols <- DBI::dbGetQuery(con, glue::glue("DESCRIBE {best_table}"))
      if (!all(c("i", "j") %in% cols$column_name)) {
        cli::cli_alert_warning(
          "Found candidate table '{best_table}' but it lacks 'i' or 'j' columns. Skipping."
        )
        return(NULL)
      }
    },
    error = function(e) NULL
  )

  return(best_table)
}

#' @description
#' This function will create a dense COO table if one does not already exist
#' or if the existing table is not large enough.
#' @keywords internal
#' @noRd
.initialize_precompute_matrix <- function(con, n_rows, n_cols) {
  precompute_name <- .find_precompute_table(con, n_rows, n_cols)

  if (is.null(precompute_name)) {
    cli::cli_alert_info(
      "Computing new dense COO table with {n_rows} rows and {n_cols} columns..."
    )
    return(precompute(conn = con, m = n_rows, n = n_cols))
  }

  # Parse dimensions from name
  precomp_dim <- regmatches(
    precompute_name,
    regexpr("\\d+x\\d+", precompute_name)
  )
  dims_parts <- strsplit(precomp_dim, "x")[[1]]
  dims <- list(
    rows = bit64::as.integer64(dims_parts[1]),
    cols = bit64::as.integer64(dims_parts[2])
  )

  # Check if we can use existing or need transpose
  if (n_rows <= dims$rows & n_cols <= dims$cols) {
    return(dplyr::tbl(con, precompute_name))
  } else if (n_rows <= dims$cols & n_cols <= dims$rows) {
    # Transpose case
    new_name <- glue::glue("precomp_{dims$cols}x{dims$rows}")
    sql <- glue::glue(
      "CREATE OR REPLACE TEMPORARY VIEW {new_name} AS SELECT j AS i, i AS j FROM {precompute_name}"
    )
    invisible(DBI::dbExecute(con, sql))
    return(dplyr::tbl(con, new_name))
  } else {
    # Create new
    cli::cli_alert_info(
      "Computing new dense COO table with {n_rows} rows and {n_cols} columns..."
    )
    return(precompute(conn = con, m = n_rows, n = n_cols))
  }
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
precompute <- function(conn, m, n, verbose = FALSE) {
  .check_con(conn = conn)

  if (!(is.numeric(m)) || !(is.numeric(n))) {
    stop("m and n must be integers or numerics")
  }

  n_rows <- bit64::as.integer64(m)
  n_cols <- bit64::as.integer64(n)
  total <- n_rows * n_cols
  name <- paste0("precomp_", n_rows, "x", n_cols)

  # Use BIGINT if needed
  int32_limit <- bit64::as.integer64(2147483647)
  index_type <- if (n_rows > int32_limit | n_cols > int32_limit) {
    "BIGINT"
  } else {
    "INT"
  }

  # Generate grid using parquet for efficient storage/compression
  # Note: implicit order by j,i for downstream operations
  sql <- glue::glue(
    "
    COPY (
      SELECT
        CAST(((row_id.generate_series - 1) % {n_rows} + 1) AS {index_type}) AS i,
        CAST(FLOOR((row_id.generate_series - 1) / {n_rows}) + 1 AS {index_type}) AS j,
        row_id.generate_series - 1 AS idx
      FROM generate_series(1, {total}) AS row_id
    )
    TO '{name}.parquet' (FORMAT PARQUET, ROW_GROUP_SIZE 1000000, COMPRESSION LZ4_RAW);
  "
  )
  invisible(DBI::dbExecute(conn, sql))

  # Load back as table
  invisible(DBI::dbExecute(
    conn,
    glue::glue(
      "CREATE OR REPLACE TABLE {name} AS SELECT * FROM read_parquet('{name}.parquet');"
    )
  ))
  file.remove(paste0(name, ".parquet"))

  if (verbose) {
    cat(
      glue::glue(
        "Precomputed tbl '{name}' with {n_rows} rows and {n_cols} columns"
      ),
      "\n"
    )
  }

  return(dplyr::tbl(conn, name))
}
