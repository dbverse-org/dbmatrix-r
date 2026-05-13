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
  # Convert to numeric to avoid integer64 overflow in comparisons
  n_rows <- as.numeric(n_rows)
  n_cols <- as.numeric(n_cols)

  .attach_precomp_db(con)

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

  dims_str <- gsub("precomp_", "", tables$table_name, fixed = TRUE)
  dims_list <- strsplit(dims_str, "x")

  valid_fmt <- lengths(dims_list) == 2
  if (!any(valid_fmt)) {
    return(NULL)
  }

  candidates <- tables[valid_fmt, ]
  dims_list <- dims_list[valid_fmt]
  dim_matrix <- do.call(rbind, lapply(dims_list, function(x) as.numeric(x)))

  # Check normal orientation
  valid_normal <- dim_matrix[, 1] >= n_rows & dim_matrix[, 2] >= n_cols
  # Check transposed orientation
  valid_transposed <- dim_matrix[, 1] >= n_cols & dim_matrix[, 2] >= n_rows

  if (!any(valid_normal) && !any(valid_transposed)) {
    return(NULL)
  }

  # Find best fit across both orientations
  best_table <- NULL
  best_dist <- Inf
  transposed <- FALSE

  if (any(valid_normal)) {
    m <- dim_matrix[valid_normal, , drop = FALSE]
    c <- candidates[valid_normal, ]
    d <- abs(m[, 1] - n_rows) + abs(m[, 2] - n_cols)
    idx <- which.min(d)
    if (d[idx] < best_dist) {
      best_dist <- d[idx]
      best_table <- c$full_name[idx]
      transposed <- FALSE
    }
  }

  if (any(valid_transposed)) {
    m <- dim_matrix[valid_transposed, , drop = FALSE]
    c <- candidates[valid_transposed, ]
    d <- abs(m[, 1] - n_cols) + abs(m[, 2] - n_rows)
    idx <- which.min(d)
    if (d[idx] < best_dist) {
      best_dist <- d[idx]
      best_table <- c$full_name[idx]
      transposed <- TRUE
    }
  }

  if (is.null(best_table)) {
    return(NULL)
  }

  # Validate structure
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

  list(name = best_table, transposed = transposed)
}

#' @description
#' This function will create a dense COO table if one does not already exist
#' or if the existing table is not large enough.
#' @keywords internal
#' @noRd
.initialize_precompute_matrix <- function(con, n_rows, n_cols) {
  precomp_result <- .find_precompute_table(con, n_rows, n_cols)

  if (is.null(precomp_result)) {
    cli::cli_alert_info(
      "Computing new dense COO table with {n_rows} rows and {n_cols} columns..."
    )
    return(precompute(conn = con, m = n_rows, n = n_cols))
  }

  precompute_name <- precomp_result$name
  use_transposed <- precomp_result$transposed

  precomp <- dplyr::tbl(con, precompute_name)

  # Convert to numeric for dplyr filter compatibility
  nr <- as.numeric(n_rows)
  nc <- as.numeric(n_cols)

  if (use_transposed) {
    precomp <- precomp |>
      dplyr::rename(i_orig = i, j_orig = j) |>
      dplyr::rename(i = j_orig, j = i_orig) |>
      dplyr::filter(i <= !!nr, j <= !!nc)
  } else {
    precomp <- precomp |>
      dplyr::filter(i <= !!nr, j <= !!nc)
  }

  return(precomp)
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
#' @return A `tbl_dbi` object referencing the newly created precomputed lookup
#'   table in DuckDB.
#' @keywords internal
#' @concept dbMatrix
#' @examples
#' \donttest{
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' precompute <- getFromNamespace("precompute", "dbMatrix")
#' precompute(con = con, m = 100, n = 100)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
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
