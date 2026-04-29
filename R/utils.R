#' @importFrom data.table ":="
NULL

# Print Formatting ####

#' @title Wrap message
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @keywords internal
#' @noRd
wrap_msg <- function(..., sep = ' ') {
  message(wrap_txt(..., sep = sep))
}

#' @title Wrap text
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @keywords internal
#' @noRd
wrap_txt <- function(..., sep = ' ', strWidth = 100, errWidth = FALSE) {
  custom_width <- ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if (!isTRUE(custom_width)) {
    if (isTRUE(errWidth)) strWidth <- getOption('width') - 6
  }

  cat(..., sep = sep) |>
    capture.output() |>
    strwrap(
      prefix = ' ',
      initial = '', # indent later lines, no indent first line
      width = min(80, getOption("width"), strWidth)
    ) |>
    paste(collapse = '\n')
}


# Custom stop function
stopf <- function(...) {
  wrap_txt('dbMatrix: ', ..., errWidth = TRUE) |>
    stop(call. = FALSE)
}


# From a vector, generate a string with the pattern 'item1', 'item2'
#' @keywords internal
#' @noRd
vector_to_string <- function(x) {
  toString(sprintf("'%s'", x))
}

#' @title Generate array for pretty printing of matrix values
#' @param i,j,x matched vectors of integers in i and j, with value in x
#' @param dims dimensions of the array (integer vector of 2)
#' @param fill fill character
#' @param digits default = 5. If numeric, round to this number of digits
#' @keywords internal
print_array <- function(
  i = NULL,
  j = NULL,
  x = NULL,
  dims,
  rownames = rep('', dims[1]),
  class = c('sparse', 'dense'),
  fill = '.',
  digits = 5L
) {
  total_len <- prod(dims)

  # pre-generate filler values
  if (class == "dense") {
    n_digits <- getOption('dbMatrix.digits', default = 7)
    a_vals <- rep(format(round(0, n_digits), nsmall = n_digits), total_len)
  } else {
    a_vals <- rep('.', total_len)
  }

  ijx_nargs <- sum(!is.null(i), !is.null(j), !is.null(x))
  if (ijx_nargs < 3 && ijx_nargs > 1) {
    stopf('All values for i, j, and x must be given when printing')
  }
  if (ijx_nargs == 3) {
    # format numeric
    if (is.numeric(x)) {
      ifelse(
        x < 1e4,
        format(x, digits = digits),
        format(x, digits = digits, scientific = TRUE)
      )
    }
    # populate sparse values by replace nth elements
    # note that i and j index values must be determined outside of this function
    # since the colnames are not known in here
    for (n in seq_along(x)) {
      a_vals[ij_array_map(i = i[n], j = j[n], dims = dims)] <- x[n]
    }
  }

  # print array
  # Ensure max.print is large enough to show the entire preview array
  # otherwise capture.output will truncate and cause NAs in the show method
  op <- options(max.print = total_len + 1000L)
  on.exit(options(op))

  array(a_vals, dims, dimnames = list(rownames, rep('', dims[2]))) |>
    print(quote = FALSE, right = TRUE)
}

# Map row (i) and col (j) indices to nth value of an array vector
#' @keywords internal
#' @noRd
#' @return integer position in array vector the i and j map to
ij_array_map <- function(i, j, dims) {
  # arrays map vector values first by row then by col
  (j - 1) * dims[1] + i
}

# DBI ####

# ## dbDisconnect ####
# #' @title dbDisconnect
# #' @rdname DBI
# #' @export
# setMethod('dbDisconnect', signature(x = 'dbMatrix'),
#           function(x, ...){
#             con <- get_con(x)
#             DBI::dbDisconnect(conn = con, shutdown = TRUE)
#           })
#
# ## dbListTables ####
# #' @title dbListTables
# #' @rdname DBI
# #' @export
# setMethod('dbListTables', signature(x = 'dbMatrix'),
#           function(x, ...){
#             con <- get_con(x)
#             DBI::dbListTables(conn = con)
#           })

# dbMatrix ####

## dbLoad ####
#' Create a dbMatrix object computed in a database
#' @param conn DBIConnection object
#' @param name valid name value (character)
#' @param class character, class of the dbMatrix object (e.g. "dbDenseMatrix" or "dbSparseMatrix")
#' @export
#' @rdname dbMatrix-methods
setMethod(
  'dbLoad',
  signature(conn = 'DBIConnection'),
  function(conn, name, class) {
    .check_con(conn = conn)
    if (!name %in% DBI::dbListTables(conn)) {
      stopf("'name' not found in database")
    }
    if (!class %in% c('dbDenseMatrix', 'dbSparseMatrix')) {
      stopf("Class must be 'dbDenseMatrix' or 'dbSparseMatrix'")
    }

    dim_names <- c(
      paste0("__", name, "_rownames"),
      paste0("__", name, "_colnames")
    )

    # check if rownames and colnames exist
    if (!all(dim_names %in% DBI::dbListTables(conn))) {
      stopf("Dimension names not found. Did you save with dbMatrix::compute?")
    }

    # load values saved from dbMatrix::compute()
    # IMPORTANT: must sort by i/j to ensure correct ordering.
    # DuckDB table scan order is not guaranteed to match insertion order.
    rownames <- dplyr::tbl(conn, dim_names[1]) |>
      dplyr::arrange(i) |>
      dplyr::pull('rownames')
    colnames <- dplyr::tbl(conn, dim_names[2]) |>
      dplyr::arrange(j) |>
      dplyr::pull('colnames')
    dim_names <- list(as.factor(rownames), as.factor(colnames))
    dims <- c(length(rownames), length(colnames))
    value <- dplyr::tbl(conn, name)

    x <- dbMatrix::dbMatrix(
      value = value,
      class = class,
      con = conn,
      name = name,
      dim_names = dim_names,
      dims = dims,
      overwrite = 'PASS'
    )

    return(x)
  }
)

# dbplyr ####

#' Generate table names
#' @details
#' based on dbplyr::unique_table_name
#'
#' @noRd
#' @keywords internal
unique_table_name <- function(prefix = "dbMatrix") {
  vals <- c(letters, LETTERS, 0:9)
  name <- paste0(sample(vals, 10, replace = TRUE), collapse = "")
  paste0(prefix, "_", name)
}

# Helper to check memory limit
#' @keywords internal
#' @noRd
.check_mem_limit <- function(x) {
  limit <- getOption("dbMatrix.max_mem_convert", default = 8 * 1024^3) # 8GB default

  # Estimate size: rows * cols * 8 bytes (double)
  # This is a conservative estimate for dense matrices.
  # For sparse, it might be an overestimate, but safer.
  dims <- dim(x)
  est_size <- as.numeric(dims[1]) * as.numeric(dims[2]) * 8

  if (est_size > limit) {
    stop(sprintf(
      "dbMatrix: Implicit conversion to in-memory matrix blocked.\nEstimated size: %s\nLimit: %s\nIncrease 'dbMatrix.max_mem_convert' option to override.",
      format(structure(est_size, class = "object_size"), units = "auto"),
      format(structure(limit, class = "object_size"), units = "auto")
    ))
  }

  if (getOption("dbMatrix.verbose", default = TRUE)) {
    cli::cli_alert_info(
      "Coercing dbMatrix to in-memory matrix (est. size: {format(structure(est_size, class = 'object_size'), units = 'auto')}). Control with 'dbMatrix.max_mem_convert'. See ?dbMatrix-options for more info."
    )
  }
}

# Temp table cleanup ####

#' Extract temp table names from SQL query
#'
#' Finds all __dbM_* prefixed table names in a SQL query string.
#' These are temporary tables created during operations like subsetting.
#'
#' @param sql Character string of SQL query
#' @return Character vector of temp table names found
#' @keywords internal
#' @noRd
.extract_temp_tables <- function(sql) {
  # Match __dbM_ followed by word characters (alphanumeric + underscore)
  pattern <- "__dbM_[a-zA-Z0-9_]+"
  matches <- regmatches(sql, gregexpr(pattern, sql))[[1]]
  unique(matches)
}

#' Cleanup temp tables from a connection
#'
#' Unregisters and drops temp tables created during dbMatrix operations.
#'
#' @param con DuckDB connection
#' @param tables Character vector of table names to clean up
#' @param verbose Logical, whether to print cleanup messages
#' @keywords internal
#' @noRd
.cleanup_temp_tables <- function(
  con,
  tables,
  verbose = getOption("dbMatrix.verbose", TRUE)
) {
  if (length(tables) == 0) {
    return(invisible(NULL))
  }

  for (tbl in tables) {
    # Try to unregister (for registered views)
    try(duckdb::duckdb_unregister(con, tbl), silent = TRUE)
    # Try to drop table (for materialized temps)
    try(
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS \"", tbl, "\"")),
      silent = TRUE
    )
  }

  if (verbose && length(tables) > 0) {
    cli::cli_alert_success("Cleaned up {length(tables)} temp table(s).")
  }

  invisible(tables)
}

# Named long format conversion ####

#' Convert dbMatrix to named ijx table
#'
#' Converts a `dbMatrix` to a lazy long table where row and column indices are
#' replaced by dimension names.
#'
#' @param x A dbMatrix object (dbSparseMatrix or dbDenseMatrix)
#' @param row_col Name for the row-name column (default: "row_name")
#' @param col_col Name for the column-name column (default: "col_name")
#' @param compute Whether to materialize as temp table (default: FALSE)
#' @return A lazy tbl with columns: row_col, col_col, x
#' @concept dbMatrix
#' @export
to_named_ijx_tbl <- function(
  x,
  row_col = "row_name",
  col_col = "col_name",
  compute = FALSE
) {
  if (!inherits(x, "dbMatrix")) {
    stopf("Input must be a dbMatrix object")
  }

  # Get connection and dimension names
  con <- dbplyr::remote_con(x[])
  row_names <- rownames(x)
  col_names <- colnames(x)

  # Build lookup data.frames
  row_lookup <- data.frame(
    i = seq_along(row_names),
    .row_name_tmp = row_names,
    stringsAsFactors = FALSE
  )
  col_lookup <- data.frame(
    j = seq_along(col_names),
    .col_name_tmp = col_names,
    stringsAsFactors = FALSE
  )

  # Copy to database as temporary tables
  row_tbl <- dplyr::copy_to(
    con,
    row_lookup,
    name = unique_table_name("row_lookup"),
    temporary = TRUE,
    overwrite = TRUE
  )
  col_tbl <- dplyr::copy_to(
    con,
    col_lookup,
    name = unique_table_name("col_lookup"),
    temporary = TRUE,
    overwrite = TRUE
  )

  # Join and rename
  result <- x[] |>
    dplyr::left_join(row_tbl, by = "i") |>
    dplyr::left_join(col_tbl, by = "j") |>
    dplyr::select(
      !!rlang::sym(row_col) := .row_name_tmp,
      !!rlang::sym(col_col) := .col_name_tmp,
      x
    )

  if (compute) {
    result <- dplyr::compute(
      result,
      name = unique_table_name("named_long"),
      temporary = TRUE,
      overwrite = TRUE
    )
  }

  result
}

#' @keywords internal
#' @noRd
.to_named_long <- function(
  x,
  row_col = "row_name",
  col_col = "col_name",
  compute = FALSE
) {
  to_named_ijx_tbl(
    x = x,
    row_col = row_col,
    col_col = col_col,
    compute = compute
  )
}
