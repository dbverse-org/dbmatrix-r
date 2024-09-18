
#' @importFrom data.table ":="
NULL

# Print Formatting ####

#' @title Wrap message
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @keywords internal
#' @noRd
wrap_msg = function(..., sep = ' ') {
  message(wrap_txt(..., sep = sep))
}

#' @title Wrap text
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @keywords internal
#' @noRd
wrap_txt = function(..., sep = ' ', strWidth = 100, errWidth = FALSE) {
  custom_width = ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if(!isTRUE(custom_width)) {
    if(isTRUE(errWidth)) strWidth = getOption('width') - 6
  }

  cat(..., sep = sep) |>
    capture.output() |>
    strwrap(prefix =  ' ', initial = '', # indent later lines, no indent first line
            width = min(80, getOption("width"), strWidth)) |>
    paste(collapse = '\n')
}



# Custom stop function
stopf = function(...) {
  wrap_txt('dbMatrix:\n', ..., errWidth = TRUE) |>
    stop(call. = FALSE)
}


# From a vector, generate a string with the pattern 'item1', 'item2'
#' @keywords internal
#' @noRd
vector_to_string = function(x) {
  toString(sprintf("'%s'", x))
}

#' @title Generate array for pretty printing of matrix values
#' @param i,j,x matched vectors of integers in i and j, with value in x
#' @param dims dimensions of the array (integer vector of 2)
#' @param fill fill character
#' @param digits default = 5. If numeric, round to this number of digits
#' @keywords internal
print_array = function(i = NULL,
                       j = NULL,
                       x = NULL,
                       dims,
                       rownames = rep('', dims[1]),
                       fill = '.',
                       digits = 5L) {
  total_len = prod(dims)

  # pre-generate filler values
  a_vals = rep('.', total_len)

  ijx_nargs = sum(!is.null(i), !is.null(j), !is.null(x))
  if(ijx_nargs < 3 && ijx_nargs > 1) {
    stopf('All values for i, j, and x must be given when printing')
  }
  if(ijx_nargs == 3) {
    # format numeric
    if(is.numeric(x)) {
      ifelse(x < 1e4,
             format(x, digits = digits),
             format(x, digits = digits, scientific = TRUE))
    }
    # populate sparse values by replace nth elements
    # note that i and j index values must be determined outside of this function
    # since the colnames are not known in here
    for(n in seq_along(x)) {
      a_vals[ij_array_map(i = i[n], j = j[n], dims = dims)] <- x[n]
    }
  }

  # print array
  array(a_vals, dims, dimnames = list(rownames, rep('', dims[2]))) |>
    print(quote = FALSE, right = TRUE)
}

# Map row (i) and col (j) indices to nth value of an array vector
#' @keywords internal
#' @noRd
#' @return integer position in array vector the i and j map to
ij_array_map = function(i, j, dims) {
  # arrays map vector values first by row then by col
  (j - 1) * dims[1] + i
}

# DBI ####

#' ## dbDisconnect ####
#' #' @title dbDisconnect
#' #' @rdname DBI
#' #' @export
#' setMethod('dbDisconnect', signature(x = 'dbMatrix'),
#'           function(x, ...){
#'             con <- get_con(x)
#'             DBI::dbDisconnect(conn = con, shutdown = TRUE)
#'           })
#'
#' ## dbListTables ####
#' #' @title dbListTables
#' #' @rdname DBI
#' #' @export
#' setMethod('dbListTables', signature(x = 'dbMatrix'),
#'           function(x, ...){
#'             con <- get_con(x)
#'             DBI::dbListTables(conn = con)
#'           })

# dbMatrix ####

## load ####
#' Create a dbMatrix object computed in a database
#' @export
#' @noRd
setMethod('load', signature(conn = 'DBIConnection'), function(conn, name, class) {
  .check_con(conn = conn)
  if (!name %in% DBI::dbListTables(conn)) {
    stopf("'name' not found in database")
  }
  if (!class %in% c('dbDenseMatrix', 'dbSparseMatrix')) {
    stopf("Class must be 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  dim_names <- c(paste0(name, "_rownames"), paste0(name, "_colnames"))

  # check if rownames and colnames exist
  if (!all(dim_names %in% DBI::dbListTables(conn))) {
    stopf("Dimension names not found. Did you save with dbMatrix::compute?")
  }

  # load values saved from dbMatrix::compute()
  rownames <- dplyr::tbl(conn, dim_names[1]) |> dplyr::pull('rownames')
  colnames <- dplyr::tbl(conn, dim_names[2]) |> dplyr::pull('colnames')
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
})


# dbData ####
## dbReconnect-con ####
#' @export
#' @noRd
#' @description
#' Refreshes connection to a specific file path to a local database file
setMethod('dbReconnect', signature(x = 'DBIConnection'), function(x, ...){
  if(DBI::dbIsValid(x)){
    cli::cli_alert_info('Connection is already valid')
    return(invisible(x))
  }
  driver <- x@driver
  path <- x@driver@dbdir

  # input validation
  if(!is.character(path)) {
    stopf('Path must be a character string')
  }
  if(!file.exists(path)) {
    stopf('Path does not exist: %s', path)
  }

  con = DBI::dbConnect(driver, path)

  cli::cli_alert_success(paste0('Reconnected to: \n \'', path, '\' \n'))

  return(invisible(con))

})

## dbReconnect-dbMatrix ####
#' @export
#' @noRd
#' @description
#' Refreshes connection to a specific file path to a local database file
setMethod('dbReconnect', signature(x = 'dbMatrix'), function(x, conn){
  if(!DBI::dbIsValid(conn)){
    conn <- dbReconnect(conn) |> suppressMessages()
  }

  if(!x@name %in% DBI::dbListTables(conn)){
    message = paste0('Table \'', x@name, '\' does not exist in the database.')
    stopf(message)
  }

  # TODO: signature(x = DBIConnection) to allow for any DB backend
  x[]$src$con <- conn

  return(x)

})

## dbList ####
#' List remote tables, temporary tables, and views
#' @inheritParams DBI::dbListTables
#' @export
#' @description
#' Pretty prints tables, temporary tables, and views in the database.
#' @details
#' Similar to DBI::dbListTables, but categorizes tables into three categories:
#' * Tables
#' * Temporary Tables (these will be removed when the connection is closed)
#' * Views (these may be removed when the connection is closed)
#'
#' @concept dbData
setMethod('dbList', signature(conn = 'DBIConnection'), function(conn){
  # Query to get table types
  query <- "SELECT table_name, table_type FROM information_schema.tables"
  table_types <- DBI::dbGetQuery(conn, query)

  # Categorize tables based on their types
  tables <- dplyr::filter(table_types, table_type == 'BASE TABLE')
  views <- dplyr::filter(table_types, grepl("VIEW", table_type,
                                            ignore.case = TRUE))
  temp_tables <- dplyr::filter(table_types, grepl("TEMPORARY", table_type,
                                                  ignore.case = TRUE))

  print_category <- function(category_name, items) {
    cat(crayon::green(paste0(category_name, ": \n")))
    if (length(items) == 0) {
      cat("\n")
    } else {
      print(items)
    }
  }

  # Print the results
  print_category("Tables", tables$table_name)
  print_category("Temporary Tables", temp_tables$table_name)
  print_category("Views", views$table_name)
})
# dbplyr ####

#' Generate table names
#' @details
#' based on dbplyr::unique_table_name
#'
#' @noRd
#' @keywords internal
unique_table_name <- function(prefix = ""){
  vals <- c(letters, LETTERS, 0:9)
  name <- paste0(sample(vals, 10, replace = TRUE), collapse = "")
  paste0(prefix, "_", name)
}

## compute #####
#' @export
#' @inheritParams dplyr::compute
#' @param temporary default = TRUE. If FALSE, the table will be persisted
#' in the database. If TRUE, the table will be removed after the session ends.
#' @param dimnames default = TRUE. If TRUE, the rownames and colnames will be
#' saved in the database. This allows full reconstruction of the dbMatrix object
#' using \link{\code{dbMatrix::load()}}.
#' @param ... other args passed to \link{\code{dplyr::compute()}}
#' @noRd
setMethod('compute', signature(x = 'dbMatrix'),
          function(x, temporary = TRUE, dimnames = TRUE, ...) {
  con <- get_con(x)
  .check_con(conn = con)
  dots <- list(...)

  # Determine the name to use
  if (!is.null(dots$name)) {
    name <- dots$name
    dots$name <- NULL  # Remove name from dots to avoid duplication
  } else if (!is.na(x@name)) {
    name <- x@name
  } else {
    stopf("Please provide a 'name' to compute the lazy dbMatrix table")
  }

  # Let dplyr/dbplyr handle errors
  # if (name %in% DBI::dbListTables(con)) {
  #   if (is.null(dots$overwrite) || !dots$overwrite) {
  #     stopf("Table already exists in database. Set 'overwrite'= TRUE")
  #   }
  # } else {
  #   if (!is.null(dots$overwrite) && dots$overwrite) {
  #     stopf("Table does not exist in database. Set 'overwrite'= FALSE")
  #   }
  # }

  # Use do.call to avoid 'name' conflicts with dots
  result <- do.call(dplyr::compute,
                    c(list(x[], name = name, temporary = temporary), dots))

  if(dimnames){
    .write_dimnames(x = x, name = name)
  }

  # Update the dbMatrix object
  x@value <- result
  x@name <- name

  # Return dbMatrix
  return(x)
})
