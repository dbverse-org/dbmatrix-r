#' Input validation for conn arg
#' @param conn A \link{tbl_duckdb_connection} object
#' @keywords internal
.check_con <- function(conn){
  if(missing(conn)) {
    stop("Please provide a connection")
  }

  if(!DBI::dbIsValid(conn)) {
    stop("Stale connection. Reconnect your db connection.")
  }

  if(!inherits(conn, "duckdb_connection")) {
    stop("conn must be a duckdb connection. Use duckdb drv in DBI::dbConnect()")
  }
}


#' Input validation for data arg
#' @param x A \link{Matrix}, \link{matrix}, or \link{tbl_duckdb_connection} object
#' @keywords internal
.check_value <- function(value){
  is_mat = inherits(value, 'Matrix') | inherits(value, 'matrix')
  is_duckdb = inherits(value, 'tbl_duckdb_connection')
  is_null = is.null(value)
  if(is.character(value)){
    is_valid_file = file.exists(value)
  } else {
    is_valid_file = FALSE
  }

  if(!(is_mat | is_duckdb | is_null | is_valid_file)) {
    stopf(
      'Invalid "value" input passed.'
    )
  }
}

#' Input validation for name arg
#' @keywords internal
.check_name <- function(name){
  if(missing(name)) {
    stop("Please provide a table name")
  }

  if(!is.character(name)) {
    stop("name must be a character string")
  }

  # if name starts with a number, add warning
  if(grepl("^[0-9]", name)) {
    stop("Table names should not start with a number")
  }

  # reserved name check
  reserved_names = c("intersect", "union", "except",
                     "select", "from", "where", "group", "by", "limit",
                     "create", "table", "insert")
  if(name %in% reserved_names){
    stop("Table name cannot be a RESERVED word. Try another name.")
  }
}

#' Input validation for overwrite arg
#' @keywords internal
.check_overwrite <- function(conn, overwrite, name, skip_value_check) {
  if (!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }

  # Check if the object exists (either as a table or a view)
  object_exists <- DBI::dbExistsTable(conn, name)

  if (!overwrite && object_exists) {
    stop("Object already exists. Set overwrite = TRUE to overwrite.")
  }

  # Workaround for preventing deletion of tbl that is being overwritten
  if(skip_value_check){
    return()
  }

  if (overwrite && object_exists) {
    # Determine if the object is a view
    is_view <- DBI::dbGetQuery(conn, glue::glue("
      SELECT COUNT(*) > 0 AS is_view
      FROM duckdb_views()
      WHERE view_name = '{name}'
    "))$is_view

    if (is_view) {
      # Drop the view
      DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {name}"))
    } else {
      # Drop the table
      DBI::dbRemoveTable(conn, name)
    }
  }
}
