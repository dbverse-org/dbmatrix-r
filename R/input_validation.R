
#' Input validation for data arg
#' @param value A \link{Matrix}, \link{matrix}, or \link{tbl_duckdb_connection} object
#' @keywords internal
.check_value <- function(value) {
  if (is.character(value)) {
    if (!file.exists(value)) {
      stopf('File does not exist. Please provide a valid file path.')
    }
    return(invisible(NULL))
  }
  is_valid <- inherits(value, c('Matrix', 'matrix', 'tbl_duckdb_connection')) ||
    is.null(value)

  if (!is_valid) {
    stopf('Invalid "value" input passed.')
  }
}
