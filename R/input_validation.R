
#' Input validation for data arg
#' @param value A `Matrix`, `matrix`, or `tbl_duckdb_connection` object
#' @return No return value. Called for input validation and throws an error if
#'   `value` is invalid.
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
