#' @param x value to be passed in dbMatrix constructor
#' @noRd
assert_valid_value = function(x) {
  is_mat = inherits(x, 'Matrix') | inherits(x, 'matrix')
  is_duckdb = inherits(x, 'tbl_duckdb_connection')

  if(!(is_mat | is_duckdb)) {
    stopf(
      '\nValue must be a Matrix, matrix, or \'tbl_duckdb_connection\' object.'
    )
  }
}
