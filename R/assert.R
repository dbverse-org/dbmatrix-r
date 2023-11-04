#' @param x remote table object
#' @param p DBI connection object to check against
#' @noRd
assert_in_backend = function(x, conn) {
  rn = as.character(dbplyr::remote_name(x))
  t_exist = DBI::dbExistsTable(conn, rn)
  is_duckdb = inherits(x, 'tbl_duckdb_connection')

  if(!all(t_exist, is_duckdb)) {
    stopf(
      '\ndplyr tbl input must be connected to the database specified by provided
      database path and be a \'tbl_duckdb_connection\'.'
    )
  }
}
