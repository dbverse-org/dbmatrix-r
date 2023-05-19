
assert_conn_table = function(x) {
  stopifnot('Input table is not of class "tbl_Pool or tbl_dbi' =
              inherits(x, c('tbl_Pool', 'tbl_dbi')))
}

assert_conn_valid = function(x) {
  stopifnot('Input database table does not have a valid connection' =
              DBI::dbIsValid(dbplyr::remote_con(x)))
}

assert_DT = function(x) {
  stopifnot('Input is not a data.table' =
              inherits(x, 'data.table'))
}

assert_dbData = function(x) {
  stopifnot('Input is not a GiottoDB object' =
              inherits(x, 'dbData'))
}

#' @param x input dplyr tbl to check
#' @param p Pool connection object to check against
#' @noRd
assert_in_backend = function(x, p) {
  rn = as.character(dbplyr::remote_name(x))
  t_exist = DBI::dbExistsTable(p, rn)
  is_pool = inherits(x, 'tbl_Pool')

  if(!all(t_exist, is_pool)) {
    stopf(
      '\ndplyr tbl input must be connected to the database specified by provided database path and be a \'tbl_Pool\'
      Use getBackendPool() to retrieve an appropriate Pool connection object.'
    )
  }
}


