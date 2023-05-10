
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

