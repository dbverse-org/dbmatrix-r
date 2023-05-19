

#' @name simulate_duckdb
#' @title Simulate a duckdb connection dplyr tbl_Pool in memory
#' @description
#' Create a simulated lazy table. Useful for testing purposes
#' @param data data to use
#' @keywords internal
#' @noRd
simulate_duckdb = function(data = iris) {
  drv = duckdb::duckdb(dbdir = ':memory:')
  p = pool::dbPool(drv)
  conn = pool::poolCheckout(p)
  duckdb::duckdb_register(conn, df = data, name = 'test')
  pool::poolReturn(conn)
  dplyr::tbl(p, 'test')
}


#' @name simulate_dbDataFrame
#' @title Simulate a dbDataFrame in memory
#' @description
#' Create a simulated dbDataFrame in memory. Useful for testing purposes
#' @param data data to use
#' @keywords internal
#' @noRd
simulate_dbDataFrame = function(data = simulate_duckdb()) {
  if(!inherits(data, 'tbl_lazy'))
    data = simulate_duckdb(data = data)
  dbDataFrame(data = data, remote_name = 'df_test', hash = 'ID_dummy')
}
