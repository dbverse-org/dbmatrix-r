


#' @name simulate_objects
#' @title Simulate GiottoDB objects
#' @description
#' Create simulated GiottoDB objects in memory from pre-prepared data. Useful
#' for testing purposes and examples.
#' @param data data to use
#' @param name remote name of table on database to set
NULL


#' @describeIn simulate_objects Simulate a duckdb connection dplyr tbl_Pool in memory
#' @export
simulate_duckdb = function(data = datasets::iris, name = 'test', p = NULL) {
  if(is.null(p)) {
    drv = duckdb::duckdb(dbdir = ':memory:')
    p = pool::dbPool(drv)
  }
  conn = pool::poolCheckout(p)
  duckdb::duckdb_register(conn, df = data, name = name)
  pool::poolReturn(conn)
  dplyr::tbl(p, name)
}


#' @describeIn simulate_objects Simulate a dbDataFrame in memory
#' @export
simulate_dbDataFrame = function(data = NULL, name = 'df_test', key = NA_character_) {
  if(is.null(data)) {
    data = simulate_duckdb(name = name)
  }
  if(!inherits(data, 'tbl_sql')) {
    checkmate::assert_class('data.frame')
    data = simulate_duckdb(data = data, name = name)
  }
  dbDataFrame(data = data, remote_name = name, hash = 'ID_dummy',
              init = TRUE, key = key)
}


#' @describeIn simulate_objects Simulate a dbPointsProxy in memory
#' @export
simulate_dbPointsProxy = function(data = NULL) {
  if(is.null(data)) {
    gpoint = GiottoData::loadSubObjectMini('giottoPoints')
    sv_dt = svpoint_to_dt(gpoint[], include_values = TRUE)
    data = simulate_duckdb(data = sv_dt, name = 'pnt_test')
  }
  dbPointsProxy(data = data, remote_name = 'pnt_test', hash = 'ID_dummy',
                n_point = nrow(sv_dt), init = TRUE, extent = terra::ext(gpoint[]))
}


#' @describeIn simulate_objects Simulate a dbPolygonProxy in memory
#' @export
simulate_dbPolygonProxy = function(data = NULL) {
  if(is.null(data)) {
    gpoly = GiottoData::loadSubObjectMini('giottoPolygon')
    sv_geom = data.table::setDT(terra::geom(gpoly[], df = TRUE))
    sv_atts = data.table::setDT(terra::values(gpoly[]))
    sv_atts[, geom := seq(.N)]
    data.table::setcolorder(sv_atts, neworder = c('geom', 'poly_ID'))

    data = simulate_duckdb(data = sv_geom, name = 'poly_test')
    data_atts = simulate_dbDataFrame(
      simulate_duckdb(sv_atts, p = cPool(data), name = 'poly_test_attr'),
      key = 'geom',
      name = 'poly_test_attr'
    )
  }
  dbPolygonProxy(data = data, remote_name = 'poly_test', hash = 'ID_dummy',
                 n_poly = nrow(sv_atts), init = TRUE, extent = terra::ext(gpoly[]),
                 attributes = data_atts)
}







