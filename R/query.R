


# queryStack ####
#' @name queryStack-generic
#' @title Get and set the queryStack for a db backend object
#' @inheritParams db_params
#' @aliases queryStack, queryStack<-
#' @export
setMethod('queryStack', signature(x = 'dbData'), function(x) {
  x@data$lazy_query
})

#' @rdname queryStack-generic
#' @export
setMethod('queryStack', signature(x = 'ANY'), function(x) {
  stopifnot('Unable to find an inherited method for \'queryStack\'' =
              inherits(x, 'tbl_lazy'))
  x$lazy_query
})

#' @rdname queryStack-generic
#' @export
setMethod('queryStack<-', signature(x = 'dbData'), function(x, value) {
  x@data$lazy_query = value
  x
})

#' @rdname queryStack-generic
#' @export
setMethod('queryStack<-', signature(x = 'ANY'), function(x, value) {
  stopifnot('Unable to find an inherited method for \'queryStack\'' =
              inherits(x, 'tbl_lazy'))
  x@lazy_query = value
  x
})




# Internal function #
# Accepts tbl_Pool objects #
# Simple SQL querying that allows usage of SELECT and WHERE. Accepts inputs
# for those fields through character string inputs.
# Intended mainly for use through the exported query() generic
# Works with dplyr chains.

#' @param x tbl_Pool object
#' @param select character. See \code{query} documentation below
#' @param where character. See \code{query} documentation below
#' @keywords internal
#' @noRd
sql_gen_simple_filter = function(x, select, where, ...) {
  checkmate::assert_class(x, 'tbl_Pool')

  # SELECT #
  if(missing(select)) select = 'SELECT *'
  else {
    checkmate::assert_character(select)
    select = paste0('SELECT ', '\"', paste(select,  collapse = '\", \"'), '\"')
  }

  # FROM #
  conn = pool::poolCheckout(cPool(x))
  on.exit(try(pool::poolReturn(conn), silent = TRUE))
  from = paste0('FROM (', dbplyr::sql_render(query = x, con = conn), ')')
  pool::poolReturn(conn)

  # WHERE #
  if(missing(where)) where = ''
  else {
    checkmate::assert_character(where)
    where = paste('WHERE (', paste(where, collapse = ') AND ('), ')')
  }

  statement = paste(select, from, where)
  dplyr::tbl(src = cPool(x), dbplyr::sql(statement))
}




# sql_query ####
#' @name sql_query-generic
#' @title Query a database object
#' @description
#' Query a database object using a manually prepared SQL statement.
#' Integrates into dplyr chains. More general purpose than sql_gen* functions
#' @param x a database object
#' @param statement query statement in SQL where the FROM field should be :data:
#' @param ... additional params to pass
#' @details
#' Prepared SQL with properly quoted parameters are provided through the
#' \code{statement} param, where the FROM fields should use the token :data:.
#' This token is then replaced with the rendered SQL chain from the object being
#' queried, effectively appending the new instructions. This set of appended
#' instructions are then used to update the object.
#' @keywords internal
setMethod('sql_query', signature(x = 'dbData', statement = 'character'),
          function(x, statement, ...) {
            x = reconnect(x)
            p = cPool(x)
            conn = pool::poolCheckout(p)
            on.exit(try(pool::poolReturn(conn), silent = TRUE))

            statement = gsub(pattern = ':data:',
                             replacement = paste0('(', dbplyr::sql_render(query  = x[], con = conn), ')'),
                             x = statement,
                             fixed = TRUE)
            pool::poolReturn(conn)

            x[] = dplyr::tbl(src = p, dbplyr::sql(statement))
            x
          })









# TODO polygon filtering and determine method of selection of records and recombining
# query ####
#' @rdname hidden_aliases
#' @importMethodsFrom terra query
#' @description
#' Additional methods for \pkg{terra}'s \code{query()} generic specific to
#' dbSpatVectorProxy objects.
#' NOTE: does not implement the \code{start} and \code{n} params due to lack of
#' row order in database
#' @param x a dbPolygonProxy or dbPointsProxy object
#' @param vars character. Variable (attribute) names. Must be a subset of names(x)
#' @param where character. expression like "NAME_1=’California’ AND ID > 3" ,
#' to subset records.
#' @param extent Spat* object. The extent of the object is used as a spatial
#' filter to select the geometries to read. Ignored if filter is not NULL.
#' @param filter terra SpatVector (optional) that can be used to filter the data
#' @param select_centroid logical. (default = FALSE) whether spatial filtering
#' through extent or SpatVector is performed based on centroid values
#' @param spatvector if TRUE, return as a terra spatvector
#' @param ... additional params to pass
#' @return SpatVector
#' @noRd
setMethod('query', signature(x = 'dbSpatProxyData'),
          function(x, vars = NULL, where = NULL, extent = NULL, filter = NULL,
                   spatvector = FALSE, ...) {
            x = reconnect(x)

            # extent subsetting #
            if(!is.null(extent)) {
              checkmate::assert_class(extent, 'SpatVector')
              x = extent_filter(x = x, extent = extent)
            }

            # attribute table subsetting and records selection by manual input #

            subset_args = list()
            if(!is.null(vars)) {
              checkmate::assert_character(vars)
              subset_args$select = vars
            }
            if(!is.null(where)) {
              checkmate::assert_character(where)
              subset_args$where = where
            }
            if(length(subset_args) > 0L) {
              x = filter_dbspat(x, by_value = function(dbspd) {
                subset_args$x = dbspd
                do.call('sql_gen_simple_filter', args = subset_args)
              })
            }

            # filter subsetting #
            if(!is.null(filter)) {
              # first filter by extent
              if(inherits(filter, 'giottoPolygon')) filter = filter@spatVector
              if(inherits(filter, 'SpatVector')) e = terra::ext(filter)
              else stopf('filter accepts either SpatVector or giottoPolygon only')
              x = extent_filter(x = x, extent = e)

              # remaining polygon filtering steps can be done after pulling in data
            }

            # pull values into memory as SpatVector
            v = dbspat_to_sv(x)
            if(!is.null(filter)) {
              v = terra::crop(v, filter)
            }

            return(v)
          })




# simple queries ####


sql_nrow = function(conn, remote_name) {
  checkmate::assert_character(remote_name, len = 1L)
  p = evaluate_conn(conn, mode = 'pool')
  table_quoted = pool::dbQuoteIdentifier(p, remote_name)

  as.integer(pool::dbGetQuery(
    p,
    paste('SELECT COUNT(*) FROM', table_quoted)
  ))
}




sql_max = function(conn, remote_name, col) {
  checkmate::assert_character(remote_name, len = 1L)
  checkmate::assert_character(col, len = 1L)
  p = evaluate_conn(conn, mode = 'pool')
  table_quoted = pool::dbQuoteIdentifier(p, remote_name)
  col_quoted = pool::dbQuoteIdentifier(p, col)

  as.integer(pool::dbGetQuery(
    p, paste('SELECT MAX(', col_quoted, ') FROM', table_quoted)
  ))
}
sql_min = function(conn, remote_name, col) {
  checkmate::assert_character(remote_name, len = 1L)
  checkmate::assert_character(col, len = 1L)
  p = evaluate_conn(conn, mode = 'pool')
  table_quoted = pool::dbQuoteIdentifier(p, remote_name)
  col_quoted = pool::dbQuoteIdentifier(p, col)

  as.integer(pool::dbGetQuery(
    p, paste('SELECT MIN(', col_quoted, ') FROM', table_quoted)
  ))
}









