


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




sql_gen_simple_filter = function(x, select, where, ...) {
  x = reconnect(x)
  p = cPool(x)

  # SELECT #
  if(missing(select)) select = 'SELECT *'
  else {
    stopifnot(is.character(select))
    select = paste0('SELECT ', '\"', paste(select,  collapse = '\", \"'), '\"')
  }

  # FROM #
  from = paste0('FROM (', dbplyr::sql_render(x[]), ')')

  # WHERE #
  if(missing(where)) where = ''
  else {
    stopifnot(is.character('where'))
    where = paste('WHERE (', paste(where, collapse = ') AND ('), ')')
  }

  statement = paste(select, from, where)
  x[] = dplyr::tbl(src = p, dbplyr::sql(statement))
  x
}


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

            statement = gsub(pattern = ':data:',
                             replacement = paste0('(', dbplyr::sql_render(x[]), ')'),
                             x = statement,
                             fixed = TRUE)

            x[] = dplyr::tbl(src = p, dbplyr::sql(statement))
            x
          })





# values ####
#' @rdname hidden_aliases
#' @importMethodsFrom terra values
#' @description
#' Get cell values from a SpatRaster or attributes from a SpatRaster or dbSpatProxyData
#' Values are only returned as a \code{dbDataFrame} from dbSpatProxyData
#' @param x Spat* object
#' @param ... additional params to pass
#' @export
setMethod('values', signature(x = 'dbSpatProxyData'),
          function(x, ...) {
            x = reconnect(x)
            x@attributes
          })




# Filter the data based on provided SpatExtent. Output is the geom indices provided
# If drop = TRUE (default) then the output will be a bare lazy table
#' @keywords internal
#' @noRd
extent_filter = function(x, extent, drop = TRUE) {
  stopifnot(inherits(extent, 'SpatExtent'))
  x@data = x@data %>%
    dplyr::filter(x > !!as.numeric(extent[]['xmin'])) %>%
    dplyr::filter(x < !!as.numeric(extent[]['xmax'])) %>%
    dplyr::filter(y > !!as.numeric(extent[]['ymin'])) %>%
    dplyr::filter(y < !!as.numeric(extent[]['ymax'])) %>%
    dplyr::collapse() %>%
    dplyr::select('geom')
  ifelse(drop, yes = x@data, no = x)
}





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
#' @param filter SpatVector. Used as a spatial filter to select geometries to
#' read
#' @param select_centroid logical. (default = FALSE) whether spatial filtering
#' through extent or SpatVector is performed based on centroid values
#' @param ... additional params to pass
#' @noRd
setMethod('query', signature(x = 'dbSpatProxyData'),
          function(x, vars = NULL, where = NULL, extent = NULL, filter = NULL, ...) {
            x = reconnect(x)

            # extent subsetting
            if(!is.null(extent)) {
              stopifnot(inherits(extent, 'SpatExtent'))
              x = extent_filter(x = x, extent = extent)
            }

            subset_args = list()
            # attribute table subsetting and records selection
            if(!is.null(vars)) {
              stopifnot(is.character(vars))
              subset_args$select = vars
            }
            if(!is.null(where)) {
              stopifnot(is.character(where))
              subset_args$where = where
            }
            if(!is.null(subset_args) > 0L) {
              subset_args(x = values(x))
              x@attributes = do.call('sql_gen_simple_filter', args = subset_args)
            }

            # filter subsetting
            if(!is.null(filter)) {
              # first filter by extent
              if(inherits(filter, 'giottoPolygon')) filter = filter@spatVector
              if(inherits(filter, 'SpatVector')) e = terra::ext(filter)
              else stopf('filter accepts either SpatVector or giottoPolygon only')
              x = extent_filter(x = x, extent = e)

              # remaining polygon filtering steps can be done by terra::vect()

            }



          })















