


# queryStack ####
#' @name queryStack-generic
#' @title Get and set the queryStack for a db backend object
#' @inheritParams db_params
#' @aliases queryStack, queryStack<-
#' @include generics.R
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


#' @name query
#' @title Query a database object
#' @description
#' Query a database object. Does not work in a lazy fashion.
#'
#' @param x a database object
#' @param statement query statement in SQL
#' @param ... additional params to pass
#' @export
setMethod('query', signature(x = 'dbDataFrame', statement = 'character'),
          function(x, statement, ...) {
            x = reconnect(x)

            DBI::dbGetQuery(connection(x), statement)
          })

#' @noRd
#' @keywords internal
setMethod('query', signature(x = 'dbMatrix', statement = 'character'),
          function(x, statement, ...) {
            x = reconnect(x)

            DBI::dbGetQuery(connection(x), statement)
          })
