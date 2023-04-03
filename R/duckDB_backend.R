

# connection ####

#' @name con-generic
#' @title Database connection
#' @description Return the database connection object
#' @include generics.R
NULL

#' @rdname con-generic
#' @export
setMethod('con', signature(x = 'dbMatrix'), function(x) x@connection)

#' @rdname con-generic
#' @export
setMethod('con', signature(x = 'dbDataFrame'), function(x) x@connection)



#' @name tblName-generic
#' @title Database table name
#' @description
#' Returns the table name within the database the the object acts as a link to
#' @include generics.R
NULL

#' @rdname tblName-generic
#' @export
setMethod('tblName', signature(x = 'dbMatrix'), function(x) x@table_name)
#' @rdname tblName-generic
#' @export
setMethod('tblName', signature(x = 'dbDataFrame'), function(x) x@table_name)








# summary statistics ####

#' @name nrow
#' @title The number of rows/cols
#' @description
#' \code{nrow} and \code{ncol} return the number of rows or columns present in
#' \code{x}.
#' @aliases ncol
#' @export
setMethod('nrow', signature(x = 'dbMatrix'), function(x) {

  if(is.na(x@dim[1L])) {
    res = DBI::dbGetQuery(con(x), sprintf('SELECT DISTINCT i from %s',
                                          tblName(x)))
  } else {
    res = x@dim[1L]
  }

  return(base::nrow(res))
})

#' @rdname nrow
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {

  if(is.na(x@dim[2L])) {
    res = DBI::dbGetQuery(con(x), sprintf('SELECT DISTINCT j from %s',
                                          tblName(x)))
  } else {
    res = x@dim[2L]
  }

  return(base::nrow(res))
})





#' @rdname nrow
#' @export
setMethod('nrow', signature(x = 'dbDataFrame'), function(x) {

  if(is.na(x@dim[1L])) {
    res = DBI::dbGetQuery(con(x), sprintf('SELECT COUNT(*) AS n FROM %s',
                                          tblName(x)))
  } else {
    res = x@dim[1L]
  }

  return(as.integer(res))
})


#' setMethod('ncol', signature(x = 'dbDataFrame'), function(x) {
#'
#'   if(is.na(x@dim[2L])) {
#'     res = DBI::dbGetQuery(con(x), sprintf('SELECT DISTINCT j from %s',
#'                                           tblName(x)))
#'   } else {
#'     res = x@dim[2L]
#'   }
#'
#'   return(as.integer())
#' })








#' @name dim
#' @title Dimensions of an object
#' @export
setMethod('dim', signature(x = 'dbMatrix'), function(x) {
  if(is.na(x@dim)) {
    return(c(nrow(x), ncol(x)))
  } else {
    res = x@dim
  }
})





# operations ####


# setMethod('rowSums', signature(x = 'dbMatrix'), function(x) {
#
# })
