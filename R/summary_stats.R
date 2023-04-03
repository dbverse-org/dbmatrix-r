

# summary statistics ####

# nrow ####
#' @name nrow
#' @title The number of rows/cols
#' @description
#' \code{nrow} and \code{ncol} return the number of rows or columns present in
#' \code{x}.
#' @aliases ncol
#' @export
setMethod('nrow', signature(x = 'dbMatrix'), function(x) {

  if(is.na(x@dim[1L])) {
    res = DBI::dbGetQuery(connection(x), sprintf('SELECT DISTINCT i from %s',
                                                 tblName(x)))
  } else {
    res = x@dim[1L]
  }

  return(base::nrow(res))
})



#' @rdname nrow
#' @export
setMethod('nrow', signature(x = 'dbDataFrame'), function(x) {

  if(is.na(x@dim[1L])) {
    res = DBI::dbGetQuery(connection(x), sprintf('SELECT COUNT(*) AS n FROM %s',
                                                 tblName(x)))
  } else {
    res = x@dim[1L]
  }

  return(as.integer(res))
})







# ncol ####

#' @rdname nrow
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {

  if(is.na(x@dim[2L])) {
    res = DBI::dbGetQuery(connection(x), sprintf('SELECT DISTINCT j from %s',
                                                 tblName(x)))
  } else {
    res = x@dim[2L]
  }

  return(base::nrow(res))
})









# dim ####


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
