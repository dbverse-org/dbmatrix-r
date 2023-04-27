

# callGeneric does not seem to work in dplyr chains
# will need to write them out
# setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
# {
#   e1[] = e1[] %>% dplyr::mutate(x = callGeneric(e1 = x, e2 = e2))
#   e1
# })

# Ops ####
#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
{
  e1 = reconnect(e1)

  build_call = str2lang(paste0('e1[] %>% dplyr::mutate(x = `',
                               as.character(.Generic)
                               ,'`(x, e2))'))
  e1[] = eval(build_call)
  e1
})

#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2)
{
  e2 = reconnect(e2)

  build_call = str2lang(paste0('e2[] %>% dplyr::mutate(x = `',
                               as.character(.Generic)
                               ,'`(e1, x))'))
  e2[] = eval(build_call)
  e2
})

#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2)
{
  e1 = reconnect(e1)
  e2 = reconnect(e2)

  if(!identical(e1@dims, e2@dims)) stopf('non-conformable arrays')

  build_call = str2lang(paste0("e1[] %>%
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) %>%
    dplyr::mutate(x = `", as.character(.Generic), "`(x, x.y)) %>%
    dplyr::select(c('i', 'j', 'x'))"))
  e1[] = eval(build_call)
  # print(e1[])
  e1
})















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
  x = reconnect(x)

  if(is.na(x@dims[1L])) {
    conn = pool::localCheckout(cPool(x))
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT i from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[1L])
  }

  return(base::nrow(res))
})



#' @rdname nrow
#' @export
setMethod('nrow', signature(x = 'dbDataFrame'), function(x) {
  x = reconnect(x)

  if(is.na(x@dims[1L])) {
    conn = pool::localCheckout(cPool(x))
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT COUNT(*) AS n FROM %s',
                                               remoteName(x)))
  } else {
    return(x@dims[1L])
  }

  return(as.integer(res))
})







# ncol ####

#' @rdname nrow
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)

  if(is.na(x@dims[2L])) {
    conn = pool::localCheckout(cPool(x))
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT j from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[2L])
  }

  return(base::nrow(res))
})









# dim ####


#' @name dim
#' @title Dimensions of an object
#' @export
setMethod('dim', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)

  if(any(is.na(x@dims))) {
    return(c(nrow(x), ncol(x)))
  } else {
    res = x@dims
  }
})






# rownames ####

#' @export
setMethod('rownames', signature(x = 'dbData'), function(x) {
  x = reconnect(x)
  rownames(x@data)
})

#' @export
setMethod('rownames', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)
  x@dim_names[[1]]
})


# colnames ####

#' @export
setMethod('colnames', signature(x = 'dbData'), function(x) {
  x = reconnect(x)
  colnames(x@data)
})

#' @export
setMethod('colnames', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)
  x@dim_names[[2]]
})

# @export
# setMethod('colnames<-', signature(x = 'dbMatrix'), function(x) {
#
# })


# col classes ####

# Internal function for finding the classes of each of the columns of a lazy
# table
#' @param x lazy table
#' @noRd
remote_col_classes = function(x) {
  x = reconnect(x)

  x %>%
    head(1L) %>%
    dplyr::collect() %>%
    sapply(class)
}




# head ####
#' @export
setMethod('head', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  x = reconnect(x)

  n_subset = x@dim_names[[1L]] = head(x@dim_names[[1L]], n = n)
  x[] = x[] %>% dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  x
})
#' @export
setMethod('head', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x = reconnect(x)

  x[] = x[] %in% head(x, n = n)
  x
})

# tail ####
#' @export
setMethod('tail', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  x = reconnect(x)

  n_subset = x@dim_names[[1L]] = tail(x@dim_names[[1L]], n = n)
  x[] = x[] %>% dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  x
})
#' @export
setMethod('tail', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x = reconnect(x)

  x[] = x[] %in% tail(x, n = n)
  x
})







# as.matrix ####

# setMethod('as.matrix', signature(x = 'dbMatrix'), function(x) {
#
# })



#' @export
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)

  x[] = x[] %>% dplyr::select(i = j, j = i, x)
  x@dims = c(x@dims[[2L]], x@dims[[1L]])
  x@dim_names = list(x@dim_names[[2L]], x@dim_names[[1L]])
  x
})

