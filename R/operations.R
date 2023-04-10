

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
  build_call = str2lang(paste0('e1[] %>% dplyr::mutate(x = `',
                               as.character(.Generic)
                               ,'`(e1 = x, e2 = e2))'))
  e1[] = eval(build_call)
  e1
})

#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2)
{
  build_call = str2lang(paste0('e2[] %>% dplyr::mutate(x = `',
                               as.character(.Generic)
                               ,'`(e1 = e1, e2 = x))'))
  e2[] = eval(build_call)
  e2
})

#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2)
{
  if(!identical(e1@dim, e2@dim)) stop('non-conformable arrays\n')

  build_call = str2lang(paste0("e1[] %>%
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) %>%
    dplyr::mutate(x = `", as.character(.Generic), "`(e1 = x, e2 = x.y)) %>%
    dplyr::select(c('i', 'j', 'x'))"))
  e1[] = eval(build_call)
  print(e1[])
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

  if(is.na(x@dim[1L])) {
    res = DBI::dbGetQuery(connection(x), sprintf('SELECT DISTINCT i from %s',
                                                 remoteName(x)))
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
                                                 remoteName(x)))
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
                                                 remoteName(x)))
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






# rownames ####

#' @export
setMethod('rownames', signature(x = 'dbData'), function(x) {
  rownames(x@data)
})

#' @export
setMethod('rownames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[1]]
})


# colnames ####

#' @export
setMethod('colnames', signature(x = 'dbData'), function(x) {
  colnames(x@data)
})

#' @export
setMethod('colnames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[2]]
})

# @export
# setMethod('colnames<-', signature(x = 'dbMatrix'), function(x) {
#
# })


# col classes ####

#' @noRd
remote_col_classes = function(x) {
  x %>%
    head(1L) %>%
    dplyr::collect() %>%
    sapply(class)
}




# head ####
#' @export
setMethod('head', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  slot(x, 'dim_names')[[1L]] = head(slot(x, 'dim_names')[[1L]], n = n)
  x[] = x[] %>% dplyr::filter(i %in% slot(x, 'dim_names')[[1L]])
  x@dim[][1L] = as.integer(n)
  x
})
#' @export
setMethod('head', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x[] = x[] %in% head(x, n = n)
  x
})

# tail ####
#' @export
setMethod('tail', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  slot(x, 'dim_names')[[1L]] = tail(slot(x, 'dim_names')[[1L]], n = n)
  x[] = x[] %>% dplyr::filter(i %in% slot(x, 'dim_names')[[1L]])
  x@dim[][1L] = as.integer(n)
  x
})
#' @export
setMethod('tail', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x[] = x[] %in% tail(x, n = n)
  x
})







# as.matrix ####

# setMethod('as.matrix', signature(x = 'dbMatrix'), function(x) {
#
# })



#' @export
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x[] = x[] %>% dplyr::select(i = j, j = i, x)
  x
})

