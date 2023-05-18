

# callGeneric does not seem to work in dplyr chains
# will need to write them out
# setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
# {
#   e1[] = e1[] %>% dplyr::mutate(x = callGeneric(e1 = x, e2 = e2))
#   e1
# })



# Ops helpers ####

#' @noRd
ops_ordered_args_vect = function(dbm_narg, a, b) {
  switch(
    dbm_narg,
    paste0('`(', a, ', ', b, '))'),
    paste0('`(', b, ', ', a, '))')
  )
}

#' @noRd
arith_call_dbm = function(dbm_narg, dbm, num_vect, generic_char) {

  # order matters
  ordered_args = ops_ordered_args_vect(dbm_narg, 'x', 'num_vect')

  if(length(num_vect > 1L))
    return(arith_call_dbm_vect_multi(dbm, num_vect, generic_char, ordered_args))

  build_call =
    paste0('dbm[] %>% dplyr::mutate(x = `', generic_char, ordered_args)

  dbm[] = eval(str2lang(build_call))
  dbm
}

#' @noRd
arith_call_dbm_vect_multi = function(dbm, num_vect, generic_char, ordered_args) {

  p = cPool(dbm)
  conn = pool::localCheckout(p) # create connection to allow temp tables
  cPool(dbm) = conn

  r_names = rownames(dbm)
  vect_tbl = dplyr::tibble(i = r_names, num_vect = num_vect)

  build_call = paste0(
    'dbm[] %>% ',
    'dplyr::inner_join(vect_tbl, by = \'i\', copy = TRUE) %>% ',
    'dplyr::mutate(x = `', generic_char, ordered_args,' %>% ',
    'dplyr::select(i, j, x)'
  )

  dbm[] = eval(str2lang(build_call))
  cPool(dbm) = p # return to pool connector
  dbm
}


# Ops ####
#' @rdname hidden_aliases
#' @export
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
{
  dbm = reconnect(e1)

  dbm = castNumeric(dbm)
  num_vect = if(typeof(e2) != 'double') as.numeric(e2) else e2

  arith_call_dbm(
    dbm_narg = 1L,
    dbm = dbm,
    num_vect = num_vect,
    generic_char = as.character(.Generic)
  )
})

#' @rdname hidden_aliases
#' @export
setMethod('Arith', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2)
{
  dbm = reconnect(e2)

  num_vect = if(typeof(e1) != 'double') as.numeric(e1) else e1
  dbm = castNumeric(dbm)

  arith_call_dbm(
    dbm_narg = 2L,
    dbm = dbm,
    num_vect = num_vect,
    generic_char = as.character(.Generic)
  )
})

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
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2)
{
  e1 = reconnect(e1)
  e2 = reconnect(e2)

  if(!identical(e1@dims, e2@dims)) stopf('non-conformable arrays')

  e1 = castNumeric(e1)
  e2 = castNumeric(e2)

  build_call = str2lang(paste0("e1[] %>%
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) %>%
    dplyr::mutate(x = `", as.character(.Generic), "`(x, x.y)) %>%
    dplyr::select(c('i', 'j', 'x'))"))
  e1[] = eval(build_call)
  e1
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





# rowSums ####
#' @rdname hidden_aliases
#' @export
setMethod('rowSums', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = rownames(x)
            vals = x[] %>%
              dplyr::group_by(i) %>%
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) %>%
              dplyr::arrange(i) %>%
              dplyr::collapse() %>%
              dplyr::pull(sum_x)
            names(vals) = val_names
            vals
          })
# colSums ####
#' @rdname hidden_aliases
#' @export
setMethod('colSums', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = colnames(x)
            vals = x[] %>%
              dplyr::group_by(j) %>%
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) %>%
              dplyr::arrange(j) %>%
              dplyr::collapse() %>%
              dplyr::pull(sum_x)
            names(vals) = val_names
            vals
          })
# rowMeans ####
#' @rdname hidden_aliases
#' @export
setMethod('rowMeans', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = rownames(x)
            vals = x[] %>%
              dplyr::group_by(i) %>%
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) %>%
              dplyr::arrange(i) %>%
              dplyr::collapse() %>%
              dplyr::pull(mean_x)
            names(vals) = val_names
            vals
          })
# colMeans ####
#' @rdname hidden_aliases
#' @export
setMethod('colMeans', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = colnames(x)
            vals = x[] %>%
              dplyr::group_by(j) %>%
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) %>%
              dplyr::arrange(j) %>%
              dplyr::collapse() %>%
              dplyr::pull(mean_x)
            names(vals) = val_names
            vals
          })




# colSds ####
#' @rdname hidden_aliases
#' @export
setMethod('colSds', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = colnames(x)
            vals = x[] %>%
              dplyr::group_by(j) %>%
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) %>%
              dplyr::arrange(j) %>%
              dplyr::collapse() %>%
              dplyr::pull(sd_x)
            names(vals) = val_names
            vals
          })




# rowSds ####
#' @rdname hidden_aliases
#' @export
setMethod('rowSds', signature(x = 'dbMatrix'),
          function(x, ...)
          {
            x = reconnect(x)
            x = castNumeric(x)

            val_names = rownames(x)
            vals = x[] %>%
              dplyr::group_by(i) %>%
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) %>%
              dplyr::arrange(j) %>%
              dplyr::collapse() %>%
              dplyr::pull(sd_x)
            names(vals) = val_names
            vals
          })



# t ####
#' @rdname hidden_aliases
#' @export
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x = reconnect(x)

  x[] = x[] %>% dplyr::select(i = j, j = i, x)
  x@dims = c(x@dims[[2L]], x@dims[[1L]])
  x@dim_names = list(x@dim_names[[2L]], x@dim_names[[1L]])
  x
})





# mean ####
#' @rdname hidden_aliases
#' @export
setMethod('mean', signature(x = 'dbMatrix'), function(x, ...) {
  x = reconnect(x)
  x = castNumeric(x)

  x[] %>%
    dplyr::summarise(mean_x = mean(x)) %>%
    dplyr::pull(mean_x)
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











# drop ####
# @rdname hidden_aliases
# @export
# setMethod('drop', signature(x = 'dbMatrix'), function(x) {
#
# })




# column data types ####
# Due to how these functions will be commonly seen within other functions, a
# call to `reconnect()` is omitted.

## colTypes ####

#' @name colTypes
#' @title Column data types of GiottoDB objects
#' @description
#' Get the column data types of objects that inherit from \code{'dbData'}
#' @param x GiottoDB data object
#' @param ... additional params to pass
#' @export
setMethod('colTypes', signature(x = 'dbData'), function(x, ...) {
  vapply(data.table::as.data.table(head(x[], 1L)), typeof, character(1L))
})


## castNumeric ####

#' @name castNumeric
#' @title Set a column to numeric
#' @description
#' Sets a column to numeric after first checking the column data type. Does
#' nothing if the column is already a \code{double}
#' This precaution is to avoid truncation of values.
#' @param x GiottoDB data object
#' @param col column to cast to numeric
#' @param ... additional params to pass
#' @export
setMethod('castNumeric', signature(x = 'dbData', col = 'character'), function(x, col, ...) {
  if(colTypes(x)[col] != 'double') {
    sym_col = dplyr::sym(col)
    x[] = x[] %>% dplyr::mutate(!!sym_col := as.numeric(!!sym_col))
  }
  x
})

#' @rdname castNumeric
#' @export
setMethod('castNumeric', signature(x = 'dbMatrix', col = 'missing'), function(x, ...) {
  if(colTypes(x)['x'] != 'double') {
    x[] = x[] %>% dplyr::mutate(x := as.numeric(x))
  }
  x
})







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











