# callGeneric does not seem to work in dplyr chains
# will need to write them out
# setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
# {
#   e1[] = e1[] |> dplyr::mutate(x = callGeneric(e1 = x, e2 = e2))
#   e1
# })

# Math Ops Helpers ####

#' @noRd
ops_ordered_args_vect = function(dbm_narg, a, b) {
  switch(dbm_narg,
         paste0('`(', a, ', ', b, '))'),
         paste0('`(', b, ', ', a, '))'))
}

#' @noRd
arith_call_dbm = function(dbm_narg, dbm, num_vect, generic_char) {
  # order matters
  ordered_args = ops_ordered_args_vect(dbm_narg, 'x', 'num_vect')

  # if not a vector (non-scalar arith)
  if (length(num_vect) > 1L)
    return(arith_call_dbm_vect_multi(dbm, num_vect, generic_char, ordered_args))

  build_call =
    paste0('dbm[] |> dplyr::mutate(x = `', generic_char, ordered_args)

  dbm[] = eval(str2lang(build_call))
  dbm
}

#' @noRd
arith_call_dbm_vect_multi = function(dbm, num_vect, generic_char, ordered_args) {

  # handle dimnames
  r_names = rownames(dbm)
  if (is.factor(r_names)) {
    r_names = 1:length(r_names)
  }

  # perform matching of vect by rownames on dbm
  vect_tbl = dplyr::tibble(i = match(names(num_vect), r_names),
                           num_vect = unname(num_vect[match(names(num_vect),
                                                            r_names)]))

  # run dplyr chain
  build_call = paste0(
    'dbm[] |> ',
    'dplyr::inner_join(vect_tbl, by = \'i\', copy = TRUE) |> ',
    'dplyr::mutate(x = `',
    generic_char,
    ordered_args,
    ' |> ',
    'dplyr::select(i, j, x)'
  )

  dbm[] = eval(str2lang(build_call))

  # } else { # if on disk use faster duckdb cli
  #   # close pool connection
  #   DBI::dbDisconnect(conn, shutdown = TRUE)
  #
  #   # construct query
  #   query <- paste0("UPDATE ", remote_name ," SET x = x ", generic_char, " ", num_vect, ";")
  #
  #   # send query to duckdb cli for faster processing
  #   system(paste("duckdb", db_path, shQuote(query)))
  # }

  # return to pool connector
  # cPool(dbm) = p

  # show
  return(dbm)
}

# Math Ops ####

## Arith: dbm_e2 ####
#' @rdname hidden_aliases
#' @export
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2) {
  dbm = castNumeric(e1)

  num_vect = if(typeof(e2) != 'double'){
    as.numeric(e2)
  } else{
    e2
  }

  # density
  if (class(e1) == 'dbSparseMatrix' && !all(e2 == 0) &&
      as.character(.Generic) %in% c('-', '+')) {
      dbm = toDbDense(dbm)
  }

  arith_call_dbm(
    dbm_narg = 1L,
    dbm = dbm,
    num_vect = num_vect,
    generic_char = as.character(.Generic)
  )
})

## Arith: e1_dbm ####
#' @rdname hidden_aliases
#' @export
setMethod('Arith', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2) {
  dbm = castNumeric(e2)

  num_vect = if (typeof(e1) != 'double'){
    as.numeric(e1)
  } else{
    e1
  }

  # Only densify if not 0 and if op is + or -
  if (class(dbm) == 'dbSparseMatrix' && e1 != 0 && as.character(.Generic) %in% c('-', '+')) {
    dbm = toDbDense(dbm)
  }

  arith_call_dbm(
    dbm_narg = 2L,
    dbm = dbm,
    num_vect = num_vect,
    generic_char = as.character(.Generic)
  )
})

## Arith: dbm_dbm ####
#' @rdname hidden_aliases
#' @export
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2)
{
  if (!identical(e1@dims, e2@dims))
    stopf('non-conformable arrays')

  e1 = castNumeric(e1)
  e2 = castNumeric(e2)

  build_call = str2lang(
    paste0(
      "e1[] |>
      dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y'), copy = TRUE) |>
      dplyr::mutate(x = `",
      as.character(.Generic),
      "`(x, x.y)) |>
      dplyr::select(c('i', 'j', 'x'))"
    )
  )
  e1[] = eval(build_call)
  e1
})


## Ops: dbm_e2 ####
#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
{
  # e1 = reconnect(e1)

  build_call = str2lang(paste0(
    'e1[] |> dplyr::mutate(x = `',
    as.character(.Generic)
    ,
    '`(x, e2))'
  ))
  e1[] = eval(build_call)
  e1
})

## Ops: e1_dbm ####
#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2)
{
  # e2 = reconnect(e2)

  build_call = str2lang(paste0(
    'e2[] |> dplyr::mutate(x = `',
    as.character(.Generic)
    ,
    '`(e1, x))'
  ))
  e2[] = eval(build_call)
  e2
})

## Ops: dbm_dbm ####
#' @rdname hidden_aliases
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2)
{
  if (!identical(e1@dims, e2@dims)){
    stopf('non-conformable arrays')
  }

  build_call = str2lang(
    paste0(
      "e1[] |>
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) |>
    dplyr::mutate(x = `",
      as.character(.Generic),
      "`(x, x.y)) |>
    dplyr::select(c('i', 'j', 'x'))"
    )
  )
  e1[] = eval(build_call)
  # print(e1[])
  e1
})

# Math Summary Ops ####
## rowSums ####

#' @title rowSums
#' @rdname hidden_aliases
#' @export
setMethod('rowSums', signature(x = 'dbDenseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            val_names = rownames(x)

            # calculate rowSums
            vals = x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::collapse() |>
              dplyr::pull(sum_x)

            # set dimname
            names(vals) = val_names

            # show
            vals
          }
        )

#' @title rowSums
#' @rdname hidden_aliases
#' @export
setMethod('rowSums', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            # calc rowsum for nonzero values in ijx
            rowSum = x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::pull(sum_x)

            # get row_idx for non-zero values in ijx
            nonzero_row_indices = x[] |>
              dplyr::arrange(i) |>
              dplyr::pull(i) |>
              unique()

            # format data for join operation
            nonzero_rownames = rownames(x)[nonzero_row_indices]
            rownames_df <- data.frame(rowname = rownames(x),
                                      stringsAsFactors = FALSE)
            rowSum_df <- data.frame(rowname = nonzero_rownames,
                                    value = rowSum,
                                    stringsAsFactors = FALSE)

            # left join to retain order of original dimnames
            merged_df <- dplyr::left_join(rownames_df, rowSum_df,
                                          by = "rowname") |>
                         dplyr::mutate(value = ifelse(is.na(value), 0, value))

            # return rowSums as a named vector
            res <- merged_df$value
            names(res) <- as.factor(merged_df$rowname)

            # show
            res

          })

## colSums ####

#' @title colSums
#' @rdname hidden_aliases
#' @export
setMethod('colSums', signature(x = 'dbDenseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            val_names = colnames(x)

            # calculate colSums
            vals = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::collapse() |>
              dplyr::pull(sum_x)

            # set dimnames
            names(vals) = val_names

            # show
            vals
          })

#' @title colSums
#' @rdname hidden_aliases
#' @export
setMethod('colSums', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            # calc colsum for nonzero values in ijx
            colSum = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::pull(sum_x)

            # get col_idx for non-zero values in ijx
            nonzero_col_indices = x[] |>
              dplyr::arrange(j) |>
              dplyr::pull(j) |>
              unique()

            # format data for join operation
            nonzero_colnames = colnames(x)[nonzero_col_indices]
            colnames_df <- data.frame(colname = colnames(x),
                                      stringsAsFactors = FALSE)
            colSum_df <- data.frame(colname = nonzero_colnames,
                                    value = colSum,
                                    stringsAsFactors = FALSE)

            # left join to retain order of original dimnames
            merged_df <- dplyr::left_join(colnames_df, colSum_df,
                                          by = "colname") |>
                         dplyr::mutate(value = ifelse(is.na(value), 0, value))

            # return rowSums as a named vector
            res <- merged_df$value
            names(res) <- as.factor(merged_df$colname)

            # show
            res
          })



## rowMeans ####

#' @title rowMeans
#' @rdname hidden_aliases
#' @export
setMethod('rowMeans', signature(x = 'dbDenseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            val_names = rownames(x)

            # calculate rowMeans
            vals = x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::collapse() |>
              dplyr::pull(mean_x)

            # set dimnames
            names(vals) = val_names

            # show
            vals
          })

#' @title rowMeans
#' @rdname hidden_aliases
#' @export
setMethod('rowMeans', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            # get non-zero row idx (factors) and convert to integers
            row_indices = x[] |>
              dplyr::arrange(i) |>
              dplyr::pull(i) |>
              unique() |>
              as.integer()

            # get non-zero row names by row idx
            val_names = factor(rownames(x)[row_indices])

            # calculate rowSums
            row_sums = rowSums(x)
            n_cols <- dim(x)[2]
            vals = row_sums / n_cols

            # show
            vals
          })

## colMeans ####

#' @title colMeans
#' @rdname hidden_aliases
#' @export
setMethod('colMeans', signature(x = 'dbDenseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            val_names = colnames(x)
            vals = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::collapse() |>
              dplyr::pull(mean_x)
            names(vals) = val_names
            vals
          })

#' @title colMeans
#' @rdname hidden_aliases
#' @export
setMethod('colMeans', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            x = castNumeric(x)

            # get non-zero column idx (factors) and convert to integers
            col_indices = x[] |>
              dplyr::arrange(j) |>
              dplyr::pull(j) |>
              unique() |>
              as.integer() |>
              sort()

            # get non-zero column names by column idx
            val_names = factor(colnames(x)[col_indices])

            # calculate
            col_sums = colSums(x)
            n_rows <- dim(x)[1]
            vals = col_sums / n_rows

            # show
            vals
          })

## colSds ####

#' @title colSds
#' @rdname hidden_aliases
#' @export
setMethod('colSds', signature(x = 'dbDenseMatrix'),
          function(x, ...)
          {
            # x = reconnect(x)
            x = castNumeric(x)

            val_names = colnames(x)
            vals = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::collapse() |>
              dplyr::pull(sd_x)
            names(vals) = val_names
            vals
          })

#' @title colSds
#' @rdname hidden_aliases
#' @export
setMethod('colSds', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            # x = reconnect(x)
            x = castNumeric(x)

            stop("to be implemented")

            # val_names = colnames(x)
            # vals <- x[] |>
            #   group_by(j) |>
            #   summarise(sd_x = sqrt(mean((x - col_means[j])^2, na.rm = TRUE))) |>
            #   pull(sd_x)
            #
            # vals = x[] |>
            #   dplyr::group_by(j) |>
            #   dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
            #   dplyr::arrange(j) |>
            #   dplyr::collapse() |>
            #   dplyr::pull(sd_x)
            # names(vals) = val_names
            # vals
          })

## rowSds ####
#' @title rowSds
#' @rdname hidden_aliases
#' @export
setMethod('rowSds', signature(x = 'dbDenseMatrix'),
          function(x, ...)
          {
            # x = reconnect(x)
            x = castNumeric(x)

            val_names = rownames(x)
            vals = x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::collapse() |>
              dplyr::pull(sd_x)
            names(vals) = val_names
            vals
          })

#' @title rowSds
#' @rdname hidden_aliases
#' @export
setMethod('rowSds', signature(x = 'dbSparseMatrix'),
          function(x, ...)
          {
            # x = reconnect(x)
            x = castNumeric(x)

            stop("to be implemented")

            # val_names = rownames(x)
            # vals = x[] |>
            #   dplyr::group_by(i) |>
            #   dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
            #   dplyr::arrange(i) |>
            #   dplyr::collapse() |>
            #   dplyr::pull(sd_x)
            # names(vals) = val_names
            # vals
          })

## mean ####

#' @title mean
#' @rdname hidden_aliases
#' @export
setMethod('mean', signature(x = 'dbDenseMatrix'), function(x, ...) {
  x = castNumeric(x)

  res = x[] |>
    dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) |>
    dplyr::pull(mean_x)

  return(res)

})

#' @title mean
#' @rdname hidden_aliases
#' @export
setMethod('mean', signature(x = 'dbSparseMatrix'), function(x, ...) {
  x = castNumeric(x)

  dim = dim(x)
  n = dim[1] * dim[2]

  res = x[] |>
    dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
    dplyr::pull(sum_x)

  res = res / n

  return(res)

})

## log ####

#' @title log
#' @rdname hidden_aliases
#' @export
setMethod('log', signature(x = 'dbMatrix'), function(x, ...) {
  x = castNumeric(x)

  x[] = x[] |>
    dplyr::mutate(x = log(x))

  return(x)

})

# General Ops ####

### t ####

#' @title Transpose
#' @rdname hidden_aliases
#' @export
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x[] = x[] |> dplyr::select(i = j, j = i, x)
  x@dims = c(x@dims[[2L]], x@dims[[1L]])
  x@dim_names = list(x@dim_names[[2L]], x@dim_names[[1L]])
  x
})

### nrow ####

#' @name nrow
#' @title The number of rows/cols
#' @description
#' \code{nrow} and \code{ncol} return the number of rows or columns present in
#' \code{x}.
#' @aliases ncol
#' @export
setMethod('nrow', signature(x = 'dbMatrix'), function(x) {
  # x = reconnect(x)

  if (is.na(x@dims[1L])) {
    conn = pool::localCheckout(cPool(x))
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT i from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[1L])
  }

  return(base::nrow(res))
})

#' @title nrow
#' @rdname hidden_aliases
#' @export
setMethod('nrow', signature(x = 'dbDataFrame'), function(x) {
  # x = reconnect(x)
  dim(x)[1L]
})

### ncol ####

#' @title ncol
#' @rdname hidden_aliases
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {
  # x = reconnect(x)

  if (is.na(x@dims[2L])) {
    conn = pool::localCheckout(cPool(x))
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT j from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[2L])
  }

  return(base::nrow(res))
})

#' @title ncol
#' @rdname hidden_aliases
#' @export
setMethod('ncol', signature(x = 'dbDataFrame'), function(x) {
  # x = reconnect(x)
  ncol(x@data)
})

#' @title ncol
#' @rdname hidden_aliases
#' @export
setMethod('ncol', signature(x = 'dbDataFrame'), function(x) {
  # x = reconnect(x)
  ncol(x@data)
})

### dim ####

#' @title dim
#' @rdname hidden_aliases
#' @export
setMethod('dim',
          signature(x = 'dbMatrix'),
          function(x) {
            if (any(is.na(x@dims))) {
              return(c(nrow(x), ncol(x)))
            } else {
              res = x@dims
            }
          })

### head ####
#' @title head
#' @export
setMethod('head', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset = 1:n
  x[] = x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  return(x)
})

#' @title head
#' @export
setMethod('head', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x[] = x[] %in% head(x, n = n)
  return(x)
})

### tail ####
#' @title tail
#' @export
setMethod('tail', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset = (x@dims[1L] - n):x@dims[1L]
  x[] = x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  return(x)
})

#' @title tail
#' @export
setMethod('tail', signature(x = 'dbDataFrame'), function(x, n = 6L, ...) {
  x[] = x[] %in% tail(x, n = n)
  return(x)
})

# Column data types ####
# Due to how these functions will be commonly seen within other functions, a
# call to `reconnect()` is omitted.

## colTypes ####

#' @name colTypes
#' @title Column data types of dbData objects
#' @description
#' Get the column data types of objects that inherit from \code{'dbData'}
#' @param x dbData data object
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
#' @param x dbData data object
#' @param col column to cast to numeric
#' @param ... additional params to pass
#' @export
setMethod('castNumeric',
          signature(x = 'dbData', col = 'character'),
          function(x, col, ...) {
            if (colTypes(x)[col] != 'double') {
              sym_col = dplyr::sym(col)
              x[] = x[] |> dplyr::mutate(!!sym_col := as.numeric(!!sym_col))
            }
            return(x)
          })

#' @rdname castNumeric
#' @export
setMethod('castNumeric',
          signature(x = 'dbMatrix', col = 'missing'),
          function(x, ...) {
            if (colTypes(x)['x'] != 'double') {
              x[] = x[] |> dplyr::mutate(x := as.numeric(x))
            }
            return(x)
          })
