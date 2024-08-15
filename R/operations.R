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

  build_call = paste0('dbm[] |> dplyr::mutate(x = `',
                      generic_char, ordered_args)

  dbm[] = eval(str2lang(build_call))
  dbm
}

#' @noRd
arith_call_dbm_vect_multi = function(dbm, num_vect, generic_char, ordered_args) {
  dim = dim(dbm)
  mat = .recycle_vector_to_matrix(num_vect, dim)
  vect_tbl = as_ijx(mat)

  ordered_args <- if (ordered_args == '`(x, num_vect))') {
    '`(x.x, x.y))'
  } else {
    '`(x.y, x.x))'
  }

  build_call <- glue::glue(
    'dbm[] |> ',
    'dplyr::full_join(vect_tbl, by = c("i", "j"), copy = TRUE) |> ',
    'dplyr::mutate(',
    'x.x = coalesce(x.x, 0), ',
    'x.y = coalesce(x.y, 0), ',
    'x = `', generic_char, ordered_args,' |> ',
    'dplyr::select(i, j, x) |> ',
    'dplyr::filter(x != 0)'
  )

  # }

  dbm[] = eval(str2lang(build_call))

  # show
  return(dbm)
}

#' @noRd
.recycle_vector_to_matrix <- function(vec, dimensions) {
  if (length(vec) == 0) {
    return(matrix(0, nrow = dimensions[1], ncol = dimensions[2]))
  }

  # Recycle the vector to match the total number of elements in the matrix
  recycled_vec <- rep(vec, length.out = prod(dimensions))

  # Create the matrix column-wise
  mat <- matrix(
    recycled_vec,
    nrow = dimensions[1],
    ncol = dimensions[2],
    byrow = FALSE  # This ensures column-wise filling
  )

  # Throw warning if length of vec is not a multiple of dimensions[2]
  # if (length(vec) %% dimensions[2] != 0) {
  #   warning('longer object length is not a multiple of shorter object length',
  #           call. = FALSE)
  # }

  return(mat)
}

# Math Ops ####

## Arith: dbm_e2 ####
#' Arith dbMatrix, e2
#' @description
#' See ?\link{\code{methods::Arith}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2) {
  if (any(e2 == 0) && as.character(.Generic) %in% c('/', '^', '%%', '%/%')) {
    stopf("Arith operations with '/', '^', '%%', '%/%' containing zero values are not yet supported for dbMatrix objects.")
  }

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
#' Arith e1, dbMatrix
#' @description
#' See ?\link{\code{methods::Arith}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Arith', signature(e1 = 'ANY', e2 = 'dbMatrix'),
          function(e1, e2) {
  if (any(e1 == 0) && as.character(.Generic) %in% c('/', '^', '%%', '%/%')) {
    stopf("Arith operations with '/', '^', '%%', '%/%' containing zero values are not yet supported for dbMatrix objects.")
  }
  dbm = castNumeric(e2)

  num_vect = if (typeof(e1) != 'double'){
    as.numeric(e1)
  } else{
    e1
  }

  # Only densify if not 0 and if op is + or -
  if (class(dbm) == 'dbSparseMatrix' && any(e1 != 0) && as.character(.Generic) %in% c('-', '+')) {
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
#' Arith dbMatrix, e2
#' @description
#' See ?\link{\code{methods::Arith}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Arith', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'),
          function(e1, e2){
  if (!identical(e1@dims, e2@dims)) {
    stopf('non-conformable arrays')
  }
  generic_char = as.character(.Generic)

  if(generic_char %in% c('-','/', '^', '%%', '%/%')){
    stopf("Arith operations with '-', '/', '^', '%%', '%/%' are not yet supported between dbMatrix objects.")
  }

  e1 = castNumeric(e1)
  e2 = castNumeric(e2)

  build_call = glue::glue(
    "e1[] |>
     dplyr::left_join(e2[], by = c('i', 'j')) |>
     dplyr::mutate(x = `{generic_char}`(x.x, x.y)) |>
     dplyr::select(i, j, x)"
  )
  e1[] = eval(str2lang(build_call))
  e1
})


## Ops: dbm_e2 ####
#' Ops dbMatrix, e2
#' @description
#' See ?\link{\code{methods::Ops}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2)
{

  build_call = glue::glue(
    'e1[] |> dplyr::mutate(x = `',
    as.character(.Generic)
    ,
    '`(x, e2))'
  )
  e1[] = eval(str2lang(build_call))
  e1
})

## Ops: e1_dbm ####
#' Ops e1, dbMatrix
#' @description
#' See ?\link{\code{methods::Ops}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Ops', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2)
{
  # e2 = reconnect(e2)

  build_call = glue::glue(
    'e2[] |> dplyr::mutate(x = `',
    as.character(.Generic)
    ,
    '`(e1, x))'
  )
  e2[] = eval(str2lang(build_call))
  e2
})

## Ops: dbm_dbm ####
#' Ops dbMatrix, dbMatrix
#' @description
#' See ?\link{\code{methods::Ops}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'dbMatrix'), function(e1, e2) {
  if (!identical(e1@dims, e2@dims)){
    stopf('non-conformable arrays')
  }

  build_call = glue::glue(
      "e1[] |>
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) |>
    dplyr::mutate(x = `",
      as.character(.Generic),
      "`(x, x.y)) |>
    dplyr::select(c('i', 'j', 'x'))"
    )
  e1[] = eval(str2lang(build_call))
  e1
})

# Math Summary Ops ####
## rowSums dbdm ####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::rowSums}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
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

## rowSums dbsm ####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::rowSums}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('rowSums', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            # calc rowsum for nonzero values in ijx
            rowSum = x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::pull(sum_x)

            # get row_idx for non-zero values in ijx
            nonzero_row_indices = x[] |>
              dplyr::distinct(i) |>
              dplyr::arrange(i) |>
              dplyr::pull(i)

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

## colSums dbdm####

#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::colSums}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
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

## colSums dbsm ####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::colSums}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('colSums', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            # calc colsum for nonzero values in ijx
            colSum = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::pull(sum_x)

            # get col_idx for non-zero values in ijx
            nonzero_col_indices = x[] |>
              dplyr::distinct(j) |>
              dplyr::arrange(j) |>
              dplyr::pull(j)

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

## rowMeans dbdm ####

#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::rowMeans}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('rowMeans', signature(x = 'dbDenseMatrix'),
          function(x, ...){
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

## rowMeans dbsm ####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::rowMeans}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('rowMeans', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            # get non-zero row idx (factors) and convert to integers
            row_indices = x[] |>
              dplyr::distinct(i) |>
              dplyr::arrange(i) |>
              dplyr::pull(i) |>
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

## colMeans dbdm####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::colMeans}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('colMeans', signature(x = 'dbDenseMatrix'),
          function(x, ...){
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

## colMeans dbsm ####
#' Form Row and Column Sums and Means
#' @description
#' See ?\link{\code{base::colMeans}} for more details.
#' @concept summary
#' @rdname row_col_sums_means
#' @export
setMethod('colMeans', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x = castNumeric(x)

            # get non-zero column idx (factors) and convert to integers
            col_indices = x[] |>
              dplyr::distinct(j) |>
              dplyr::arrange(j) |>
              dplyr::pull(j) |>
              as.integer()

            # get non-zero column names by column idx
            val_names = factor(colnames(x)[col_indices])

            # calculate
            col_sums = colSums(x)
            n_rows <- dim(x)[1]
            vals = col_sums / n_rows

            # show
            vals
          })

## colSds dbdm ####

#' Calculates the standard deviation for each row (column) of a matrix-like object
#' @description
#' See ?\link{\code{MatrixGenerics::colSds}} for more details.
#' @concept summary
#' @rdname sds
#' @export
setMethod('colSds', signature(x = 'dbDenseMatrix'),
          function(x, ...){
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

## colSds dbsm ####
#' Calculates the standard deviation for each row (column) of a matrix-like object
#' @description
#' See ?\link{\code{MatrixGenerics::colSds}} for more details.
#' @concept summary
#' @rdname sds
#' @export
setMethod('colSds', signature(x = 'dbSparseMatrix'),
          function(x, ...){
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

## rowSds dbdm####
#' Calculates the standard deviation for each row (column) of a matrix-like object
#' @description
#' See ?\link{\code{MatrixGenerics::rowSds}} for more details.
#' @concept summary
#' @rdname sds
#' @export
setMethod('rowSds', signature(x = 'dbDenseMatrix'),
          function(x, ...){
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

## rowSds dbsm ####
#' Calculates the standard deviation for each row (column) of a matrix-like object
#' @description
#' See ?\link{\code{MatrixGenerics::rowSds}} for more details.
#' @concept summary
#' @rdname sds
#' @export
setMethod('rowSds', signature(x = 'dbSparseMatrix'),
          function(x, ...){
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

## mean dbdm####

#' Arithmetic Mean
#' @description
#' See ?\link{\code{base::mean}} for more details.
#' @concept summary
#' @rdname mean
#' @export
setMethod('mean', signature(x = 'dbDenseMatrix'), function(x, ...) {
  x = castNumeric(x)

  res = x[] |>
    dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) |>
    dplyr::pull(mean_x)

  return(res)

})

## mean dbsm####
#' Arithmetic Mean
#' @description
#' See ?\link{\code{base::mean}} for more details.
#' @concept summary
#' @rdname mean
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

#' Logarithms and Exponentials
#' @description
#' See ?\link{\code{base::log}} for more details.
#' @concept transform
#' @export
setMethod('log', signature(x = 'dbMatrix'), function(x, ...) {
  x = castNumeric(x)

  x[] = x[] |>
    dplyr::mutate(x = log(x))

  return(x)

})

# General Ops ####

### t ####

#' Matrix Transpose
#' @description
#' See ?\link{\code{base::t}} for more details.
#' @concept transform
#' @export
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x[] = x[] |> dplyr::select(i = j, j = i, x)
  x@dims = c(x@dims[[2L]], x@dims[[1L]])
  x@dim_names = list(x@dim_names[[2L]], x@dim_names[[1L]])
  x
})

### nrow ####

#' The Number of Rows/Columns of an Array
#' @description
#' See ?\link{\code{base::nrow}} for more details.
#' @concept matrix_props
#' @rdname nrow_ncol
#' @export
setMethod('nrow', signature(x = 'dbMatrix'), function(x) {
  # x = reconnect(x)

  if (is.na(x@dims[1L])) {
    conn = get_con(x)
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT i from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[1L])
  }

  return(base::nrow(res))
})

### ncol ####

#' The Number of Rows/Columns of an Array
#' @description
#' See ?\link{\code{base::ncol}} for more details.
#' @concept matrix_props
#' @rdname nrow_ncol
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {
  # x = reconnect(x)

  if (is.na(x@dims[2L])) {
    conn = get_con(x)
    res = DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT j from %s',
                                               remoteName(x)))
  } else {
    return(x@dims[2L])
  }

  return(base::nrow(res))
})

### dim ####

#' Dimensions of an Object
#' @description
#' See ?\link{\code{base::dim}} for more details.
#' @concept matrix_props
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
#' Return the First or Last Parts of an Object
#' @description
#' See ?\link{\code{utils::head}} for more details.
#' @concept matrix_props
#' @rdname head_tail
#' @export
setMethod('head', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset = 1:n
  x[] = x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  return(x)
})

### tail ####
#' Return the First or Last Parts of an Object
#' @description
#' See ?\link{\code{utils::tail}} for more details.
#' @concept matrix_props
#' @rdname head_tail
#' @export
setMethod('tail', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset = (x@dims[1L] - n):x@dims[1L]
  x[] = x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] = min(x@dims[1L], as.integer(n))
  return(x)
})

# Column data types ####

## colTypes ####

#' Return the column types of a dbMatrix object
#' @concept matrix_props
#' @rdname colTypes
#' @export
setMethod('colTypes', signature(x = 'dbMatrix'), function(x, ...) {
  vapply(data.table::as.data.table(head(slot(x, "value"), 1L)), typeof, character(1L))
})

## castNumeric ####

#' @title Set a column to numeric
#' @description
#' Sets a column to numeric after first checking the column data type. Does
#' nothing if the column is already a \code{double}
#' This precaution is to avoid truncation of values.
#' @param x dbData data object
#' @param col column to cast to numeric
#' @param ... additional params to pass
#' @noRd
#' @keywords internal
setMethod('castNumeric',
          signature(x = 'dbMatrix', col = 'character'),
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
