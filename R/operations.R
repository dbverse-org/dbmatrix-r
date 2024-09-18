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

  if(length(num_vect) != dim[1] & length(num_vect) != dim[2]){
    # FIXME: .recycle_vector_to_matrix is not OOM
    mat = .recycle_vector_to_matrix(num_vect, dim)
    vect_tbl = as_ijx(mat)

    # handle order
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
  } else {
    r_names = rownames(dbm)
    c_names = colnames(dbm)
    # check to see if all names(num_vect)[1:10] is in r_names or c_names
    if(all(names(num_vect) %in% r_names)){
      vect_tbl = dplyr::tibble(
        i = match(names(num_vect), r_names),
        num_vect = unname(num_vect[match(names(num_vect),r_names)])
      )

      vect_tbl <- arrow::to_duckdb(
        .data = vect_tbl,
        con = dbplyr::remote_con(dbm[]),
        auto_disconnect = TRUE
      )

      build_call = paste0(
        'dbm[] |> ',
        'dplyr::inner_join(vect_tbl, by = \'i\') |> ',
        'dplyr::mutate(x = `',
        generic_char,
        ordered_args,
        ' |> ',
        'dplyr::select(i, j, x)'
      )
    } else if(all(names(num_vect) %in% c_names)){
      vect_tbl = dplyr::tibble(
        j = match(names(num_vect), c_names),
        num_vect = unname(num_vect[match(names(num_vect),c_names)])
      )

      vect_tbl <- arrow::to_duckdb(
        .data = vect_tbl,
        con = dbplyr::remote_con(dbm[]),
        auto_disconnect = TRUE
      )

      build_call = paste0(
        'dbm[] |> ',
        'dplyr::inner_join(vect_tbl, by = \'j\') |> ',
        'dplyr::mutate(x = `',
        generic_char,
        ordered_args,
        ' |> ',
        'dplyr::select(i, j, x)'
      )
    } else{
      stop('Names of num_vect must be in either row or column names of dbMatrix')
    }
  }

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
#' Row (column) sums for dbMatrix objects
#' @inherit MatrixGenerics::rowSums description
#' @inheritParams MatrixGenerics::rowSums
#' @param na.rm Always TRUE for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param dims Always 1 for dbMatrix queries. Included for compatibility with
#' the generic.
#' @param memory logical. If FALSE (default), results returned as dbDenseMatrix. This is recommended
#' for large computations. Set to TRUE to return the results as a vector.
#' @concept summary
#' @rdname row_col_sums
#' @export
setMethod('rowSums', signature(x = 'dbDenseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # calculate rowSums
            rowSum <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE))

            if (memory) {
              res <- rowSum |>
              dplyr::collapse() |>
                dplyr::arrange(i) |>
              dplyr::pull(sum_x)

              names(res) <- rownames(x)
            } else {
              res <- new("dbDenseMatrix")
              rowSum <- rowSum |>
                dplyr::mutate(j = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::arrange(i) |>
                dplyr::select(i, j, x)
              res@value <- rowSum
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(nrow(x), 1L)
              res@dim_names <- list(rownames(x), c('col1'))
            }

            return(res)
          }
        )

## rowSums dbsm ####
#' @concept summary
#' @rdname row_col_sums
#' @export
setMethod('rowSums', signature(x = 'dbSparseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # calc rowsum for nonzero values in ijx
            rowSum <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE))

            if (memory) {
              rowSum <- rowSum |>
              dplyr::arrange(i) |>
              dplyr::pull(sum_x)

            # get row_idx for non-zero values in ijx
              nonzero_row_indices <- x[] |>
              dplyr::distinct(i) |>
              dplyr::arrange(i) |>
              dplyr::pull(i)

            # format data for join operation
              nonzero_rownames <- rownames(x)[nonzero_row_indices]
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
            } else {
              res <- new("dbDenseMatrix")
              rowSum <- rowSum |>
                dplyr::mutate(j = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::select(i, j, x) |>
                dplyr::collapse() |>
                dplyr::arrange(i)
              res@value <- rowSum
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(nrow(x), 1L)
              res@dim_names <- list(rownames(x), c('col1'))
            }

            # show
            return(res)
          })

## colSums dbdm####
#' @rdname row_col_sums
#' @export
setMethod('colSums', signature(x = 'dbDenseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # calculate colSums
            colSum <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE))

            if (memory) {
              res <- colSum |>
              dplyr::collapse() |>
                dplyr::arrange(j) |>
              dplyr::pull(sum_x)

              names(res) <- colnames(x)
            } else {
              res <- new("dbDenseMatrix")
              colSum <- colSum |>
                dplyr::mutate(i = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::arrange(j) |>
                dplyr::select(i, j, x)
              res@value <- colSum
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(1L, ncol(x))
              res@dim_names <- list(c('row1'), colnames(x))
            }

            return(res)
          })

## colSums dbsm ####
#' @concept summary
#' @rdname row_col_sums
#' @export
setMethod('colSums', signature(x = 'dbSparseMatrix'),
          function(x, ..., memory = FALSE){
            x = castNumeric(x)

            # calc colsum for nonzero values in ijx
            colSum = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE))

            if (memory) {
            colSum = colSum |>
              dplyr::arrange(j) |>
              dplyr::pull(j)
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
            } else {
              res <- new("dbDenseMatrix")
              colSum <- colSum |>
                dplyr::mutate(i = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::select(i, j, x) |>
                dplyr::collapse() |>
                dplyr::arrange(j)
              res@value <- colSum
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(1L, ncol(x))
              res@dim_names <- list(c('row1'), colnames(x))
            }

            # show
            return(res)
          })

## rowMeans dbdm ####
#' Row (column) means for dbMatrix objects
#' @inheritParams MatrixGenerics::rowMeans
#' @inherit MatrixGenerics::rowMeans description
#' @param na.rm Always TRUE for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param dims Always 1 for dbMatrix queries. Included for compatibility with
#' the generic.
#' @param memory logical. If FALSE (default), results returned as dbDenseMatrix. This is recommended
#' for large computations. Set to TRUE to return the results as a vector.
#' @concept summary
#' @rdname row_col_means
#' @export
setMethod('rowMeans', signature(x = 'dbDenseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # Calculate row means
            rowMean <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE))

            if (memory) {
              res <- rowMean |>
              dplyr::collapse() |>
                dplyr::arrange(i) |>
              dplyr::pull(mean_x)
              names(res) <- rownames(x)
            } else {
              res <- new("dbDenseMatrix")
              rowMean <- rowMean |>
                dplyr::mutate(j = 1) |>
                dplyr::rename(x = mean_x) |>
                dplyr::select(i, j, x) |>
                dplyr::collapse() |>
                dplyr::arrange(i)
              res@value <- rowMean
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(nrow(x), 1L)
              res@dim_names <- list(rownames(x), c('col1'))
            }
            return(res)
          })

## rowMeans dbsm ####
#' @rdname row_col_means
#' @export
setMethod('rowMeans', signature(x = 'dbSparseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            if (memory) {
              row_indices <- x[] |>
              dplyr::distinct(i) |>
              dplyr::arrange(i) |>
              dplyr::pull(i) |>
              as.integer()

              # calculate rowMeans
              row_sums <- rowSums(x)
              n_cols <- ncol(x)
              res <- row_sums / n_cols
            } else {
              # calculate rowMeans
              res <- rowSums(x, memory = FALSE) # dbDenseMatrix
              n_cols <- ncol(x)
              res[] <- res[] |>
                dplyr::mutate(x := x / n_cols)
            }

            return(res)
          })

## colMeans dbdm####
#' @rdname row_col_means
#' @export
setMethod('colMeans', signature(x = 'dbDenseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # Calculate column means
            colMean <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(mean_x = mean(x, na.rm = TRUE))

            if (memory) {
              res <- colMean |>
              dplyr::collapse() |>
                dplyr::arrange(j) |>
              dplyr::pull(mean_x)
              names(res) = colnames(x)
            } else {
              res <- new("dbDenseMatrix")
              colMean <- colMean |>
                dplyr::mutate(i = 1) |>
                dplyr::rename(x = mean_x) |>
                dplyr::select(i, j, x) |>
                dplyr::collapse() |>
                dplyr::arrange(j)
              res@value <- colMean
              res@name <- NA_character_ # for lazy queries
              res@init <- TRUE
              res@dims <- c(1L, ncol(x))
              res@dim_names <- list(c('row1'), colnames(x))
            }
            return(res)
          })

## colMeans dbsm ####
#' @rdname row_col_means
#' @export
setMethod('colMeans', signature(x = 'dbSparseMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            if (memory) {
              col_indices <- x[] |>
              dplyr::distinct(j) |>
              dplyr::arrange(j) |>
              dplyr::pull(j) |>
              as.integer()

              # calculate colMeans
              col_sums <- colSums(x)
              n_rows <- nrow(x)
              res <- col_sums / n_rows
            } else {
              res <- colSums(x, memory = FALSE) # dbDenseMatrix
              n_rows <- nrow(x)
              res[] <- res[] |>
                dplyr::mutate(x := x / n_rows)
            }

            return(res)
          })

## colSds dbdm ####
#' Row (column) standard deviations for dbMatrix objects
#' @inheritParams MatrixGenerics::colSds
#' @inherit MatrixGenerics::colSds description
#' @param na.rm Always TRUE for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param rows,cols Always NULL for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param center Always NULL for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param useNames Always TRUE for dbMatrix queries. Included for compatibility
#' with the generic.
#' @param dim Always NULL for dbMatrix queries. Included for compatibility with
#' the generic.
#' @concept summary
#' @rdname sds
#' @export
setMethod('colSds', signature(x = 'dbDenseMatrix'),
          function(x, ...){
            x <- castNumeric(x)

            val_names <- colnames(x)
            vals <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
              dplyr::arrange(j) |>
              dplyr::collapse() |>
              dplyr::pull(sd_x)
            names(vals) <- val_names
            vals
          })

## colSds dbsm ####
#' @concept summary
#' @rdname sds
#' @export
setMethod('colSds', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x <- castNumeric(x)

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
#' @concept summary
#' @rdname sds
#' @export
setMethod('rowSds', signature(x = 'dbDenseMatrix'),
          function(x, ...){
            x <- castNumeric(x)

            val_names <- rownames(x)
            vals <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sd_x = sd(x, na.rm = TRUE)) |>
              dplyr::arrange(i) |>
              dplyr::collapse() |>
              dplyr::pull(sd_x)
            names(vals) <- val_names
            vals
          })

## rowSds dbsm ####
#' @concept summary
#' @rdname sds
#' @export
setMethod('rowSds', signature(x = 'dbSparseMatrix'),
          function(x, ...){
            x <- castNumeric(x)

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

#' Arithmetic Mean for dbMatrix objects
#' @inheritParams base::mean
#' @inherit base::mean description
#' @param x dbMatrix object
#' @concept summary
#' @rdname mean
#' @export
setMethod('mean', signature(x = 'dbDenseMatrix'), function(x, ...) {
  x <- castNumeric(x)

  res <- x[] |>
    dplyr::summarise(mean_x = mean(x, na.rm = TRUE)) |>
    dplyr::pull(mean_x)

  return(res)

})

## mean dbsm####
#' @concept summary
#' @rdname mean
#' @export
setMethod('mean', signature(x = 'dbSparseMatrix'), function(x, ...) {
  x <- castNumeric(x)

  dim <- dim(x)
  n <- dim[1] * dim[2]

  res <- x[] |>
    dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
    dplyr::pull(sum_x)

  res <- res / n

  return(res)

})

## log ####

#' Logarithms and Exponentials
#' @inheritParams base::log
#' @inherit base::log description
#' @concept transform
#' @export
setMethod('log', signature(x = 'dbMatrix'), function(x, base = exp(1)) {
  x <- castNumeric(x)

  x[] <- x[] |>
    dplyr::mutate(x := log(x, base))

  return(x)

})

## sum ####

#' Sum of Vector Elements in dbMatrix objects
#' @inherit base::sum description
#' @param x dbMatrix object
#' @param na.rm Always TRUE for dbMatrix queries. Included for compatibility
#' with the generic.
#' @concept summary
#' @export
setMethod('sum', signature(x = 'dbMatrix'), function(x, na.rm = TRUE) {
  x <- castNumeric(x)

  res <- x[] |>
    dplyr::summarize(sum = sum(x, na.rm = TRUE)) |>
    dplyr::pull(sum)

  return(res)

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
