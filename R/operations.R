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

  # helper functions
  .skip_computation <- function(num_vect, generic_char) {
    if ((generic_char %in% c('*', '/') && all(num_vect == 1))) {
      return(TRUE)
    }

    if ((generic_char %in% c('+', '-') && all(num_vect == 0))) {
      return(TRUE)
    }

    return(FALSE)
  }

  .do_vect_multi <- function(num_vect, generic_char, dbm){
    if (length(num_vect) > 1) {
      return(TRUE)
    }

    if (is(dbm, 'dbDenseMatrix')) {
      return(FALSE)
    }

    if (is.infinite(num_vect) && generic_char %in% c('*')) {
      return(TRUE)
    }

    if (num_vect != 0 && generic_char %in% c('-', '+')) {
      return(TRUE)
    }

    if (num_vect == 0 && generic_char %in% c('/')) {
      return(TRUE)
    }

    return(FALSE)
  }

  .is_additive_ops <- function(num_vect){
    if (num_vect == "+" || num_vect == "-") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  .do_densification <- function(num_vect, generic_char, dbm){
    if (is(dbm, "dbSparseMatrix") && length(num_vect) == 1 &&
        .is_additive_ops(generic_char)) {
      return(TRUE)
    }

    if (is(dbm, "dbSparseMatrix") && length(num_vect) == 1 &&
        (generic_char == "^" || generic_char == "%%") && num_vect == 0) {
      return(TRUE)
    }

    return(FALSE)
  }

  # Main function
  if (all(is.nan(num_vect))) {
    if (is(dbm, 'dbSparseMatrix')) {
      dbm <- .to_db_dense(x = dbm)
    }

    dbm[] <- dbm[] |> dplyr::mutate(x = 0.0 / 0.0) # generate NaNs

    return(dbm)
  }

  if (.skip_computation(num_vect, generic_char)) {
    return(dbm)
  }

  if (.do_densification(num_vect, generic_char, dbm)) {
    dbm <- .to_db_dense(x = dbm)
  }

  if (.do_vect_multi(num_vect, generic_char, dbm)) {
    return(arith_call_dbm_vect_multi(dbm, num_vect, generic_char, ordered_args))
  }

  if (generic_char == "^" && num_vect == Inf) { # edge case 1
    build_call <- paste0(
      "dbm[] |> ",
      "dplyr::mutate(",
      "x = dplyr::case_when(",
      "x < 0 ~ 0/0, ", # workaround to generate NaNs (0/0)
      "TRUE ~ `", generic_char, ordered_args,
      ") "
    )
  } else if (generic_char == "%/%" && num_vect == 0) { # edge case 2
    dbm <- .to_db_dense(x = dbm)
    dbm[] <- dbm[] |>
      dplyr::mutate(
        x = dplyr::case_when(
          x < 0 ~ -Inf,
          x > 0 ~ Inf,
          x == 0 ~ 0 / 0,
          TRUE ~ x
        )
      )
    return(dbm)
  } else if (generic_char == "%/%" && num_vect == Inf) { # edge case 3
    dbm[] <- dbm[] |>
      dplyr::mutate(
        x = dplyr::case_when(
          x < 0 ~ -1,
          x > 0 ~ 0,
          TRUE ~ x
        )
      )
    return(dbm)
  } else if (generic_char == "%%" && num_vect == Inf) { # edge case 4
    dbm[] <- dbm[] |>
      dplyr::mutate(x = dplyr::case_when(x < 0 ~ Inf, TRUE ~ x ))

    return(dbm)
  } else { # general case
    build_call <- paste0('dbm[] |> dplyr::mutate(x = `',
                         generic_char, ordered_args)

  }

  dbm[] <- eval(str2lang(build_call))
  return(dbm)
}

#' @noRd
arith_call_dbm_vect_multi = function(dbm, num_vect, generic_char, ordered_args) {
  # get inputs
  con <- dbplyr::remote_con(dbm[])

  # handle order
  if (ordered_args == '`(x, num_vect))') {
    swap_arith_order <- FALSE
  } else {
    swap_arith_order <- TRUE
  }

  # Create dbDenseMatrix with dbVector
  dbv <- .as_dbVector(vector = num_vect, con = con)
  dbm <- .join_dbm_vector(dbm = dbm, dbVector = dbv, op = generic_char,
                          swap_arith_order = swap_arith_order)
  return(dbm)
}
#' @keywords internal
#' @noRd
.as_dbVector <- function(vector, con) {
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
## %in% operator ####
#' @title Value Matching
#'
#' @description
#' Implements the `%in%` operator for dbMatrix objects. This operator checks if
#' elements from the left operand are contained in the right operand, returning
#' a logical vector.
#'
#' @param x A dbMatrix object or any other object
#' @param table Any object or a dbMatrix object
#'
#' @details
#' This is a method for the standard `%in%` operator for dbMatrix objects.
#' It follows R's standard behavior for the `%in%` operator:
#'
#' - When `x` is a dbDenseMatrix, it returns a logical vector with the same length as the
#'   total number of elements in the matrix.
#' - When `table` is a dbDenseMatrix, it allows checking if elements in `x` are in the matrix.
#' - For dbSparseMatrix objects, it throws an error to match the behavior of dgCMatrix.
#'
#' @return
#' A logical vector of the same length as `x`, indicating which elements of `x` are in `table`.
#'
#' @examples
#' \dontrun{
#' # Create a dbMatrix
#' mat <- matrix(1:9, nrow = 3, ncol = 3)
#' dbmat <- as.dbMatrix(mat)
#'
#' # Check if elements in dbMatrix are in a vector
#' result <- dbmat %in% c(1, 3, 5, 7, 9)
#'
#' # Check if elements in a vector are in dbMatrix
#' result <- c(1, 3, 5, 7, 9) %in% dbmat
#' }
#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "dbDenseMatrix", table = "ANY"), function(x, table) {
  if (length(table) == 0) {
    return(rep(FALSE, length(x)))
  }

  db_vector <- x[] |>
    dplyr::arrange(j, i) |>
    dplyr::pull(x)

  return(db_vector %in% table)
})



#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "ANY", table = "dbDenseMatrix"), function(x, table) {
  if (length(x) == 0) {
    return(logical(0))
  }

  table_data <- table[] |>
    dplyr::arrange(j, i) |>
    dplyr::pull(x)

  result <- x %in% table_data
  return(result)
})

#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "numeric", table = "dbSparseMatrix"), function(x, table) {
  if (length(x) == 0) {
    return(logical(0))
  }

  # Convert dbSparseMatrix to vector and check membership
  table_data <- table[] |>
    dplyr::arrange(j, i) |>
    dplyr::pull(x)

  result <- x %in% table_data
  return(result)
})

#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "numeric", table = "dbDenseMatrix"), function(x, table) {
  if (length(x) == 0) {
    return(logical(0))
  }

  # Convert dbDenseMatrix to vector and check membership
  table_data <- table[] |>
    dplyr::arrange(j, i) |>
    dplyr::pull(x)

  result <- x %in% table_data
  return(result)
})

#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "ANY", table = "dbSparseMatrix"), function(x, table) {
  if (length(x) == 0) {
    return(logical(0))
  }

  # Convert dbSparseMatrix to vector and check membership
  table_data <- table[] |>
    dplyr::arrange(j, i) |>
    dplyr::pull(x)

  result <- x %in% table_data
  return(result)
})

#' @rdname percent-in
#' @concept transform
#' @export
setMethod("%in%", signature(x = "dbSparseMatrix", table = "ANY"), function(x, table) {
  stop("'match' requires vector arguments")
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

# MatrixGenerics ####
## rowVars dbdm ####
#' Row (column) variances for [`dbMatrix`] objects
#' @inherit MatrixGenerics::rowVars description
#' @param x A [`dbMatrix`] object.
#' @param rows Always NULL for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param cols Always NULL for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param na.rm Always TRUE for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param center Always NULL for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param ... Additional arguments (not used, but included for compatibility with the generic).
#' @param useNames Always TRUE for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param memory logical. If FALSE (default), results returned as dbDenseMatrix. This is recommended
#' for large computations. Set to TRUE to return the results as a vector.
#' @concept summary
#' @rdname row_col_vars
#' @export
setMethod("rowVars", signature(x = "dbDenseMatrix"),
          function(x,
                   rows = NULL,
                   cols = NULL,
                   na.rm = TRUE,
                   center = NULL,
                   ...,
                   memory = FALSE,
                   useNames = TRUE) {

            x <- castNumeric(x)
            k <- ncol(x)

            row_stats <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(
                sum_x  = sum(x, na.rm = na.rm),
                sum_x2 = sum(x * x, na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                var_x = (sum_x2 - (sum_x * sum_x) / !!k) / (!!k - 1)
              ) |>
              dplyr::arrange(i)

            if (memory) {
              v <- dplyr::pull(row_stats, var_x)
              if (useNames) names(v) <- rownames(x)
              return(v)
            }

            rowVar <- row_stats |>
              dplyr::transmute(i, j = 1L, x = var_x)

            res <- new(
              Class = "dbDenseMatrix",
              value = rowVar,
              name = NA_character_,
              init = TRUE,
              dims = c(nrow(x), 1L),
              dim_names = list(rownames(x), "col1")
            )

            return(res)
          })

## rowVars dbsm ####
#' @concept summary
#' @rdname row_col_vars
#' @export
setMethod('rowVars', signature(x = 'dbSparseMatrix'),
          function(x, rows = NULL, cols = NULL, na.rm = TRUE, center = NULL,
                   ..., memory = FALSE, useNames = TRUE) {
            x <- castNumeric(x)
            k <- ncol(x)

            row_stats <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(
                sum_x  = sum(x, na.rm = na.rm),
                sum_x2 = sum(x * x , na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                var_x = dplyr::case_when(
                  k <= 1 ~ NA_real_,
                  TRUE ~ (sum_x2 - (sum_x * sum_x) / k) / (k - 1)
                )
              ) |>
              dplyr::arrange(i)

            if (memory) {
              # Return as named vector
              res <- row_stats |>
                dplyr::pull(var_x)

              if (useNames) names(res) <- rownames(x)
              return(res)
            } else {
              # Return as dbDenseMatrix
              rowVar <- row_stats |>
                dplyr::mutate(j = 1, x = var_x) |>
                dplyr::select(i, j, x)

              res <- new(
                Class = "dbDenseMatrix",
                value = rowVar,
                name = NA_character_,
                init = TRUE,
                dims = c(nrow(x), 1L),
                dim_names = list(rownames(x), c('col1'))
              )

              return(res)
            }
          })

## colVars dbdm ####
#' @rdname row_col_vars
#' @export
setMethod("colVars", signature(x = "dbDenseMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = TRUE, center = NULL,
                   ..., memory = FALSE, useNames = TRUE) {

            x <- castNumeric(x)
            m <- nrow(x)                       # total rows

            col_stats <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(
                sum_x  = sum(x,        na.rm = na.rm),
                sum_x2 = sum(x * x,    na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                var_x = (sum_x2 - (sum_x * sum_x) / !!m) / (!!m - 1)
              ) |>
              dplyr::arrange(j)

            if (memory) {
              v <- dplyr::pull(col_stats, var_x)
              if (useNames) names(v) <- colnames(x)
              return(v)
            }

            colVars <- col_stats |>
              dplyr::transmute(i = j, j = 1L, x = var_x)

            res <-  new(
              "dbDenseMatrix",
              value = colVars,
              name = NA_character_,
              init = TRUE,
              dims = c(ncol(x), 1L),
              dim_names = list(colnames(x), "col1")
            )

            return(res)
          })

## colVars dbsm ####
#' @concept summary
#' @rdname row_col_vars
#' @export
setMethod("colVars", signature(x = "dbSparseMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = TRUE, center = NULL,
                   ..., memory = FALSE, useNames = TRUE) {
            x <- castNumeric(x)
            m <- nrow(x)

            col_stats <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(
                sum_x  = sum(x, na.rm = na.rm),
                sum_x2 = sum(x * x, na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                var_x = (sum_x2 - (sum_x * sum_x) / !!m) / (!!m - 1)
              ) |>
              dplyr::arrange(j)

            if (memory) {
              v <- dplyr::pull(col_stats, var_x)
              if (useNames) names(v) <- colnames(x)
              return(v)
            }

            colVars <- col_stats |>
              dplyr::transmute(i = j, j = 1L, x = var_x)

            res <-  new("dbDenseMatrix",
                        value = colVars,
                        name = NA_character_,
                        init = TRUE,
                        dims = c(ncol(x), 1L),
                        dim_names = list(colnames(x), "col1"))

            return(res)
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

# Math ####
## abs ####
## sign ####
## sqrt ####
## ceiling ####
## floor ####
## trunc ####
## cummax ####
## cummin ####
## cumprod ####
## cumsum ####
## log ####
## log10 ####
## log2 ####
## log1p ####
## acos ####
## acosh ####
## asin ####
## asinh ####
## atan ####
## atanh ####
## exp ####
## expm1 ####
## cos ####
## cosh ####
## cospi ####
## sin ####
## sinh ####
## sinpi ####
## tan ####
## tanh ####
## tanpi ####
## gamma ####
## lgamma ####
## digamma ####
## trigamma ####
#' Math Operations for [`dbMatrix`] Objects
#'
#' @description
#' Implements the `Math` [`S4groupGeneric`] functions
#' for [`dbMatrix`] objects. This includes various mathematical operations such as
#' logarithms, exponentials, trigonometric functions, and other transformations.
#'
#' @param x A [`dbMatrix`] object.
#'
#' @details
#' This method provides implementations for the following Math functions:
#'
#' *Arithmetic and rounding*:
#' * `abs()`, `sign()`, `sqrt()`, `ceiling()`, `floor()`, `trunc()`
#'
#' *Cumulative operations*:
#' * `cummax()`, `cummin()`, `cumprod()`, `cumsum()`
#' * **Note: `cumprod()` is not supported**
#'
#' *Logarithmic*:
#' * `log()`, `log10()`, `log2()`, `log1p()`
#' * **Note: `log1p()` is not supported**
#'
#' *Trigonometric*:
#' * `cos()`, `sin()`, `tan()`, `acos()`, `asin()`, `atan()`
#' * `cosh()`, `sinh()`, `tanh()`, `acosh()`, `asinh()`, `atanh()`
#' * `cospi()`, `sinpi()`, `tanpi()`
#' * **Note: `acosh()` `asinh()` `atanh()` are not supported**
#'
#' *Exponential*:
#' * `exp()`, `expm1()`
#' * **Note: `expm1()` is not supported**
#'
#' *Special functions*:
#' * `gamma()`, `lgamma()`, `digamma()`, `trigamma()`
#' * **Note: `digamma()` `trigamma()` are not supported**
#'
#' The function applies the specified mathematical operation to each element
#' of the [`dbMatrix`] object.
#'
#' @return
#' A [`dbMatrix`] object with the mathematical operation applied to each element.
#'
#' @examples
#' mat <- matrix(1, nrow = 3, ncol = 3)
#' dbmat <- as.dbMatrix(mat)
#' log(dbmat)
#' sqrt(dbmat)
#' sin(dbmat)
#'
#' @concept transform
#' @export
setMethod('Math', signature(x = 'dbMatrix'), function(x) {
  build_call = glue::glue(
    "x[] |>
     dplyr::mutate(x = `", as.character(.Generic),"`(x))"
  )

  x[] <- eval(str2lang(build_call))
  x@name <- NA_character_
  return(x)
})

# Summary ####
## max ####
## min ####
## range ####
## prod ####
## sum ####
## any ####
## all ####
#' Summary Methods for [`dbMatrix`] Objects
#'
#' @description
#' Implements the [`S4groupGeneric`] group generic functions for dbMatrix objects.
#'
#' @param x A dbMatrix object.
#' @param ... Additional arguments (not used, but included for compatibility with the generic).
#' @param na.rm Logical. If TRUE, remove NA values before computation. Always set to TRUE for this implementation.
#' @concept summary
#' @details
#' This method provides implementations for the following [`S4groupGeneric`] functions:
#' * `max()`: Maximum value
#' * `min()`: Minimum value
#' * `range()`: *Not supported*
#' * `prod()`: Product of all values
#' * `sum()`: Sum of all values
#' * `any()`: Returns TRUE if any value is TRUE
#' * `all()`: Returns TRUE if all values are TRUE
#'
#'
#' @return
#' The result of applying the respective summary function to the dbMatrix object.
#' The type of the return value depends on the specific function called.
#'
#' @examples
#' mat <- matrix(1, nrow = 3, ncol = 3)
#' dbmat <- as.dbMatrix(mat)
#' max(dbmat)
#' min(dbmat)
#' prod(dbmat)
#' sum(dbmat)
#' any(dbmat > 0)
#' all(dbmat > 0)
#'
#' @export
setMethod('Summary', signature(x = 'dbMatrix'), function(x, ..., na.rm = TRUE) {
  # Check to see if 'range' or 'all' is equal to as.character(.Generic)
  if (as.character(.Generic) == 'range' ) {
    stopf(paste0("range() is not yet supported for dbMatrix objects."))
  }

  if (as.character(.Generic) == 'any' | as.character(.Generic) == 'all' ) {
    x_class <- x[] |> head(n=1) |> dplyr::pull(x) |> class()
    if(x_class != 'logical') {
      x[] <- x[] |> dplyr::mutate(x = as.logical(x))
    }
    warning("coercing argument of type 'double' to logical")
  }

  build_call = glue::glue(
    "x[] |>
     dplyr::summarise(res := `", as.character(.Generic),"`(x, na.rm = TRUE)) |>
     dplyr::pull(res)"
  )
  res <- suppressWarnings(eval(str2lang(build_call)))

  # avoid using dplyr::pull()
  # build_call = glue::glue(
  #   "x[] |>
  #    dplyr::summarise(res := `", as.character(.Generic),"`(x, na.rm = TRUE)) |>
  #    dbplyr::sql_render()"
  # )
  # sql <- suppressWarnings(eval(str2lang(build_call)))
  # res <- DBI::dbGetQuery(dbplyr::remote_con(x[]), sql)[[1]] # get value

  if(is.na(res)){ #FIXME
    res <- TRUE
  }

  return(res)
})

# General Ops ####

### t ####

#' Matrix Transpose
#' @description Given a [`dbMatrix`] `x`, `t` returns the transpose of `x`.
#' @param x [`dbMatrix`] object
#' @importFrom Matrix t
#' @return [`dbMatrix`] object
#' @concept transform
#' @export
#' @exportMethod t
#' @rdname t-dbMatrix
setMethod('t', signature(x = 'dbMatrix'), function(x) {
  x[] <- x[] |> dplyr::select(i = j, j = i, x)
  x@dims <- c(x@dims[[2L]], x@dims[[1L]])
  x@dim_names <- list(x@dim_names[[2L]], x@dim_names[[1L]])
  return(x)
})

### nrow ####

#' The Number of Rows/Columns of a dbMatrix Object
#' @description nrow and ncol return the number of rows or columns present in x.
#' @param x dbMatrix object
#' @concept matrix_props
#' @rdname nrow_ncol
#' @export
setMethod('nrow', signature(x = 'dbMatrix'), function(x) {
  if (is.na(x@dims[1L])) {
    conn <- get_con(x)
    res <- DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT i from %s',
                                               dbplyr::remote_name(x[])))
  } else {
    return(x@dims[1L])
  }

  return(base::nrow(res))
})

### ncol ####

#' The Number of Rows/Columns of an Array 2
#' @concept matrix_props
#' @rdname nrow_ncol
#' @export
setMethod('ncol', signature(x = 'dbMatrix'), function(x) {
  if (is.na(x@dims[2L])) {
    conn <- get_con(x)
    res <- DBI::dbGetQuery(conn = conn, sprintf('SELECT DISTINCT j from %s',
                                               dbplyr::remote_name(x[])))
  } else {
    return(x@dims[2L])
  }
  return(base::nrow(res))
})

### dim ####

#' Dimensions of an Object
#' @description Retrieve the dimension of an object.
#' @param x dbMatrix object
#' @concept matrix_props
#' @export
setMethod('dim', signature(x = 'dbMatrix'), function(x) {
  if (any(is.na(x@dims))) {
    return(c(nrow(x), ncol(x)))
  } else {
    res <- x@dims
  }
})

### head ####
#' Return the First or Last Parts of an Object
#' @inherit utils::head description
#' @inheritParams utils::head
#' @concept matrix_props
#' @rdname head_tail
#' @export
setMethod('head', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset <- 1:n
  x[] <- x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] <- min(x@dims[1L], as.integer(n))
  return(x)
})

### tail ####
#' @concept matrix_props
#' @rdname head_tail
#' @export
setMethod('tail', signature(x = 'dbMatrix'), function(x, n = 6L, ...) {
  n_subset <- (x@dims[1L] - n):x@dims[1L]
  x[] <- x[] |> dplyr::filter(i %in% n_subset)
  x@dims[1L] <- min(x@dims[1L], as.integer(n))
  return(x)
})

### length ####
# TODO: add value param once setter is created
#' Length of a [`dbMatrix`] Object
#' @concept matrix_props
#' @description Get or set the length of vectors (including lists) and factors,
#' and of any other R object for which a method has been defined.
#' @param x [`dbMatrix`] object
#' @rdname length
#' @export
setMethod('length', signature(x = 'dbMatrix'), function(x) {
  #FIXME modify for dbVector class
  if(!1 %in% dim(x)){ # dbMatrix
    res <-  prod(dim(x))
    return(res)
  }
  res <- nrow(x)
  return(res)
})

# Column data types ####

## castNumeric ####

#' @title Set a column to numeric
#' @description
#' Sets a column to numeric type if not already
#' This precaution is to avoid truncation of values
#' @param x dbData object
#' @param col column to cast to numeric
#' @param ... additional params to pass
#' @noRd
#' @keywords internal
setMethod('castNumeric', signature(x = 'dbMatrix', col = 'character'),
          function(x, col, ...) {
            sym_col <- dplyr::sym(col)
            x[] <- x[] |> dplyr::mutate(!!sym_col := as.numeric(!!sym_col))
            return(x)
          })

#' @noRd
#' @export
setMethod('castNumeric',
          signature(x = 'dbMatrix', col = 'missing'),
          function(x, ...) {
            x[] <- x[] |> dplyr::mutate(x := as.numeric(x))
            return(x)
          })
