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
  ijx <- dplyr::tibble(i = 1:length(vector), j = 1, x = vector) |>
    dplyr::copy_to(
      dest = con,
      name = unique_table_name('dbVector'),
      temporary = TRUE,
      overwrite = TRUE
    )

  res <- new(Class = "dbDenseMatrix",
             value = ijx,
             name = NA_character_,
             init = TRUE,
             dims = c(length(vector), 1L),
             dim_names = list(paste0('row', 1:length(vector)), c('col1')))

  return(res)
}

#' @title Evaluate if a dbSparseMatrix should be densified
#' @param generic_char A character string representing the operation to be performed.
#' @param dbVector A \code{dbMatrix} object with 1D row or col.
#' @details
#' Evaluates if a \code{dbSparseMatrix} should be
#' densified for `[Arith]` operations and specific scalar values for
#' operations in the order of dbSparseMatrix, vector
#'
#'
#' @keywords internal
.eval_op_densify <- function(generic_char, dbVector){
  if (generic_char == "+" || generic_char == "-"){
    if(suppressWarnings(all(dbVector == 0))){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if (generic_char == "*"){
    if(suppressWarnings(any(dbVector == Inf)) ||
       suppressWarnings(any(dbVector == NaN))){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (generic_char == "/" || generic_char == "%%" ||
             generic_char == "%/%" || generic_char == "^"){
    if(suppressWarnings(any(dbVector == 0)) ||
       suppressWarnings(any(dbVector == NaN))){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    stopf("Invalid operator: %s", generic_char)
  }
}

#' @title Join a \code{dbSparseMatrix} with a \code{dbMatrix} object
#' @param dbm A \code{dbSparseMatrix} object.
#' @param dbVector A \code{dbMatrix} object with 1D row or col.
#' @param generic_char A character string representing the operation to be performed.
#' @param swap_arith_order order of the arguments for the operation. default: NULL
#' @keywords internal
.join_dbm_vector <- function(dbm, dbVector, op, swap_arith_order = FALSE) {
  # check inputs
  con <- dbplyr::remote_con(dbVector[])
  n_rows <- bit64::as.integer64.integer(dim(dbm)[1])
  n_cols <- bit64::as.integer64.integer(dim(dbm)[2])
  length <- bit64::as.integer64.integer(length(dbVector))
  total_dims <- n_rows * n_cols

  # validate dimensions
  rep <- ceiling(total_dims / length) #FIXME: R cannot do math with int64
  if (is.na(rep)) {
    stopf("total_dims must be a positive integer")
  }

  # check for length mismatch
  remainder <- total_dims %% length
  if (remainder > 0) {
    cli::cli_alert_warning(wrap = FALSE,
    "Longer object length [{total_dims}] is not a multiple of shorter
     object length [{length}]"
    )
  }

  # helper functions
  .perform_dense_join <- function(dbm, dbVector, op) {
    # get precomputed matrix
    precomp <- .initialize_precompute_matrix(con, n_rows, n_cols)
    name_precomp <- dbplyr::remote_name(precomp)
    if(is.na(name_precomp)) stopf("precompute error")

    # add idx to dbm
    dbm[] <- dbm[] |>
      dplyr::mutate(idx = (j - 1) * n_rows + (i - 1))

    # assign names to lazy tables in db
    dbvector_name <- .assign_dbm_name(dbm = dbVector)
    dbm_name <- .assign_dbm_name(dbm = dbm)

    # set order for arith
    arith_expr <- if (swap_arith_order) {
      paste0("v.x ", op, " ", dbm_name, ".x")
    } else {
      paste0(dbm_name, ".x ", op, " v.x")
    }

    sql <- glue::glue("
    CREATE TEMPORARY VIEW {name_ijx} AS
    WITH base_data AS (
      SELECT p.i, p.j, {arith_expr} AS x
      FROM (
        SELECT *,
        (rowid % {length}) + 1 as v_idx
        FROM {name_precomp}
      ) p
      LEFT JOIN {dbvector_name} v
        ON p.v_idx = v.i
      INNER JOIN {dbm_name}
        ON p.idx = {dbm_name}.idx
    )
    SELECT i,j, x
    FROM base_data;
    ")

    invisible(DBI::dbExecute(con, sql))

    return(dplyr::tbl(con, name_ijx))
  }

  .perform_sparse_join <- function(dbm, dbVector, op) {
    # assign names to lazy tables in db
    dbvector_name <- .assign_dbm_name(dbm = dbVector)
    dbm_name <- .assign_dbm_name(dbm = dbm)

    # set order for arith
    arith_expr <- if (swap_arith_order) {
      paste("v.x", op, dbm_name, ".x")
    } else {
      paste(dbm_name, ".x", op, "v.x")
    }

    # join dbm, dbvector
    sql <- glue::glue("
      CREATE OR REPLACE TEMPORARY VIEW {name_ijx} AS
      SELECT
          {dbm_name}.i,
          {dbm_name}.j,
          {arith_expr} as x  -- perform arith operation
      FROM {dbm_name}
      LEFT JOIN {dbvector_name} v
          ON {dbm_name}.i = v.i
    ")
    invisible(DBI::dbExecute(con, sql))
    return(dplyr::tbl(con, name_ijx))
  }

  # main function
  name_ijx <- unique_table_name('tmp_ijx')
  if (is(dbm, "dbSparseMatrix")) {
    if (.eval_op_densify(generic_char = op, dbVector = dbVector)) {
      dense_dbm <- .to_db_dense(dbm)
      dense_dbm[] <- .perform_dense_join(dense_dbm, dbVector, op)
      return(dense_dbm)
    } else {
      if (nrow(dbm) == length(dbVector)) {
        dbm[] <- .perform_sparse_join(dbm, dbVector, op)
        return(dbm)
      } else {
        dbm[] <- .perform_dense_join(dbm, dbVector, op)
        return(dbm)
      }
    }
  } else { # dbDenseMatrix
    if (nrow(dbm) == length(dbVector)) {
      dbm[] <- .perform_sparse_join(dbm, dbVector, op)
      return(dbm)
    } else {
      dbm[] <- .perform_dense_join(dbm, dbVector, op)
      return(dbm)
    }
  }
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
  dbm = castNumeric(e1)

  num_vect = if(typeof(e2) != 'double'){
    as.numeric(e2)
  } else{
    e2
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
  dbm = castNumeric(e2)

  num_vect = if (typeof(e1) != 'double'){
    as.numeric(e1)
  } else{
    e1
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
  generic_char = as.character(.Generic)
  dim1 <- dim(e1)
  dim2 <- dim(e2)

  # Check for arith operations if neither is a dbVector
  if (!1 %in% c(dim1, dim2)) {
    if (generic_char %in% c('/', '^', '%%', '%/%')) {
      stopf("Arith operations with '/', '^', '%%', '%/%' are not yet supported
            between dbMatrix objects.")
    }
  }

  # Case 1: dbVector, dbVector
  if (1 %in% dim2 & 1 %in% dim1) {
    if (!all(dim1 == dim2)) {
      stopf("dbVector-dbVector recycling not yet supported.")
    }
  }

  # Case 2: dbMatrix, dbVector
  else if (1 %in% dim2) {
    res <- .join_dbm_vector(dbm = e1, dbVector = e2, op = generic_char)
    return(res)
  }
  # Case 3: dbVector, dbMatrix
  else if (1 %in% dim1) {
    res <- .join_dbm_vector(dbm = e2, dbVector = e1, op = generic_char,
                            swap_arith_order = TRUE)
    return(res)
  }
  # Case 4: Both inputs are dbMatrix
  else {
    if (!all(dim1 == dim2)) {
      stopf('non-conformable matrix dimensions')
    }

    # Perform full join operation on dbMatrix
    # TODO: avoid full join
    build_call = glue::glue(
      "e1[] |>
         dplyr::full_join(e2[], by = c('i', 'j')) |>
         dplyr::mutate(x = `{generic_char}`(dplyr::coalesce(x.x, 0),
                       dplyr::coalesce(x.y, 0))) |>
         dplyr::select(i, j, x) |>
         dplyr::arrange(i)"
    )
    e1[] = eval(str2lang(build_call))
    e1@name = NA_character_
    return(e1)
  }



})

## Ops: dbm_e2 ####
#' Ops dbMatrix, e2
#' @description
#' See ?\link{\code{methods::Ops}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Ops', signature(e1 = 'dbMatrix', e2 = 'ANY'), function(e1, e2){
  op <- as.character(.Generic)

  if(is.na(e2) && op == "==") {
    build_call = glue::glue('e1[] |> dplyr::mutate(x = is.na(x))')
  } else if(is.na(e2) && op == "!=") {
    build_call = glue::glue('e1[] |> dplyr::mutate(x = !is.na(x))')
  } else {
    build_call = glue::glue('e1[] |> dplyr::mutate(x = `', op , '`(x, e2))')
  }

  e1[] = eval(str2lang(build_call))
  e1@name = NA_character_
  return(e1)
})

## Ops: e1_dbm ####
#' Ops e1, dbMatrix
#' @description
#' See ?\link{\code{methods::Ops}} for more details.
#' @noRd
#' @rdname summary
#' @export
setMethod('Ops', signature(e1 = 'ANY', e2 = 'dbMatrix'), function(e1, e2){
  build_call = glue::glue(
    'e2[] |> dplyr::mutate(x = `',
    as.character(.Generic)
    ,
    '`(e1, x))'
  )

  e2[] = eval(str2lang(build_call))
  e1@name = NA_character_
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
  if (!any(e1@dims %in% e2@dims)){
    stopf('non-conformable matrix dimensions')
  }

  build_call = glue::glue("
    e1[] |>
    dplyr::left_join(e2[], by = c('i', 'j'), suffix = c('', '.y')) |>
    dplyr::mutate(x = `",
      as.character(.Generic),
      "`(x, x.y)) |>
    dplyr::select(c('i', 'j', 'x'))
    ")

  e1[] = eval(str2lang(build_call))
  e1@name = NA_character_
  e1
})

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
#' Row (column) sums for [`dbMatrix`] objects
#' @inherit MatrixGenerics::rowSums description
#' @inheritParams MatrixGenerics::rowSums
#' @param na.rm Always TRUE for [`dbMatrix`]. Included for compatibility
#' with the generic.
#' @param dims Always 1 for [`dbMatrix`] queries. Included for compatibility with
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
              rowSum <- rowSum |>
                dplyr::mutate(j = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::arrange(i) |>
                dplyr::select(i, j, x)

              res <- new(Class = "dbDenseMatrix",
                         value = rowSum,
                         name = NA_character_, # for lazy queries
                         init = TRUE,
                         dims = c(nrow(x), 1L),
                         dim_names = list(rownames(x), c('col1')))
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

            # add 0 to rowSum
            view_name <- unique_table_name('tmp_view')
            num_row <- nrow(x)
            sql <- glue::glue('
                CREATE OR REPLACE TEMPORARY TABLE {view_name} AS
                SELECT generate_series AS i
                FROM generate_series(1, {num_row});
            ')
            con <- dbplyr::remote_con(x[])
            invisible(DBI::dbExecute(con, sql))
            dim_tbl <- dplyr::tbl(con, view_name)

            # calc rowsum for nonzero values in ijx
            rowSum <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::right_join(dim_tbl, by = c('i'), copy = TRUE) |> # fill missing i values
              dplyr::mutate(x = dplyr::coalesce(sum_x, 0)) |>
              dplyr::mutate(j = 1) |>
              dplyr::select(i, j, x) |>
              dplyr::collapse() |>
              dplyr::arrange(i)

            if (memory) {
              res <- rowSum |>
                dplyr::pull(x)

              names(res) <- rownames(x)
            } else {
              res <- new(Class = "dbDenseMatrix",
                         value = rowSum,
                         name = NA_character_,
                         init = TRUE,
                         dims = c(nrow(x), 1L),
                         dim_names = list(rownames(x), c('col1')))
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
              colSum <- colSum |>
                dplyr::select(i = j, sum_x) |>
                dplyr::mutate(j = 1) |>
                dplyr::rename(x = sum_x) |>
                dplyr::arrange(i) |>
                dplyr::select(i, j, x)

              res <- new(Class = "dbDenseMatrix",
                         value = colSum,
                         name = NA_character_, # for lazy queries
                         init = TRUE,
                         dims = c(ncol(x), 1L),
                         dim_names = list(colnames(x), c('col1')))
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

            view_name <- unique_table_name('tmp_view')
            num_col <- ncol(x)
            sql <- glue::glue('
                CREATE OR REPLACE TEMPORARY TABLE {view_name} AS
                SELECT generate_series AS j
                FROM generate_series(1, {num_col});
            ')
            con <- dbplyr::remote_con(x[])
            invisible(DBI::dbExecute(con, sql))
            dim_tbl <- dplyr::tbl(con, view_name)

            # calc colsum for nonzero values in ijx
            colSum = x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(sum_x = sum(x, na.rm = TRUE)) |>
              dplyr::right_join(dim_tbl, by = c('j'), copy = TRUE) |>
              dplyr::mutate(x = dplyr::coalesce(sum_x, 0)) |>
              dplyr::select(i = j, x) |>
              dplyr::mutate(j = 1) |>
              dplyr::select(i, j, x) |>
              dplyr::collapse() |>
              dplyr::arrange(i)

            if (memory) {
              res <- colSum |>
                dplyr::pull(x)

              names(res) <- colnames(x)
            } else {
              res <- new(Class = "dbDenseMatrix",
                          value = colSum,
                          name = NA_character_,
                          init = TRUE,
                          dims = c(ncol(x), 1L),
                          dim_names = list(colnames(x), c('col1')))
            }

            # show
            return(res)
          })

## rowMeans dbm ####
#' Row (column) means for [`dbMatrix`] objects
#' @inheritParams MatrixGenerics::rowMeans
#' @inherit MatrixGenerics::rowMeans description
#' @param na.rm Always TRUE for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param dims Always 1 for [`dbMatrix`] queries. Included for compatibility with
#' the generic.
#' @param memory logical. If FALSE (default), results returned as dbDenseMatrix.
#' This is recommended for large computations. Set to TRUE to return the results
#' as a vector.
#' @concept summary
#' @rdname row_col_means
#' @export
setMethod('rowMeans', signature(x = 'dbMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            # calculate rowMeans
            rowSums <- rowSums(x, memory = FALSE) # dbDenseMatrix
            n_cols <- ncol(x)
            rowMeans <- rowSums
            rowMeans[] <- rowMeans[] |> dplyr::mutate(x := x / n_cols)
            if (memory) {
              rowMeans <- rowMeans[] |>
                dplyr::arrange(i) |>
                dplyr::pull(x)
              names(rowMeans) <- rownames(x)
            }

            return(rowMeans)
          })

## colMeans dbm ####
#' @rdname row_col_means
#' @export
setMethod('colMeans', signature(x = 'dbMatrix'),
          function(x, ..., memory = FALSE){
            x <- castNumeric(x)

            colSums <- colSums(x, memory = FALSE) # dbDenseMatrix
            n_rows <- nrow(x)
            colMeans <- colSums
            colMeans[] <- colMeans[] |> dplyr::mutate(x := x / n_rows)
            if (memory) {
              colMeans <- colMeans[] |>
                dplyr::arrange(i) |>
                dplyr::pull(x)
              names(colMeans) <- colnames(x)
            }

            return(colMeans)
          })

## colSds dbdm ####
#' Row (column) standard deviations for [`dbMatrix`] objects
#' @inherit MatrixGenerics::rowSds description
#' @param x A [`dbMatrix`] object.
#' @param rows Always NULL for [`dbMatrix`] queries. TODO
#' @param cols Always NULL for [`dbMatrix`] queries. TODO
#' @param na.rm Always TRUE for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param center Always NULL for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @param ... Additional arguments (not used, but included for compatibility with the generic).
#' @param memory logical. If FALSE (default), results returned as dbDenseMatrix.
#' @param useNames Always TRUE for [`dbMatrix`] queries. Included for compatibility
#' with the generic.
#' @concept summary
#' @rdname row_col_sds
#' @export
setMethod("colSds", signature(x = "dbDenseMatrix"),
          function(x, ..., memory = FALSE, useNames = TRUE) {

            x <- castNumeric(x)
            m <- nrow(x)

            col_stats <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(
                sum_x  = sum(x,      na.rm = TRUE),
                sum_x2 = sum(x * x , na.rm = TRUE),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                sd_x = if (m <= 1) NA_real_
                else sqrt((sum_x2 - (sum_x * sum_x) / !!m) / (!!m - 1))
              ) |>
              dplyr::arrange(j)

            if (memory) {
              res <- dplyr::pull(col_stats, sd_x)
              if (useNames) names(res) <- colnames(x)
              return(res)
            }

            colSd <- dplyr::transmute(col_stats, i = j, j = 1L, x = sd_x)

            res <- new("dbDenseMatrix",
                       value = colSd,
                       name = NA_character_,
                       init = TRUE,
                       dims = c(ncol(x), 1L),
                       dim_names = list(colnames(x), "col1"))
            return(res)
          })

## colSds dbsm ####
#' @concept summary
#' @rdname row_col_sds
#' @export
setMethod("colSds", signature(x = "dbSparseMatrix"),
          function(x, ..., memory = FALSE, useNames = TRUE) {

            x <- castNumeric(x)
            m <- nrow(x)

            col_stats <- x[] |>
              dplyr::group_by(j) |>
              dplyr::summarise(
                sum_x  = sum(x,      na.rm = TRUE),
                sum_x2 = sum(x * x , na.rm = TRUE),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                sd_x = if (m <= 1) NA_real_
                else sqrt((sum_x2 - (sum_x * sum_x) / m) / (m - 1))
              ) |>
              dplyr::arrange(j)

            if (memory) {
              res <- dplyr::pull(col_stats, sd_x)
              if (useNames) names(res) <- colnames(x)
              return(res)
            }

            colSd <- dplyr::transmute(col_stats, i = j, j = 1L, x = sd_x)

            res <- new("dbDenseMatrix",
                       value = colSd,
                       name = NA_character_,
                       init = TRUE,
                       dims = c(ncol(x), 1L),
                       dim_names = list(colnames(x), "col1"))
            return(res)
          })

## rowSds dbdm ####
#' @concept summary
#' @rdname row_col_sds
#' @export
setMethod("rowSds", signature(x = "dbDenseMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = TRUE, center = NULL,
                   ..., memory = FALSE, useNames = TRUE) {

            x <- castNumeric(x)
            k <- ncol(x)

            row_stats <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(
                sum_x  = sum(x,      na.rm = na.rm),
                sum_x2 = sum(x * x , na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                sd_x = if (k <= 1) NA_real_
                else sqrt((sum_x2 - (sum_x * sum_x) / !!k) / (!!k - 1))
              ) |>
              dplyr::arrange(i)

            if (memory) {
              res <- dplyr::pull(row_stats, sd_x)
              if (useNames) names(res) <- rownames(x)
              return(res)
            }

            rowSd <- dplyr::transmute(row_stats, i = i, j = 1L, x = sd_x)

            res <- new("dbDenseMatrix",
                       value = rowSd,
                       name = NA_character_,
                       init = TRUE,
                       dims = c(nrow(x), 1L),
                       dim_names = list(rownames(x), "col1"))
            return(res)
          })

## rowSds dbsm ####
#' @concept summary
#' @rdname row_col_sds
#' @export
setMethod("rowSds", signature(x = "dbSparseMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = TRUE, center = NULL,
                   ..., memory = FALSE, useNames = TRUE) {

            x <- castNumeric(x)
            k <- ncol(x)

            row_stats <- x[] |>
              dplyr::group_by(i) |>
              dplyr::summarise(
                sum_x  = sum(x,      na.rm = na.rm),
                sum_x2 = sum(x * x , na.rm = na.rm),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                sd_x = if (k <= 1) NA_real_
                else sqrt((sum_x2 - (sum_x * sum_x) / k) / (k - 1))
              ) |>
              dplyr::arrange(i)

            if (memory) {
              res <- dplyr::pull(row_stats, sd_x)
              if (useNames) names(res) <- rownames(x)
              return(res)
            }

            rowSd <- dplyr::transmute(row_stats, i = i, j = 1L, x = sd_x)

            res <- new("dbDenseMatrix",
                       value = rowSd,
                       name = NA_character_,
                       init = TRUE,
                       dims = c(nrow(x), 1L),
                       dim_names = list(rownames(x), "col1"))
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

#' Arithmetic Mean for [`dbMatrix`] objects
#' @inheritParams base::mean
#' @inherit base::mean description
#' @param x [`dbMatrix`] object
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
#' @description `nrow` and `ncol` return the number of rows or columns present in `x`.
#' @param x [`dbMatrix`] object
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
#' @param x [`dbMatrix`] object
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
