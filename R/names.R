# names ####
# TODO: add @value once setter is implemented, update to 1D dbMatrix
#' The names of a dbMatrix Object
#' @param x A dbMatrix object
#' @concept matrix_props
#' @return A character vector of the names of the 1D dbMatrix object (1D matrices only)
setMethod('names', signature(x = 'dbDenseMatrix'), function(x) {
  # Only 1D dbMatrix objects (1-dimensional matrices) should have names
  # Regular matrices should return NULL for names()
  if (!1 %in% x@dims) {
    return(NULL) # Regular matrices don't have names, return NULL instead of error
  }
  if (1 %in% dim(x)[1]) {
    return(colnames(x))
  } else {
    return(rownames(x))
  }
})

#TODO implement names <- setter

# rownames ####
#' Retrieve and Set Row (Column) Dimension Names of dbMatrix Objects
#' @inheritParams base::rownames
#' @param do.NULL Not used for this method. Included for compatibility with the
#' generic.
#' @param prefix Not used for this method. Included for compatibility with the
#' generic.
#' @rdname matrix_props
#' @concept matrix_props
#' @export
rownames.dbMatrix <- function(x, do.NULL = TRUE, prefix = "row") {
  as.character(x@dim_names[[1]])
}

#' @rdname matrix_props
#' @concept matrix_props
#' @usage \method{rownames}{dbMatrix}(x) <- value
#' @export
`rownames<-.dbMatrix` <- function(x, value) {
  if (is.null(value)) {
    stopf('rownames are required for dbMatrix objects')
  }

  if (x@dims[1] != length(value)) {
    stopf('length of rownames to set does not equal number of rows')
  }
  x@dim_names[[1]] <- value
  x
}

# colnames ####
#' @rdname matrix_props
#' @concept matrix_props
#' @export
colnames.dbMatrix <- function(x, do.NULL = TRUE, prefix = "col") {
  as.character(x@dim_names[[2]])
}

#' @rdname matrix_props
#' @concept matrix_props
#' @usage \method{colnames}{dbMatrix}(x) <- value
#' @export
`colnames<-.dbMatrix` <- function(x, value) {
  if (x@dims[2] != length(value)) {
    stopf('length of colnames to set does not equal number of columns')
  }

  x@dim_names[[2]] <- value
  x
}

# dimnames ####
#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('dimnames', signature(x = 'dbMatrix'), function(x) {
  list(
    as.character(x@dim_names[[1]]),
    as.character(x@dim_names[[2]])
  )
})

#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod(
  'dimnames<-',
  signature(x = 'dbMatrix', value = 'list'),
  function(x, value) {
    x@dim_names <- value
    x
  }
)

# internal functions ####
#' @keywords internal
#' @noRd
#' @param x dbMatrix object
#' @description
#' Function to write dbMatrix dimnames to a database. This function is intended
#' to be used ONLY when saving a dbMatrix object, to permit recovery
#' of the dimnames when the object is loaded back into memory.
#'
.write_dimnames <- function(x, name) {
  con <- dbplyr::remote_con(x@value)
  .check_con(con)
  dimnames <- dimnames(x)

  if (!inherits(x, 'dbMatrix')) {
    stopf('x must be a dbMatrix object')
  }
  if (is.na(x@name) && is.null(name)) {
    stopf('Name is empty. Use dbSave() to save the lazy dbMatrix object.')
  }
  check_names <- c(
    paste0("__", name, "_rownames"),
    paste0("__", name, "_colnames")
  )
  registered_names <- duckdb::duckdb_list_arrow(conn = con)
  names_to_unregister <- check_names[check_names %in% registered_names]
  names_to_remove <- check_names[check_names %in% DBI::dbListTables(con)]
  if (length(names_to_unregister) > 0) {
    sapply(names_to_unregister, function(name) {
      duckdb::duckdb_unregister_arrow(conn = con, name = name)
    })
  }
  if (length(names_to_remove) > 0) {
    sapply(names_to_remove, function(name) {
      DBI::dbRemoveTable(con, name)
    })
  }

  # Include explicit integer indices to avoid non-deterministic ROW_NUMBER()
  rownames_dt <- data.table::data.table(
    i = seq_along(dimnames[[1]]),
    rownames = dimnames[[1]] |> as.character()
  )
  colnames_dt <- data.table::data.table(
    j = seq_along(dimnames[[2]]),
    colnames = dimnames[[2]] |> as.character()
  )

  # NOTE: Using __ prefix to distinguish from user tables
  # temporary = FALSE ensures tables persist across connection close/reopen
  dplyr::copy_to(
    df = rownames_dt,
    dest = con,
    name = paste0("__", name, "_rownames"),
    temporary = FALSE,
    overwrite = TRUE
  ) |>
    invisible()

  dplyr::copy_to(
    df = colnames_dt,
    dest = con,
    name = paste0("__", name, "_colnames"),
    temporary = FALSE,
    overwrite = TRUE
  ) |>
    invisible()
}

#' @keywords internal
#' @noRd
#' @return character
.assign_dbm_name <- function(dbm, name = NULL) {
  if (is.null(name)) {
    name <- unique_table_name('tmp_dbm')
  }
  dbm[] <- to_view(x = dbm[], name = name)
  dbm@name <- name

  return(name)
}
