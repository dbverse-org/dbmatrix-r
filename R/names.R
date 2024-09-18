# names ####

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
setMethod('rownames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[1]]
})

#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('rownames<-', signature(x = 'dbMatrix'), function(x, value) {
  if(is.null(value)){
    stopf('rownames are required for dbMatrix objects')
  }

  if(x@dims[1] != length(value)){
    stopf('length of rownames to set does not equal number of rows')
  }
  x@dim_names[[1]] = value
  x
})

# colnames ####
#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('colnames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[2]]
})

#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('colnames<-', signature(x = 'dbMatrix'), function(x, value) {
  if(x@dims[2] != length(value)){
    stopf('length of colnames to set does not equal number of columns')
  }

  x@dim_names[[2]] = value
  x
})

# dimnames ####
#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('dimnames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names
})

#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('dimnames<-', signature(x = 'dbMatrix', value = 'list'),
          function(x, value) {
  x@dim_names = value
  x
})

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
  dimnames = dimnames(x)

  if(!inherits(x, 'dbMatrix')){
    stopf('x must be a dbMatrix object')
  }
  if (is.na(x@name) & is.null(name)) {
    stopf('Name is empty. Use dbSave() to save the lazy dbMatrix object.')
  }
  check_names <- c(paste0(name, "_rownames"), paste0(name, "_colnames"))
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

  rownames_dt = data.table::data.table(rownames = dimnames[[1]] |> as.character())
  colnames_dt = data.table::data.table(colnames = dimnames[[2]] |> as.character())

  dplyr::copy_to(
    df = rownames_dt,
    dest = con,
    name = paste0(name, "_temp_rownames"),
    overwrite = TRUE
  ) |>
    invisible()

  dplyr::copy_to(
    df = colnames_dt,
    dest = con,
    name = paste0(name, "_temp_colnames"),
    overwrite = TRUE
  ) |>
    invisible()

  # FIXME: compute in separate schema to hide from DBI::dbListTables()
  dplyr::tbl(con, paste0(name, "_temp_rownames")) |>
    dplyr::mutate(i = dplyr::row_number()) |>
    dplyr::compute(temporary = FALSE, name = paste0(name, "_rownames")) |>
    invisible()

  dplyr::tbl(con, paste0(name, "_temp_colnames")) |>
    dplyr::mutate(j = dplyr::row_number()) |>
    dplyr::compute(temporary = FALSE, name = paste0(name, "_colnames")) |>
    invisible()

  # remove temporary tables
  DBI::dbRemoveTable(con, paste0(name, "_temp_rownames"))
  DBI::dbRemoveTable(con, paste0(name, "_temp_colnames"))
}
