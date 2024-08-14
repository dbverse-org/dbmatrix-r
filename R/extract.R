# dbData ####
## Empty ####
### Extract [] ####
#' @rdname hidden_aliases
#' @concept dbMatrix
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@value
          })

### Set [] ####
# no initialize to prevent slowdown
#' @rdname hidden_aliases
#' @concept dbMatrix
#' @export
setMethod('[<-', signature(x = 'dbMatrix', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@value = value
            x
          })

# dbMatrix ####
## rows only ####
#' @rdname hidden_aliases
#' @concept dbMatrix
#' @export
setMethod('[',
          signature(
            x = 'dbMatrix',
            i = 'dbIndex',
            j = 'missing',
            drop = 'missing'
          ),
          function(x, i, ...) {
            # get dbMatrix info
            con = get_con(x)
            dim = dim(x)

            # check inputs
            .check_extract(x = x, i = i, j = NULL, dim = dim)

            # create mapping of filtered rownames to row index
            map = data.frame(i = seq_along(rownames(x)), rowname = rownames(x))
            filter_i = get_dbM_sub_idx(index = i,
                                       dbM_dimnames = x@dim_names, dims = 1)
            map = map |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            # send map to db for subsetting
            name = unique_table_name('temp_i')

            # FIXME: workaround for lack of support for writing
            # tables to a custom schema that is invisible to the user
            # i.e. not present in DBI::dbListTables(con)
            #
            # To see the tables in the arrow schema,
            # use duckdb::duckdb_list_arrow(conn = con)
            map_temp <- arrow::to_duckdb(
              .data = map, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            # subset dbMatrix
            x[] <- x[] |>
              dplyr::filter(i %in% !!map$i) |>
              dplyr::inner_join(map_temp, by = c("i" = "i")) |>
              dplyr::select(i = new_i, j, x)

            # update dbMatrix attributes
            x@dim_names[[1L]] = filter_i
            x@dims[1L] <- length(filter_i)

            return(x)
          })

## cols only ####
#' @rdname hidden_aliases
#' @concept dbMatrix
#' @export
setMethod('[',
          signature(
            x = 'dbMatrix',
            i = 'missing',
            j = 'dbIndex',
            drop = 'missing'
          ),
          function(x, j, ...) {
            # get dbMatrix info
            con <- get_con(x)
            dim = dim(x)

            # check for dims
            .check_extract(x = x, i = NULL, j = j, dim = dim)

            # create mapping of filtered colnames to col index
            map <- data.frame(j = seq_along(colnames(x)), colname = colnames(x))
            filter_j <- get_dbM_sub_idx(index = j,
                                       dbM_dimnames = x@dim_names, dims = 2)
            map <- map |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            # send map to db for subsetting
            name <- unique_table_name('temp_j')

            # FIXME:
            map_temp <- arrow::to_duckdb(
              .data = map, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            # Subset with arrow virtual table
            x[] <- x[] |>
              dplyr::filter(j %in% !!map$j) |>
              dplyr::inner_join(map_temp, by = c("j" = "j")) |>
              dplyr::select(i, j = new_j, x)

            # Update dbMatrix attributes
            x@dim_names[[2L]] <- filter_j
            x@dims[2L] <- length(filter_j)

            return(x)
          })

## rows and cols ####
#' @rdname hidden_aliases
#' @concept dbMatrix
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'dbIndex', j = 'dbIndex', drop = 'missing'),
          function(x, i, j, ...) {
            # get dbMatrix info
            con = get_con(x)
            dim = dim(x)

            # check for dims
            .check_extract(x = x, i = i, j = j, dim = dim)

            # create mapping of dim indices and dimnames
            map_i = data.frame(i = seq_along(rownames(x)),
                               rowname = rownames(x))

            map_j = data.frame(j = seq_along(colnames(x)),
                               colname = colnames(x))

            # subset map by filtered dimnames
            filter_i = get_dbM_sub_idx(index = i,
                                       dbM_dimnames = x@dim_names, dims = 1)
            filter_j = get_dbM_sub_idx(index = j,
                                       dbM_dimnames = x@dim_names, dims = 2)

            map_i = map_i |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            map_j = map_j |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            name = unique_table_name('temp_map_ij_i')
            map_temp_j <- arrow::to_duckdb(
              .data = map_j, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            name = unique_table_name('temp_map_ij_j')
            map_temp_i <- arrow::to_duckdb(
              .data = map_i, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            x[] <- x[] |>
              dplyr::filter(i %in% !!map_i$i, j %in% !!map_j$j) |>
              dplyr::inner_join(map_temp_i, by = c("i" = "i")) |>
              dplyr::inner_join(map_temp_j, by = c("j" = "j")) |>
              dplyr::select(i = new_i, j = new_j, x)

            # update dbMatrix attributes
            x@dim_names[[1L]] = filter_i
            x@dim_names[[2L]] = filter_j
            x@dims[1L] <- length(filter_i)
            x@dims[2L] <- length(filter_j)

            return(x)
          })
#' @description
#' Internal function to index `dbMatrix` objects by `dbIndex` superclass.
#' Can apply to both rows (dims = 1) and columns (dims = 2)
#' @keywords internal
#' @noRd
get_dbM_sub_idx = function(index, dbM_dimnames, dims) {
  # check that idx is 1 or 2
  if(dims != 1 && dims != 2){
    stop("dims must be 1 (rows) or 2 (columns)")
  }
  dims = as.integer(dims)

  if(is.character(index)){
    return(index)
  }

  if (is.logical(index)) {
    index <- recycle_boolean_index(index, length(dbM_dimnames[[dims]]))
  }

  sub_names = dbM_dimnames[[dims]]
  return(sub_names[index])
}

#' @details
#' Recycles a logical index to the length of a vector. This is to emulate the
#' strange behavior of what occurs in R when indexing matrices with a logical
#' vector.
#' Note:
#' Recycle indexing behavior is unique to logic indexing, not numerical nor
#' character indexing for matrices.
#' @noRd
#' @keywords internal
recycle_boolean_index <- function(index, length) {
  if (is.logical(index) && length(index) < length) {
    recycled <- rep_len(index, length)
    return(which(recycled))
  }
  return(index)
}

#' @noRd
#' @keywords internal
.check_extract <- function(x = x, i = NULL, j = NULL, dim){
  if (!is.null(j)) {
    if (is.numeric(j) & max(j) > dim[2]) {
      stopf("Index exceeds column dimension of", dim[2])
    }
    else if(is.character(j) & !all(j %in% colnames(x))) {
      missing_cols <- j[!j %in% colnames(x)]
      stopf("Column(s) not found in dbMatrix: \n", missing_cols)
    }
    else if (is.logical(j) & length(j) > dim[2]) {
      stopf("Index exceeds column dimension of", dim[2])
    }
  }
  if (!is.null(i)) {
    if (is.numeric(i) & max(i) > dim[1]) {
      stopf("Index exceeds row dimension of", dim[1])
    }
    else if(is.character(i) & !all(i %in% colnames(x))) {
      missing_cols <- i[!i %in% rownames(x)]
      stopf("Row(s) not found in dbMatrix: \n", missing_cols)
    }
    else if (is.logical(i) & length(i) > dim[1]) {
      stopf("Index exceeds row dimension of", dim[1])
    }
  }

}
