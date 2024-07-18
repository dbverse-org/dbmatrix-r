# dbData ####
## Empty ####
### Extract [] ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbData', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@value
          })

### Set [] ####
# no initialize to prevent slowdown
#' @rdname hidden_aliases
#' @export
setMethod('[<-', signature(x = 'dbData', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@value = value
            x
          })

# dbMatrix ####
## rows only ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'dbIndex', j = 'missing', drop = 'missing'),
          function(x, i, ...) {
            # get dbMatrix info
            con = get_con(x)
            tbl_name = get_tblName(x)

            # create mapping of filtered rownames to row index
            # note: https://duckdb.org/docs/sql/statements/create_sequence.html
            map = data.frame(i = seq_along(rownames(x)), rowname = rownames(x))
            filter_i = get_dbM_sub_i(index = i, dbM_dimnames = x@dim_names)
            map = map |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            # send map to db for subsetting
            # TODO: implement unique naming of temp tables
            map_temp <- dplyr::copy_to(dest = con,
                                       df = map,
                                       name = 'map_temp_i',
                                       overwrite = TRUE,
                                       temporary = TRUE)

            # subset dbMatrix
            x@value <- x@value |>
              dplyr::filter(i %in% !!map$i) |>
              dplyr::inner_join(map_temp, by = c("i" = "i")) |>
              dplyr::select(i = new_i, j, x)

            # update dbMatrix attributes
            x@dim_names[[1L]] = filter_i
            x@dims[1L] <- ifelse(is.logical(i), sum(i), length(i))

            return(x)
          })

## cols only ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'missing', j = 'dbIndex', drop = 'missing'),
          function(x, j, ...) {
            # get dbMatrix info
            con = get_con(x)
            tbl_name = get_tblName(x)

            # create mapping of filtered colnames to col index
            # note: https://duckdb.org/docs/sql/statements/create_sequence.html
            map = data.frame(j = seq_along(colnames(x)), colname = colnames(x))
            filter_j = get_dbM_sub_j(index = j, dbM_dimnames = x@dim_names)
            map = map |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            # send map to db for subsetting
            # TODO: implement unique table name
            duckdb::dbWriteTable(conn = con,
                                 name = 'map_temp_j',
                                 overwrite = TRUE,
                                 value = map,
                                 temporary = TRUE)
            map_temp <- dplyr::tbl(con, "map_temp_j")

            # subset dbMatrix
            x@value <- x@value |>
              dplyr::filter(j %in% !!map$j) |>
              dplyr::inner_join(map_temp, by = c("j" = "j")) |>
              dplyr::select(i, j = new_j, x)

            # update dbMatrix attributes
            x@dim_names[[2L]] = filter_j
            x@dims[2L] <- ifelse(is.logical(j), sum(j), length(j))

            return(x)
          })

## rows and cols ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'dbIndex', j = 'dbIndex', drop = 'missing'),
          function(x, i, j, ...) {
            # get dbMatrix info
            con = get_con(x)
            tbl_name = get_tblName(x)

            # create mapping of dim indices and dimnames
            map_i = data.frame(i = seq_along(rownames(x)),
                             rowname = rownames(x))

            map_j = data.frame(j = seq_along(colnames(x)),
                               colname = colnames(x))

            # subset map by filtered dimnames
            # note: https://duckdb.org/docs/sql/statements/create_sequence.html
            filter_i = get_dbM_sub_i(index = i, dbM_dimnames = x@dim_names)
            filter_j = get_dbM_sub_j(index = j, dbM_dimnames = x@dim_names)

            map_i = map_i |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            map_j = map_j |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            # subset dbMatrix j
            # TODO: implement unique table name
            duckdb::dbWriteTable(conn = con,
                                 name = 'map_temp_ij_i',
                                 overwrite = TRUE,
                                 value = map_j,
                                 temporary = TRUE)
            map_temp <- dplyr::tbl(con, "map_temp_ij_i")

            x@value <- x@value |>
              dplyr::filter(j %in% !!map_j$j) |>
              dplyr::inner_join(map_temp, by = c("j" = "j")) |>
              dplyr::select(i, j = new_j, x)

            # subset dbMatrix i
            # TODO: implement unique table name
            duckdb::dbWriteTable(conn = con,
                                 name = 'map_temp_ij_j',
                                 overwrite = TRUE,
                                 value = map_i,
                                 temporary = TRUE)

            map_temp <- dplyr::tbl(con, "map_temp_ij_j")

            x@value <- x@value |>
              dplyr::filter(i %in% !!map_i$i) |>
              dplyr::inner_join(map_temp, by = c("i" = "i")) |>
              dplyr::select(i=new_i, j , x)

            # update dbMatrix attributes
            x@dim_names[[1L]] = filter_i
            x@dim_names[[2L]] = filter_j
            x@dims[1L] <- ifelse(is.logical(i), sum(i), length(i))
            x@dims[2L] <- ifelse(is.logical(j), sum(j), length(j))

            return(x)
          })

#' @noRd
get_dbM_sub_i = function(index, dbM_dimnames) {
  if(is.character(index)) return(index)
  i_names = dbM_dimnames[[1L]]
  return(i_names[index])
}

#' @noRd
get_dbM_sub_j = function(index, dbM_dimnames) {
  if(is.character(index)) return(index)
  j_names = dbM_dimnames[[2L]]
  return(j_names[index])
}


# dbDataFrame ####
## rows only ####
#' @rdname hidden_aliases
#' @export
setMethod(
  '[', signature(x = 'dbDataFrame', i = 'dbIndex', j = 'missing', drop = 'ANY'),
  function(x, i, ..., drop = FALSE) {
    x = reconnect(x)
    if(any(is.na(x@key))) stopf('Set dbDataFrame key with `keyCol()` to subset on \'i\'')

    # numerics and logical
    if(is.logical(i) | is.numeric(i)) {
      if(is.logical(i)) i = which(i)
      x@data = x@data |>
        flex_window_order(x@key) |>
        dplyr::mutate(.n = dplyr::row_number()) |>
        dplyr::collapse() |>
        dplyr::filter(.n %in% i) |>
        dplyr::select(-.n) |>
        dplyr::collapse()
    } else { # character
      x@data = x@data |>
        flex_window_order(x@key) |>
        dplyr::filter(!!as.name(x@key) %in% i) |>
        dplyr::collapse()
    }
    x
  })

## cols only ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbDataFrame', i = 'missing', j = 'dbIndex', drop = 'ANY'),
          function(x, j, ..., drop = FALSE) {
            x = reconnect(x)
            checkmate::assert_logical(drop)

            if(is.logical(j)) j = which(j)
            x@data = x@data |> dplyr::select(dplyr::all_of(j))
            x
          })

#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbDataFrame', i = 'dbIndex', j = 'dbIndex', drop = 'ANY'),
          function(x, i, j, ..., drop = FALSE) {
            x = reconnect(x)
            x = x[i,]
            x = x[, j]
            x
          })

## Empty ####
### Extract [] ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbDataFrame', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@data
          })

### Set [] ####
# no initialize to prevent slowdown
#' @rdname hidden_aliases
#' @export
setMethod('[<-', signature(x = 'dbDataFrame', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@data = value
            x
          })

#'@ flex_window_order
#'
#'@param x dbplyr::tbl_lazy
#'@param order_cols character vector of column names
#'@description
#'workaround for multiple dbplyr window column ordering
#'
#'
#'@keywords internal
flex_window_order = function(x, order_cols) {
  keys = paste0('!!as.name("', order_cols, '")')
  keys = paste0(keys, collapse = ', ')
  call_str = paste0('x |> dbplyr::window_order(', keys, ')')
  eval(str2lang(call_str))
}
