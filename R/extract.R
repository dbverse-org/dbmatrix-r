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
setMethod('[', signature(x = 'dbMatrix', i = 'gdbIndex', j = 'missing', drop = 'missing'),
          function(x, i, ...) {
            select = get_dbM_sub_i(index = i, dbM_dimnames = x@dim_names)
            x@dim_names[[1L]] = select
            x[] = x[] %>% dplyr::filter(i %in% select)
            x@dims[1L] = if(is.logical(i)) sum(i) else length(i)
            x
          })

## cols only ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'missing', j = 'gdbIndex', drop = 'missing'),
          function(x, j, ...) {
            select = get_dbM_sub_j(index = j, dbM_dimnames = x@dim_names)
            x@dim_names[[2L]] = select
            x[] = x[] %>% dplyr::filter(j %in% select)
            x@dims[2L] = if(is.logical(j)) sum(j) else length(j)
            x
          })

## rows and cols ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'gdbIndex', j = 'gdbIndex', drop = 'missing'),
          function(x, i, j, ...) {
            select_i = get_dbM_sub_i(index = i, dbM_dimnames = x@dim_names)
            select_j = get_dbM_sub_j(index = j, dbM_dimnames = x@dim_names)
            x@dim_names[[1L]] = select_i
            x@dim_names[[2L]] = select_j
            x[] = x[] %>% dplyr::filter(i %in% select_i, j %in% select_j)
            x@dims[1L] = if(is.logical(i)) sum(i) else length(i)
            x@dims[2L] = if(is.logical(j)) sum(j) else length(j)
            x
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
  '[', signature(x = 'dbDataFrame', i = 'gdbIndex', j = 'missing', drop = 'ANY'),
  function(x, i, ..., drop = FALSE) {
    x = reconnect(x)
    if(any(is.na(x@key))) stopf('Set dbDataFrame key with `keyCol()` to subset on \'i\'')

    # numerics and logical
    if(is.logical(i) | is.numeric(i)) {
      if(is.logical(i)) i = which(i)
      x@data = x@data %>%
        flex_window_order(x@key) %>%
        dplyr::mutate(.n = dplyr::row_number()) %>%
        dplyr::collapse() %>%
        dplyr::filter(.n %in% i) %>%
        dplyr::select(-.n) %>%
        dplyr::collapse()
    } else { # character
      x@data = x@data %>%
        flex_window_order(x@key) %>%
        dplyr::filter(!!as.name(x@key) %in% i) %>%
        dplyr::collapse()
    }
    x
  })

## cols only ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbDataFrame', i = 'missing', j = 'gdbIndex', drop = 'ANY'),
          function(x, j, ..., drop = FALSE) {
            x = reconnect(x)
            checkmate::assert_logical(drop)

            if(is.logical(j)) j = which(j)
            x@data = x@data %>% dplyr::select(dplyr::all_of(j))
            x
          })

#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbDataFrame', i = 'gdbIndex', j = 'gdbIndex', drop = 'ANY'),
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
#'@param x
#'@param order_cols
#'@description
#'workaround for multiple dbplyr window column ordering
#'
#'
#'@keywords internal
flex_window_order = function(x, order_cols) {
  keys = paste0('!!as.name("', order_cols, '")')
  keys = paste0(keys, collapse = ', ')
  call_str = paste0('x %>% dbplyr::window_order(', keys, ')')
  eval(str2lang(call_str))
}
