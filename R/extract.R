
# dbData ####
## Empty ####
### Extract [] ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbData', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@data
          })



### Set [] ####
# no initialize to prevent slowdown
#' @rdname hidden_aliases
#' @export
setMethod('[<-', signature(x = 'dbData', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@data = value
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









