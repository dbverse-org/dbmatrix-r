

# Empty ####
## Extract [] ####
#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbData', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@data
          })



## Set [] ####
# no initialize to prevent slowdown
#' @rdname hidden_aliases
#' @export
setMethod('[<-', signature(x = 'dbData', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@data = value
            x
          })





# numeric ####

#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'numeric', j = 'missing', drop = 'missing'),
          function(x, i, ...) {
            x@dim_names[[1L]] = x@dim_names[[1L]][[i]]
            x[] = x[] %in% dplyr::filter(i %in% x@dim_names[[1L]])
            x@dim[1L] = as.integer(n)
            x
          })

#' @rdname hidden_aliases
#' @export
setMethod('[', signature(x = 'dbMatrix', i = 'missing', j = 'numeric', drop = 'missing'),
          function(x, j, ...) {
            x@dim_names[[2L]] = x@dim_names[[2L]][[i]]
            x[] = x[] %in% dplyr::filter(j %in% x@dim_names[[2L]])
            x@dim[2L] = as.integer(n)
            x
          })













