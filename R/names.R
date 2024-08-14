# names ####

# TODO ensure these match the row / col operations
# rownames ####
#' @rdname matrix_props
#' @concept matrix_props
#' @export
setMethod('rownames', signature(x = 'dbMatrix'), function(x) {
  rownames(x@value)
})

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
  colnames(x@value)
})

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
setMethod('dimnames<-', signature(x = 'dbMatrix', value = 'list'), function(x, value) {
  x@dim_names = value
  x
})
