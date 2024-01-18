# names ####

#' @rdname hidden_aliases
#' @export
setMethod('names', signature(x = 'dbDataFrame'), function(x) {
  x = reconnect(x)
  colnames(x)
})

#' @rdname hidden_aliases
#' @export
setMethod('names<-', signature(x = 'dbDataFrame', value = 'dbIndex'), function(x, value) {
  x = reconnect(x)
  dplyr_set_colnames(x, value = as.character(value))
})

# TODO ensure these match the row / col operations
# rownames ####
#' @rdname hidden_aliases
#' @export
setMethod('rownames', signature(x = 'dbData'), function(x) {
  rownames(x@value)
})

#' @rdname hidden_aliases
#' @export
setMethod('rownames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[1]]
})

#' @rdname hidden_aliases
#' @export
setMethod('rownames<-', signature(x = 'dbMatrix'), function(x, value) {
  if(x@dims[1] != length(value)){
    stopf('length of rownames to set does not equal number of rows')
  }
  x@dim_names[[1]] = value
  x
})

# colnames ####
#' @rdname hidden_aliases
#' @export
setMethod('colnames', signature(x = 'dbData'), function(x) {
  colnames(x@value)
})

#' @rdname hidden_aliases
#' @export
setMethod('colnames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names[[2]]
})

#' @rdname hidden_aliases
#' @export
setMethod('colnames<-', signature(x = 'dbMatrix'), function(x, value) {
  if(x@dims[2] != length(value)){
    stopf('length of colnames to set does not equal number of columns')
  }

  x@dim_names[[2]] = value
  x
})

#' @rdname hidden_aliases
#' @export
setMethod('colnames<-', signature(x = 'dbDataFrame', value = 'dbIndex'), function(x, value) {
  x = reconnect(x)
  dplyr_set_colnames(x = x, value = as.character(value))
})

# dimnames ####
#' @rdname hidden_aliases
#' @export
setMethod('dimnames', signature(x = 'dbMatrix'), function(x) {
  x@dim_names
})

#' @rdname hidden_aliases
#' @export
setMethod('dimnames<-', signature(x = 'dbMatrix', value = 'list'), function(x, value) {
  x@dim_names = value
  x
})

#' @rdname hidden_aliases
#' @export
setMethod('dimnames', signature(x = 'dbDataFrame'), function(x) {
  dimnames(x[])
})

#' @rdname hidden_aliases
#' @export
setMethod('dimnames<-', signature(x = 'dbDataFrame', value = 'list'), function(x, value) {
  x = dplyr_set_colnames(x, value = as.character(value[[2]]))
  x
})
