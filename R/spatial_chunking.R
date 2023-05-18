

# SpatExtent generics ####
#' @rdname hidden_aliases
#' @importMethodsFrom terra ext ext<-
#' @export
setMethod('ext', signature(x = 'dbSpatProxyData'), function(x, ...) {
  x@extent
})
#' @rdname hidden_aliases
#' @export
setMethod('ext<-', signature(x = 'dbSpatProxyData', value = 'SpatExtent'), function(x, value) {
  x@extent = value
  x
})



# doSpatChunk ####

#' @export
setMethod('doSpatChunk',
          signature(x = 'dbPolygonProxy', what = 'character', args = 'list', extent = 'missing'),
          function(x, what, args, ...) {
            do.call('doSpatChunk', args = list(x = x, what = what, args = args, ..., extent = ext(x)))
          })



#' @export
setMethod('doSpatChunk',
          signature(x = 'dbPolygonProxy', what = 'character', args = 'list', extent = 'missing'),
          function(x, what, args, extent, ...) {

          })





