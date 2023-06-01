
#' @importFrom methods setAs
#' @importFrom terra as.polygons
#' @importFrom terra as.points
NULL


# Data Coercion




# as.matrix ####


#' @rdname hidden_aliases
#' @title Convert a GiottoDB object to a matrix
#' @param x dbMatrix
#' @param ... additional params to pass
#' @export
setMethod('as.matrix', signature('dbMatrix'), function(x, ...) {
  # message('Pulling DB matrix into memory...')
  p_tbl = x@data %>%
    tidyr::pivot_wider(names_from = 'j', values_from = 'x') %>%
    dplyr::collect()

  mtx = p_tbl %>%
    dplyr::select(-i) %>%
    as('matrix')

  rownames(mtx) = p_tbl$i

  return(mtx)
})





# as.polygons ####

#' @rdname hidden_aliases
#' @export
setMethod('as.polygons', signature('dbPolygonProxy'), function(x, ...) {
  dbspat_to_sv(x)
})


# as.points ####
#' @rdname hidden_aliases
#' @export
setMethod('as.points', signature('dbPointsProxy'), function(x, ...) {
  dbspat_to_sv(x)
})






