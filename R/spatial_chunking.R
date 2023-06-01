

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


# setMethod('chunkApply')

#' @name chunkSpatApply
#' @title Apply a function over spatially chunked data
#' @param x dbPolygonProxy or dbPointsProxy
#' @param y dbPointsProxy (if appropriate for the function)
#' @param fun function to apply
#' @param extent (optional) spatial extent to chunk across. Takes the extent of
#' \code{x} by default
#' @param n_per_chunk (optional) desired max number of records to process per chunk
#' This value can be set using setting \code{options(gdb.nperchunk = ?)}
#' @param remote_name name to assign results in database
#' @param ... additional params to pass to \code{\link[future.apply]{future_lapply}}
NULL





# determine n chunks and allocate indices if needed
#' @name chunkSpatApply
#' @title Apply a function in a spatially chunked manner
#' @description
#' Split a function operation into multiple spatial chunks that is parallelized
#' if desired using \code{future_lapply}. Results are appended back into the
#' database as a new table. Use \code{chunkSpatApplyPoly} if the expected output
#' is polygon information or \code{chunkSpatApplyPoints} if points.
#' @param x dbPolygonProxy or dbPointsProxy
#' @param y missing/null if not needed. Otherwise accepts a dbPolygonProxy or
#' dbPointsProxy object
#' @param chunk_y (default = TRUE) whether y also needs to be spatially chunked
#' if it is provided.
#' @param fun function to apply
#' @param extent spatial extent of data to apply across. Defaults to the extent
#' of \code{x} if not given
#' @param n_per_chunk (default is 1e5) number of records to try to process per chunk.
#' This value can be set globally using options(gdb.nperchunk = ?)
#' @param remote_name name to assign the result in the database. Defaults to a
#' generic incrementing 'gdb_nnn' if not given
#' @param ... additional params to pass to GiottoDB object creation
#' @return dbPolygonProxy or dbPointsProxy
#' @export
chunkSpatApplyPoly = function(x = NULL,
                              y = NULL,
                              chunk_y = TRUE,
                              fun,
                              extent = NULL,
                              n_per_chunk = getOption('gdb.nperchunk', 1e5),
                              remote_name = NULL,
                              ...) {
  chunk_spat_apply(x = x, y = y, chunk_y = chunk_y, fun = fun, extent = extent,
                   n_per_chunk = n_per_chunk, remote_name = remote_name,
                   output = 'dbPolygonProxy', ...)
}
#' @rdname chunkSpatApply
#' @export
chunkSpatApplyPoints = function(x = NULL,
                                y = NULL,
                                chunk_y = TRUE,
                                fun,
                                extent = NULL,
                                n_per_chunk = getOption('gdb.nperchunk', 1e5),
                                remote_name = NULL) {
  chunk_spat_apply(x = x, y = y, chunk_y = chunk_y, fun = fun, extent = extent,
                   n_per_chunk = n_per_chunk, remote_name = remote_name,
                   output = 'dbPointsProxy', ...)
}


#TODO
# internal
chunk_spat_apply = function(x = NULL,
                            y = NULL,
                            chunk_y = TRUE,
                            fun,
                            extent = NULL,
                            n_per_chunk = 1e5,
                            remote_name = NULL,
                            output = c('tbl', 'dbPolygonProxy', 'dbPointsProxy'),
                            ...) {
  checkmate::assert_class(x, 'dbSpatProxyData')
  if(!is.null(y)) checkmate::assert_class(y, 'dbSpatProxyData')
  checkmate::assert_function(fun)
  if(is.null(extent)) extent = terra::ext(x)
  checkmate::assert_class(extent, 'SpatExtent')
  checkmate::assert_numeric(n_per_chunk)
  if(is.null(remote_name)) remote_name = result_count()
  checkmate::assert_character(remote_name)
  output = match.arg(output, choices = c('tbl', 'dbPolygonProxy', 'dbPointsProxy'))
  p = cPool(x)

  # determine chunking #
  # ------------------ #
  n_rec = nrow(x)
  min_chunks = n_rec / n_per_chunk
  #' chunk_plan slightly expands bounds, allowing for use of 'soft' selections
  #' with 'extent_filter() on two sides during the chunk processing
  ext_list = chunk_plan(extent = extent, min_chunks = min_chunks)


  # chunk data #
  # ---------- #
  #' Calculations with y are expected to be performed relative to x. Spatial
  #' chunk subsetting of y is performed based on the updated extent of the x
  #' chunks after their chunk subset.

  # filter data to chunk ROI
  chunk_x_list = lapply(
    ext_list,
    function(e) {
      # 'soft' selections on top and right
      extent_filter(x = x, extent = e, include = c(TRUE, TRUE, FALSE, FALSE),
                    method = 'mean')
    }
  )
  # filter out empty chunks
  chunk_x_list_len = lapply(chunk_x_list, nrow)
  not_empty = chunk_x_list_len > 0L
  chunk_x_list = chunk_x_list[not_empty]
  chunk_x_list_len = chunk_x_list_len[not_empty]

  # debug
  preview_chunk_plan(ext_list[not_empty])

  # filter any y input (if needed) based on x
  chunk_y_input = NULL
  if(!is.null(y)) {
    if(isTRUE(chunk_y)) {
      ext_x_list = lapply(chunk_x_list, extent_calculate)
      # hard selections on all sides.
      chunk_y_input = extent_filter(x = y, extent = ext_x_list,
                                    include = rep(TRUE, 4))
    } else {
      chunk_y_input = y
    }
  }


  #' 1. Setup lapply
  #' 2. Run provided function
  #' 3. write or return values
  n_chunks = length(chunk_x_list)
  progressr::with_progress({
    pb = progressr::progressor(steps = n_chunks)
    chunk_outs = future.apply::future_lapply(
      X = n_chunks,
      future.packages = c('terra'),
      function(chunk_x_list, chunk_y_input, fun, p, ..., chunk_i) {

        # run function on x, and conditionally, y
        out = if(is.null(chunk_y_input)) {
          # x only
          fun(x = chunk_x_list[[chunk_i]])
        } else {
          # x and y
          if(length(chunk_y_input) == 1L) {
            # single y
            fun(x = chunk_x_list[[chunk_i]],
                y = chunk_y_input)
          } else {
            # chunked y values
            fun(x = chunk_x_list[[chunk_i]],
                y = chunk_y_input[[chunk_i]])
          }
        }

        # write/append values ?
        # include expected geom values for polygons
        # ... params go here

        # update progress and return
        pb()
        return(out)
      },
      chunk_x_list = chunk_x_list,
      chunk_y_input = chunk_y_input,
      fun = fun,
      p = p,
      ...
    )
  })




  # generate object #
  # --------------- #
  out = switch(output,
               'tbl' = {

               },
               'dbPolygonProxy' = {

               },
               'dbPointsProxy' = {

               })

  return(out)
}











# helper functions ####

#' @name get_dim_n_chunks
#' @title Get rows and cols needed to create at least n chunks from given extent
#' @description Algorithm to determine how to divide up a provided extent into
#' at least \code{n} different chunks. The chunks are arranged so as to prefer
#' being as square as posssible with the provided dimensions and minimum n chunks.
#' @param n minimum n chunks
#' @param e selection extent
#' @examples
#' \dontrun{
#' e <- terra::ext(0, 100, 0, 100)
#' get_dim_n_chunk(n = 5, e = e)
#' }
#' @seealso \code{\link{chunk_plan}}
#' @return numeric vector of x and y stops needed
get_dim_n_chunks = function(n, e) {
  # find x to y ratio as 'r'
  e = e[]
  r = (e[['xmax']] - e[['xmin']]) / (e[['ymax']] - e[['ymin']])

  # x * y = n = ... ry^2 = n
  y = ceiling(sqrt(n/r))
  x = ceiling(n/y)

  return(c(y,x))
}


#' @name chunk_plan
#' @title Plan spatial chunking extents
#' @description
#' Generate the individual extents that will be used to spatially chunk a set of
#' data for piecewise and potentially parallelized processing. Chunks will be
#' generated first by row, then by column. The chunks try to be as square as
#' possible since downstream functions may require slight expansions of the
#' extents to capture all parts of selected polygons. Minimizing the perimeter
#' relative to area decreases waste.
#' @param extent terra SpatExtent that covers the region to spatially chunk
#' @param nrows,ncols numeric. nrow/ncol must be provided as a pair. Determines how many
#' rows and cols respectively will be used in spatial chunking. If NULL, min_chunks
#' will be used as an automated method of planning the spatial chunking
#' @param min_chunks numeric. minimum number of chunks to use.
#' @seealso \code{\link{get_dim_n_chunks}}
#' @examples
#' \dontrun{
#'  e <- terra::ext(0, 100, 0, 100)
#'
#' e_chunk1 <- chunk_plan(e, min_chunks = 9)
#' e_poly1 <- sapply(e_chunk1, terra::as.polygons)
#' e_poly1 <- do.call(rbind, e_poly1)
#' plot(e_poly1)
#'
#' e_chunk2 <- chunk_plan(e, nrows = 3, ncols = 5)
#' e_poly2 <- sapply(e_chunk2, terra::as.polygons)
#' e_poly2 <- do.call(rbind, e_poly2)
#' plot(e_poly2)
#' }
#' @keywords internal
#' @return vector of chunked SpatExtents
chunk_plan = function(extent, min_chunks = NULL, nrows = NULL, ncols = NULL) {
  checkmate::assert_class(extent, 'SpatExtent')
  if(!is.null(nrows)) checkmate::assert_true(length(c(nrows, ncols)) == 2L)
  else {
    checkmate::assert_numeric(min_chunks)
    res = get_dim_n_chunks(n = min_chunks, e = extent)
    nrows = res[1L]
    ncols = res[2L]
  }

  # slightly expand bounds to account for values that may otherwise be missed
  e = extent[] + c(-1, 1, -1, 1)
  x_stops = seq(from = e[['xmin']], to = e[['xmax']], length.out = ncols + 1L)
  y_stops = seq(from = e[['ymin']], to = e[['ymax']], length.out = nrows + 1L)

  ext_list = c()
  for(i in seq(nrows)) {
    for(j in seq(ncols)) {
      ext_list =
        c(ext_list,
          terra::ext(x_stops[j], x_stops[j + 1L], y_stops[i], y_stops[i + 1L]))
    }
  }
  return(ext_list)
}




#' @name preview_chunk_plan
#' @title Plot a preview of the chunk plan
#' @description
#' Plots the output from \code{\link{chunk_plan}} as a set of polygons to preview.
#' Can be useful for debugging. Invisibly returns the planned chunks as a SpatVector
#' of polygons
#' @param extent_list list of extents from \code{chunk_plan}
#' @keywords internal
preview_chunk_plan = function(extent_list) {
  checkmate::assert_list(extent_list, types = 'SpatExtent')
  poly_list = sapply(extent_list, terra::as.polygons)
  poly_bind = do.call(rbind, poly_list)
  terra::plot(poly_bind, values = as.factor(seq_along(poly_list)))
  invisible(poly_bind)
}






