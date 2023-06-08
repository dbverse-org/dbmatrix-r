
# functions to aid in plotting with database sources

# Plotting pipeline from
# https://blog.djnavarro.net/posts/2022-08-23_visualising-a-billion-rows/

#' @name desparse_to_grid_bin_data
#' @title Desparsify binned data and assign to rasterized locations
#' @param data data.frame
#' @param extent terra extent in which to plot
#' @param resolution min number of tiles to collect
#' @keywords internal
desparse_to_grid_bin_data = function(data, px_x, px_y) {
  checkmate::assert_class(data, 'data.frame')
  checkmate::assert_numeric(px_x, len = 1L)
  checkmate::assert_numeric(px_y, len = 1L)
  px_x = as.integer(px_x)
  px_y = as.integer(px_y)

  tidyr::expand_grid(x_bin = 1:px_x, y_bin = 1:px_y) %>%
    dplyr::left_join(data, by = c('x_bin', 'y_bin')) %>%
    dplyr::mutate(density = tidyr::replace_na(density, 0))
}




# function to bin spatial x and y point information
#' @name raster_bin_data
#' @title Bin database values in xy
#' @param data tbl_Pool to use
#' @param extent terra extent in which to plot
#' @param resolution min number of tiles to collect
#' @return matrix of rasterized point information
#' @keywords internal
raster_bin_data = function(data,
                           extent,
                           resolution = 5e4) {
  checkmate::assert_class(extent, 'SpatExtent')
  checkmate::assert_numeric(resolution)
  resolution = as.integer(resolution)
  checkmate::assert_class(data, 'dbPointsProxy')

  res = get_dim_n_chunks(n = resolution, e = extent)
  px_y = res[[1L]]
  px_x = res[[2L]]
  span_x = extent$xmax - extent$xmin ; names(span_x) = NULL
  span_y = extent$ymax - extent$ymin ; names(span_y) = NULL
  xmin = extent$xmin ; names(xmin) = NULL
  ymin = extent$ymin ; names(ymin) = NULL

  bin_data = data[] %>%
    extent_filter(extent = extent, include = rep(TRUE, 4L)) %>%
    dplyr::mutate(
      unit_scaled_x = (x - xmin) / span_x,
      unit_scaled_y = (y - ymin) / span_y,
      x_bin = as.integer(round(px_x * unit_scaled_x, digits = 0L)),
      y_bin = as.integer(round(px_y * unit_scaled_y, digits = 0L))) %>%
    dplyr::count(x_bin, y_bin, name = 'density') %>%
    dplyr::collect() %>%
    desparse_to_grid_bin_data(px_x = px_x, px_y = px_y)

  matrix(data = bin_data$density,
         nrow = px_y,
         ncol = px_x)
}




#' internal function for rasterized rendering from matrix data
render_image = function(mat,
                        cols = c("#002222", "white", "#800020"),
                        axes = FALSE,
                        ...) {
  # save current values for reset at end
  op = par()$mar
  on.exit(par(mar = op))
  par(mar = rep(0L, 4L))

  shades = grDevices::colorRampPalette(cols)
  graphics::image(
    z = log10(t(mat + 1)),
    axes = axes,
    asp = 1L,
    col = shades(256),
    useRaster = TRUE,
    ...
  )
}



setMethod('plot', signature(x = 'dbPointsProxy', y = 'missing'), function(x, resolution = 5e6, ...) {
  e = extent_calculate(x)
  bin_mat = raster_bin_data(data = x, extent = e, resolution = resolution)
  render_image(mat = bin_mat,
               x = seq(e$xmin, e$xmax, length.out = ncol(bin_mat)),
               y = seq(e$ymin, e$ymax, length.out = nrow(bin_mat)))
})





