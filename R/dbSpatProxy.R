

# collate
#' @include dbDataFrame.R
NULL



# initialize ####
## dbSpatProxyData ####
setMethod('initialize', signature('dbSpatProxyData'), function(.Object, extent, ...) {

  # call dbData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbSpatProxyData specific data input #
  # ----------------------------------- #

  if(!missing(extent)) if(!is.null(extent)) .Object@extent = extent

  if(!is.null(.Object@data)) {
    if(sum(.Object@extent[]) == 0) .Object@extent = extent_calculate(.Object)
  }

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})



## dbPolygonProxy ####
setMethod('initialize', signature('dbPolygonProxy'), function(.Object, attributes, n_poly, ...) {

  # call dbSpatProxyData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbPolygonProxy specific data input #
  # ---------------------------------- #

  if(!missing(attributes)) if(!is.null(attributes)) .Object@attributes = attributes
  if(!missing(n_poly)) .Object@n_poly = n_poly


  # try to generate if lazy table for attributes if it does not exist
  # the hash and remote_name are needed for this
  if(!is_init(.Object@attributes)) {
    if(!is.na(.Object@hash) & !is.na(.Object@remote_name)) {

      .Object@attributes =
        new('dbDataFrame',
            hash = .Object@hash,
            remote_name = paste0(.Object@remote_name, '_attr'),
            key = 'geom')
    }
  }

  if(!is.null(.Object@data)) {
    if(is.null(.Object@n_poly)) .Object@n_poly =
        dplyr::summarise(.Object@data, max(geom))
  }

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})





## dbPointsProxy ####
setMethod('initialize', signature('dbPointsProxy'), function(.Object, n_point, ...) {

  # call dbSpatProxyData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbPolygonProxy specific data input #
  # ---------------------------------- #

  if(!missing(n_point)) .Object@n_point = n_point

  if(!is.null(.Object@data)) {
    if(is.null(.Object@n_point)) .Object@n_point =
        dplyr::summarise(.Object@data, max(geom))
  }

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})








# object creation ####

# Functions to create the indicated objects.
# OVERVIEW OF STEPS:
# From a provided input, whether already made or a filepath, if needed, the data
# is read in, formatted properly, and then added to the database backend.
# Following this, the class object is created and a lazy table connected to the
# read-in data is attached.

#' @name createDBPolygonProxy
#' @title Create a framework for chunked processing with terra polygon SpatVectors
#' @description
#' Create an S4 dbPolygonProxy object that is composed of two database tables.
#' One table hold geometry information while the other holds attribute information.
#' @param SpatVector object coercible to SpatVector or filepath to spatial data
#' readable by \code{\link[terra]{vect}}
#' @param remote_name name of remote table on database backend
#' @param db_path filepath to the database backend
#' @param id_col column in data to read in that contains the polygon id information
#' @param xy_col columns in data to read in that contain the x and y vertex info
#' @param extent terra SpatExtent (optional) that can be used to subset the data
#' to read in before it is saved to database
#' @param overwrite whether to overwrite if \code{remote_name} already exists on
#' the database
#' @param chunk_size the number of polygons to read in per chunk read
#' @param callback data formatting and manipulations to perform chunkwise before the data
#' is saved to database. Instructions should be provided as a function that takes
#' an input of a data.table and returns a data.table
#' @param custom_table_fields (optional) custom table field SQL settings to use
#' during table creation for the spatial geometry table
#' @param custom_table_fields_attr (optional) custom table field SQL settings to
#' use during table creation for the attributes table
#' @param attributes (optional) a \code{tbl_sql} connected to the database backend
#' that contains the associated attributes table. Only used if a pre-made \code{tbl_sql}
#' is also provided to \code{SpatVector} param
#' @details Information is only read into the database during this process. Based
#' on the \code{remote_name} and \code{db_path} a lazy connection is then made
#' downstream during dbData intialization and appended to the object. If the data
#' already exists within the database backend then it is entirely permissible to
#' omit the \code{SpatVector} param.
#' @export
createDBPolygonProxy = function(SpatVector,
                                remote_name = 'poly_test',
                                db_path = ':temp:',
                                id_col = 'poly_ID',
                                xy_col = c('x', 'y'),
                                extent = NULL,
                                overwrite = FALSE,
                                chunk_size = 10000L,
                                callback = NULL,
                                custom_table_fields = fields_preset$dbPoly_geom,
                                custom_table_fields_attr = NULL,
                                attributes = NULL,
                                ...) {
  db_path = getDBPath(db_path)
  backend_ID = calculate_backend_id(db_path)
  p = getBackendPool(backend_ID)
  if(inherits(SpatVector, 'tbl')) assert_in_backend(x = SpatVector, p = p)
  if(inherits(attributes, 'dbDataFrame')) assert_in_backend(x = attributes[], p = p)

  data = NULL
  atts = NULL
  if(inherits(SpatVector, 'tbl_Pool')) { # data is already in DB and tbl is provided
    data = SpatVector
    atts = attributes
  } else { # data must be read in

    # database input #
    overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)

    # read data if needed
    if(is.character(SpatVector)) {
      n_poly = streamSpatialToDB_arrow(path = SpatVector,
                                       backend_ID = backend_ID,
                                       remote_name = remote_name,
                                       id_col = id_col,
                                       xy_col = xy_col,
                                       extent = extent,
                                       overwrite = overwrite,
                                       chunk_size = chunk_size,
                                       callback = callback,
                                       custom_table_fields = custom_table_fields,
                                       custom_table_fields_attr = custom_table_fields_attr,
                                       ...)
    }

  }

  # if values are NULL:
  # data table attached during init
  # attributes table attached during init
  # extent values are calculated during init
  dbPoly = new('dbPolygonProxy',
               data = data,
               attributes = atts,
               hash = backend_ID,
               remote_name = remote_name,
               n_poly = n_poly)
}





createDBPointProxy = function(SpatVector,
                              remote_name = 'pnt_test',
                              db_path = ':temp:',
                              xy_col = c('x', 'y'),
                              extent = NULL,
                              overwrite = FALSE,
                              chunk_size = 10000L,
                              callback = NULL,
                              custom_table_fields = NULL,
                              ...) {
  db_path = getDBPath(db_path)
  backend_ID = calculate_backend_id(db_path)
  p = getBackendPool(backend_ID)
  if(inherits(SpatVector, 'tbl')) assert_in_backend(x = SpatVector, p = p)
  checkmate::assert_character(xy_col, len = 2L)
  checkmate::assert_character(remote_name, len = 1L)

  data = NULL
  if(inherits(SpatVector, 'tbl_Pool')) { # data is already in DB and tbl is provided
    data = SpatVector
  } else { # data must be read in

    # database input #
    overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)

    # setup fields
    if(is.character(custom_table_fields)) {
      custom_table_fields[['.uID']] = 'UINTEGER NOT NULL'
      custom_table_fields[['x']] = 'DOUBLE NOT NULL'
      custom_table_fields[['y']] = 'DOUBLE NOT NULL'
    }
    if(is.null(custom_table_fields)) custom_table_fields = c(
      .uID = 'UINTEGER NOT NULL',
      x = 'DOUBLE NOT NULL',
      y = 'DOUBLE NOT NULL'
    )

    # read data if needed #
    if(is.character(SpatVector)) {
      n_point = streamToDB_fread(
        path = SpatVector,
        backend_ID = backend_ID,
        remote_name = remote_name,
        idx_col = '.uID',
        pk = '.uID',
        nlines = chunk_size,
        callback = function(x) {
          x = callback(x)

          # setup reserved cols #
          # set idx_col name
          if('.uID' %in% names(x)) stopf(
            'colname \'.uID\' is reserved for dbPointProxy, but already exists'
          )
          # set xy_col
          if(xy_col[[1L]] != 'x' & 'x' %in% names(x)) stopf(
            'colname \'x\' is reserved for the coordinate info designated in param xy_col, but already exists'
          )
          if(xy_col[[1L]] != 'y' & 'y' %in% names(x)) stopf(
            'colname \'y\' is reserved for the coordinate info designated in param xy_col, but already exists'
          )
          data.table::setnames(x, old = c(xy_col[[1L]]), new = c('x'))
          data.table::setnames(x, old = c(xy_col[[2L]]), new = c('y'))
          data.table::setcolorder(x, c('x', 'y'))
        },
        custom_table_fields = custom_table_fields,
        ...
      )
    }
  }

  # create object #
  dbpoint = dbPointsProxy(
    n_point = n_point,
    data = data,
    hash = backend_ID,
    remote_name = remote_name
  )
}








# values ####
#' @rdname hidden_aliases
#' @importMethodsFrom terra values
#' @description
#' Get cell attributes from a dbSpatProxyData
#' Values are only returned as a \code{dbDataFrame} from dbSpatProxyData
#' @param x Spat* object
#' @param ... additional params to pass
#' @export
setMethod('values', signature(x = 'dbPolygonProxy'),
          function(x, ...) {
            x = reconnect(x)
            x@attributes
          })
#' @rdname hidden_aliases
#' @export
setMethod('values', signature(x = 'dbPointsProxy'),
          function(x, ...) {
            x = reconnect(x)
            x@data %>%
              dplyr::select(-c('x', 'y'))
          })








# extent_calculate ####
#' @name extent_calculate
#' @title Calculate dbSpatProxyData spatial extent
#' @param x a dbSpatProxyData object
#' @return terra SpatExtent
#' @family Extent processing functions
#' @keywords internal
setMethod('extent_calculate', signature(x = 'dbSpatProxyData'), function(x, ...) {
  xmin = dplyr::summarise(x@data, min(x, na.rm = TRUE)) %>% dplyr::pull()
  xmax = dplyr::summarise(x@data, max(x, na.rm = TRUE)) %>% dplyr::pull()
  ymin = dplyr::summarise(x@data, min(y, na.rm = TRUE)) %>% dplyr::pull()
  ymax = dplyr::summarise(x@data, max(y, na.rm = TRUE)) %>% dplyr::pull()
  terra::ext(xmin, xmax, ymin, ymax)
})







# simplify_poly_as_point ####
#' Generate a representative point for all the polygons in a dbPolygonProxy. This
#' facilitates spatial selection of the polygons as there are fewer points to keep
#' track of, making it less computationally intensive and less ambiguous whether
#' a polygon should be selected by a specific selection if parts of it fall in multiple
#' selections.

#' @name simplify_poly_as_point
#' @title Represent polygon as single xy coordinate pair stand-ins
#' @description
#' Internal.
#' Calculate simple representative spatial locations for polygons.
#' Depending on \code{method} param, finds the polygon vertex that is the mean or
#' at the lower left, lower right, upper left, or upper right. All methods are
#' roughly equivalent in speed.
#' Note that this is NOT the polygon centroid.
#' @param x tbl_Pool (usually from a dbPolygonProxy object)
#' @param method one of 'mean', 'lowL', 'lowR', 'upL', 'upR'
#' @param ... additional params to pass
#' @return tbl_Pool of xy coordinates
#' @keywords internal
simplify_poly_as_point = function(
    x,
    method = c('mean', 'lowL', 'lowR', 'upL', 'upR'),
    ...)
{
  checkmate::assert_class(x, 'tbl_Pool')
  checkmate::assert_subset(c('x', 'y'), names(x))
  method = match.arg(method, choices = c('mean', 'lowL', 'lowR', 'upL', 'upR'))

  y = switch(
    method,
   'lowL' = x %>%
     dplyr::group_by(geom) %>%
     dplyr::summarise(geom, x = min(x, na.rm = TRUE), y = min(y, na.rm = TRUE)),
   'lowR' = x %>%
     dplyr::group_by(geom) %>%
     dplyr::summarise(geom, x = max(x, na.rm = TRUE), y = min(y, na.rm = TRUE)),
   'upL' = x %>%
     dplyr::group_by(geom) %>%
     dplyr::summarise(geom, x = min(x, na.rm = TRUE), y = max(y, na.rm = TRUE)),
   'upR' = x %>%
     dplyr::group_by(geom) %>%
     dplyr::summarise(geom, x = max(x, na.rm = TRUE), y = max(y, na.rm = TRUE)),
   'mean' = x %>%
     dplyr::group_by(geom) %>%
     dplyr::summarise(geom, x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE))
  )

  y
}









# extent_filter ####
# Filter the data based on provided SpatExtent. Output is a filtered table.
# Depending on need, the original table or attributes table can be left joined
# to the output to finish the subset
#
#' @name extent_filter
#' @title Filter by terra SpatExtent
#' @description
#' Filter database-backed spatial data for only those records that fall within a
#' spatial \code{extent} as given by a terra \code{SpatExtent} object. This
#' selection
#' @param x dbSpatProxyData
#' @param extent SpatExtent defining a spatial region to select
#' @param method character. Method of selection. 'mean' (default) selects a polygon
#' if the mean point of all vertex coordinates falls within the \code{extent}.
#' 'all' selects a polygon if ANY of its vertices fall within the \code{extent}.
#' @param include logical vector of the form c(bottom, left, top, right) which
#' determines whether the specified extent bound should be inclusive of the bound
#' value itself. (ie greater/less than OR equal to (default) vs only greater/less than)
#' @param ... additional params to pass
#' @keywords internal
#' @family Extent processing functions
setMethod('extent_filter',
          signature(x = 'dbPolygonProxy', extent = 'SpatExtent', include = 'logical'),
          function(x, extent, include, method = c('all', 'mean'), ...) {
            method = match.arg(method, choices = c('all', 'mean'))

            x = filter_dbspat(x, by_geom = function(spdata) {
              if(method == 'mean') {
                # if method == mean, simplify poly to mean point
                spdata_select = simplify_poly_as_point(spdata,
                                                       method = 'mean',
                                                       dbPointsProxy = FALSE)
              } else {
                spdata_select = spdata
              }
              # pass to tbl_sql method to find polys within spatial subset
              spdata_select = extent_filter(spdata_select, extent, include, ...)
              spdata_select = dplyr::distinct(spdata_select, geom)

              # finalize spatial subset by selecting all vertices of requested
              # poly(s) and returning
              spdata_select %>%
                dplyr::left_join(spdata, by = 'geom', copy = FALSE)
            })

            # update extent
            # x@extent = extent_calculate(x)
            x
          })
#' @rdname extent_filter
#' @keywords internal
setMethod('extent_filter',
          signature(x = 'dbPointsProxy', extent = 'SpatExtent', include = 'logical'),
          function(x, extent, include, ...) {
            filter_dbspat(x, by_geom = function(spdata) {
              # pass to tbl_sql method to find points within spatial subset
              extent_filter(spdata, extent, include, ...)
            })
          })
#' @rdname extent_filter
#' @keywords internal
setMethod('extent_filter',
          signature(x = 'ANY', extent = 'SpatExtent', include = 'missing'),
          function(x, extent, ...) {

            # param include decides if values selections should be greater/less
            # than OR equal to the specified bound (include = TRUE, default) or
            # if they should the selection should only be greater or less than
            # (include = FALSE). Provide this input in the order of bottom,
            # left, top, right bounds as a logical vector.

            # pass to next method with 'include' defaults #
            extent_filter(x, extent, include = rep(TRUE, 4L))
          })

# tbl_sql method. Implemented with an assertion check due to difficulties with
# S4 dispatch onto an S3 class that is not the first one.
#' @rdname extent_filter
#' @keywords internal
setMethod('extent_filter',
          signature(x = 'ANY', extent = 'SpatExtent', include = 'logical'),
          function(x, extent, include, ...) {
            checkmate::assert_class(x, 'tbl_sql')
            checkmate::assert_logical(include, len = 4L)
            if(isTRUE(include[[2L]])) x = dplyr::filter(x, x >= !!as.numeric(extent[]['xmin']))
            else x = dplyr::filter(x, x > !!as.numeric(extent[]['xmin']))
            if(isTRUE(include[[4L]])) x = dplyr::filter(x, x <= !!as.numeric(extent[]['xmax']))
            else x = dplyr::filter(x, x < !!as.numeric(extent[]['xmax']))
            if(isTRUE(include[[1L]])) x = dplyr::filter(x, y >= !!as.numeric(extent[]['ymin']))
            else x = dplyr::filter(x, y > !!as.numeric(extent[]['ymin']))
            if(isTRUE(include[[3L]])) x = dplyr::filter(x, y <= !!as.numeric(extent[]['ymax']))
            else x = dplyr::filter(x, y < !!as.numeric(extent[]['ymax']))

            return(dplyr::collapse(x))
          })





# crop ####
#' @name hidden_aliases
#' @param x object to crop
#' @param y object to crop with
#' @importMethodsFrom terra crop
#' @return dbSpatProxyData
#' @export
setMethod('crop', signature(x = 'dbSpatProxyData', y = 'SpatExtent'),
          function(x, y, ...) {
            extent_filter(x, extent, ...)
          })






# filter_dbspat ####
#' @name filter_dbspat
#' @title Filter dbSpatProxyData
#' @description
#' Internal function to abstract away the differences in handling of filtering
#' data for dbPolygonProxy and dbPointsProxy. This is because the geometry and
#' values (attributes) are separated into two tables for dbPolygonProxy but are
#' present in a single table for dbPointsProxy. This function accepts
#' \code{dbSpatProxyData} as input, but the function passed to by_geom or by_value
#' should be defined for the internal \code{tbl_sql}.
#' @param x dbSpatProxyData
#' @param by_geom,by_value dplyr/dbplyr function to manipulate the data with
#' across either the geometry OR value data.
#' @examples
#' dbPoly <- simulate_dbPolygonProxy()
#' dbPoly_filtered <- filter_dbspat(x = dbpoly,
#'                                  by_value = function(dbspd) {
#'                                    dplyr::filter(dbspd, poly_ID == '101161259912191124732236989250178928032')
#'                                  })
#' dbPoly_filtered
#' dbPoly_filtered <- filter_dbspat(x = dbpoly,
#'                                  by_geom = function(dbspd) {
#'                                    dbspd %>% dplyr::filter(x > 6500)
#'                                  })
#' dbPoly_filtered
#' @keywords internal
setMethod( # FOR POLY GEOM
  'filter_dbspat',
  signature(x = 'dbPolygonProxy', by_geom = 'function', by_value = 'missing'),
  function(x, by_geom = NULL, by_value = NULL, ...) {

    # geometry and attribute information are in two separate tables for
    # dbPolygonProxy. Subsets and selections performed in one table must then
    # matched in the other through the use of a join that drops the unselected
    # records from the other table.

    x@data = by_geom(x@data)
    x@attributes@data = x@data %>%
      dplyr::left_join(x@attributes@data, by = 'geom', copy = FALSE) %>%
      dplyr::select(-dplyr::any_of(c('part', 'x', 'y', 'hole'))) %>%
      dplyr::distinct() %>%
      dplyr::collapse()
    return(x)
  })
setMethod( # FOR POLY VALUE
  'filter_dbspat',
  signature(x = 'dbPolygonProxy', by_geom = 'missing', by_value = 'function'),
  function(x, by_value, ...) {

    x@attributes@data = by_value(x@attributes@data)
    x@data = x@attributes@data %>%
      dplyr::left_join(x@data, by = 'geom', copy = FALSE) %>%
      dplyr::select(dplyr::all_of(c('geom', 'part', 'x', 'y', 'hole'))) %>%
      dplyr::collapse()
    return(x)
  })
setMethod( # FOR POINT GEOM
  'filter_dbspat',
  signature(x = 'dbPointsProxy', by_geom = 'function', by_value = 'missing'),
  function(x, by_geom, ...) {
    x@data = by_geom(x@data)
    return(x)
  })
setMethod( # FOR POINT VALUE
  'filter_dbspat',
  signature(x = 'dbPointsProxy', by_geom = 'missing', by_value = 'function'),
  function(x, by_value, ...) {
    x@data = by_value(x@data)
    return(x)
  })




# conversions ####

#' Convert a dbSpatProxyData object to a spatvector
#' @keywords internal
#' @return terra SpatVector
#' @noRd
setMethod('dbspat_to_sv', signature(x = 'dbPolygonProxy'), function(x, ...) {
  geom = x@data %>%
    dplyr::arrange(geom) %>%
    dplyr::collect()
  atts = x@attributes@data %>%
    dplyr::arrange(geom) %>%
    dplyr::select(-c('geom')) %>%
    dplyr::collect()

  terra::vect(as.matrix(geom),
              atts = atts,
              type = 'polygons')
})

#' @keywords internal
#' @noRd
setMethod('dbspat_to_sv', signature(x = 'dbPointsProxy'), function(x, ...) {
  DT = dplyr::collect(x@data)
  terra::vect(DT, geom = c('x', 'y'), keepgeom = FALSE)
})





# helper functions ####


#' @name svpoint_to_dt
#' @title SpatVector point to data.table
#' @param spatvector spatvector to use
#' @param include_values whether to include attribute values in output
#' @keywords internal
svpoint_to_dt = function (spatvector, include_values = TRUE)
{
  checkmate::assert_true(terra::geomtype(spatvector) == 'points')
  if (isTRUE(include_values)) {
    DT_values = cbind(terra::crds(spatvector), terra::values(spatvector)) %>%
      data.table::setDT()
  }
  else {
    DT_values = terra::crds(spatvector) %>% data.table::as.data.table()
  }
  return(DT_values)
}






# parallelization ####

#' EXPERIMENTAL
#' Starting from a terra SpatVector, distribute chunks to be processed to parallel
#' processes via future.lapply(). The results are then put back in the original
#' session and rbinded together into a new terra SpatVector
#' - All steps are in-memory
#' - Requires a lot of wrap() serialization
#' - much SLOWER than serial processing in a small dataset
#' @name sv_lapply_parallel
#' @title Apply a function on a SpatVector in parallel
#' @param ... additional params passed to future.lapply
#' @keywords internal
#' @noRd
sv_lapply_parallel = function(X,
                             FUN,
                             n_chunk = NULL,
                             n_per_chunk = 2e5,
                             ...) {
  checkmate::assert_class(X, 'SpatVector')
  checkmate::assert_function(FUN)
  checkmate::assert_numeric(n_per_chunk)
  if(is.null(n_chunk)) n_chunk = as.integer(nrow(X) / n_per_chunk)

  # generate stops indices
  stops_idx = chunk_ranges(start = 1, stop = nrow(X), n_chunks = n_chunk)

  # split SpatVector and wrap
  sv_wrap_list = lapply(stops_idx, function(chunk_range) {
    terra::wrap(X[chunk_range[1L]:chunk_range[2L]])
  })

  # parallelized function
  wrapped_out = future.apply::future_lapply(
    sv_wrap_list,
    future.seed = TRUE,
    function(passed_fun, x) {
      x = terra::vect(x)
      x = passed_fun(x)
      terra::wrap(x)
    },
    passed_fun = FUN,
    ...)

  # unwrap
  out = lapply(wrapped_out, function(sv_wrapped) {
    terra::vect(sv_wrapped)
  })

  # combine
  do.call(rbind, out)
}





#' EXPERIMENTAL
#' Chunk an operation by spatially tiling a dbSpatVectorProxy. The tiles are
#' pulled into memory in parallel processes where they are then run. Results
#' are then either returned
# dbspat_tile_lapply_parallel = function() {
#
# }


#' EXPERIMENTAL
#' Chunk an operation by splitting a









