

#' @importClassesFrom terra SpatExtent
#' @keywords internal
NULL






# Virtual parent non-spatial classes ####

## dbData ####

#' @name dbData
#' @title dbData
#' @description Framework for objects that link to the database backend
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @noRd
setClass('dbData',
         contains = 'VIRTUAL',
         slots = list(
           data = 'ANY',
           hash = 'character',
           remote_name = 'character'
         ),
         prototype = list(
           data = NULL,
           hash = NA_character_,
           remote_name = NA_character_
         ))












# Backend specific ####

## backendInfo ####
#' @name backendInfo
#' @title backendInfo
#' @description
#' Simple S4 class to contain information about the database backend and
#' regenerate connection pools. A hash is generated from the db_path to act
#' as a unique identifier for each backend.
#' @slot driver_call DB driver call stored as a string
#' @slot db_path path to database
#' @slot hash xxhash64 hash of the db_path
#' @export
setClass('backendInfo',
         slots = list(
           driver_call = 'character',
           db_path = 'character',
           hash = 'character'
         ),
         prototype = list(
           driver_call = NA_character_,
           db_path = NA_character_,
           hash = NA_character_
         ))






# Data Container Classes ####

## dbMatrix ####


#' @title S4 dbMatrix class
#' @description
#' Representation of triplet matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @slot path path to database on-disk file
#' @slot dim_names row [1] and col [2] names
#' @slot dims dimensions of the matrix
#' @export
dbMatrix = setClass(
  'dbMatrix',
  contains = 'dbData',
  slots = list(
    dim_names = 'list',
    dims = 'integer'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dims = c(NA_integer_, NA_integer_)
  )
)





setMethod('show', signature(object = 'dbMatrix'), function(object) {

  dimn = slot(object, 'dim_names')
  rown = dimn[[1]]
  coln = dimn[[2]]

  # print class and dims #
  # -------------------- #

  if(identical(object@dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbMatrix"\n')
    return() # exit early if no info
  } else {
    cat(object@dims[[1]], 'x', object@dims[[2]], ' matrix of class "dbMatrix"\n')
  }


  # preview print #
  # ------------- #

  # print colnames
  colname_show_n = object@dims[[2]] - 6L
  if(colname_show_n < 0L) {
    message('Colnames: ', vector_to_string(coln))
  } else if(colname_show_n >= 1L) {
    message(
      '[[ Colnames ',
      vector_to_string(head(coln, 3L)),
      ' ... suppressing ', colname_show_n, ' ...',
      vector_to_string(tail(coln, 3L)),
      ' ]]'
    )
  }

  # get matrix i and j to print
  p_coln = head(coln, 10L)
  if(object@dims[[1L]] - 6L > 0L) {
    p_rown = c(head(rown, 3L), tail(rown, 3L))
  } else {
    p_rown = rown
  }

  # prepare subset to print
  conn = cPool(object) %>% pool::poolCheckout() # conn needed for compute()
  on.exit(pool::poolReturn(conn))

  preview = object
  cPool(preview) = conn # set conn as src
  preview_dt = preview@data %>%
    dplyr::filter(i %in% p_rown & j %in% p_coln) %>%
    dplyr::arrange(i, j) %>%
    dplyr::compute() %>%
    tidyr::pivot_wider(names_from = 'j', values_from = 'x') %>%
    data.table::as.data.table()
  data.table::setkeyv(preview_dt, names(preview_dt)[1L]) # enforce ordering
  colnames(preview_dt) = NULL

  if(nrow(preview_dt < 7L)) {
    print(preview_dt, digits = 5L, row.names = 'none')
  } else {
    print(preview_dt[1:3,], digits = 5L, row.names = 'none')

    sprintf(' ........suppressing %d columns and %d rows\n',
            object@dims[[2L]] - 10L, object@dims[[1L]] - 6L)

    print(preview_dt[4:6,], digits = 5L, row.names = 'none')
  }

  cat('\n')

})






## dbDataFrame ####


#' @title S4 dbDataFrame class
#' @description
#' Representation of dataframes using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  contains = 'dbData'
)







# Virtual parent spatial classes ####

## dbSpatProxyData ####
#' @name dbSpatProxyData
#' @title dbSpatProxyData
#' @description Framework for terra SpatVector database backend proxy objects
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @slot attributes dbDataFrame of attributes information
#' @slot extent spatial extent
#' @noRd
setClass('dbSpatProxyData',
         contains = c('dbData', 'VIRTUAL'),
         slots = list(
           attributes = 'dbDataFrame',
           extent = 'SpatExtent'
         ),
         prototype = list(
           extent = terra::ext(0,0,0,0)
         ))

# Spatial Data Container Classes ####

## dbPolygonProxy ####
#' @title S4 dbPolygonProxy class
#' @description
#' Representation of polygon information using an on-disk database. Intended to
#' be used to store information that can be pulled into terra polygon SpatVectors
#' @slot data lazy table containing geometry information with columns geom, part,
#' x, y, and hole
#' @slot attributes dbDataFrame of attributes information, one of which (usually
#' the first) being 'ID' that can be joined/matched against the 'geom' values in
#' \code{attributes}
#' @slot n_poly number of polygons
#' @slot poly_ID polygon IDs
#' @slot extent extent of polygons
#' @export
dbPolygonProxy = setClass(
  'dbPolygonProxy',
  contains = 'dbSpatProxyData',
  slots = list(
    n_poly = 'numeric',
    poly_ID = 'character'
  ),
  prototype = list(
    n_poly = NA_integer_,
    poly_ID = NA_character_
  )
)



setMethod('show', signature(object = 'dbPolygonProxy'), function(object) {
  cat('An object of class "', class(object), '"\n', sep = '')
  cat('dimensions : ', paste(object@n_poly, ncol(object@attributes), collapse = ', '),
      ' (geometries, attributes)\n')
  cat('extent     : ', paste(object@extent[], collapse = ', '),
      ' (', paste(names(object@extent[]), collapse = ', '), ')',
      sep = '')
})



## dbPointsProxy ####
#' @title S4 dbPointsProxy class
#' @description
#' Representation of point information using an on-disk database. Intended to
#' be used to store information that can be pulled into terra point SpatVectors
#' @slot attributes dbDataFrame of attributes information
#' @slot n_points number of points
#' @slot feat_ID feature IDs
#' @slot extent extent of points
#' @importClassesFrom terra SpatExtent
#' @export
dbPointsProxy = setClass(
  'dbPointsProxy',
  contains = 'dbSpatProxyData',
  slots = list(
    n_point = 'numeric',
    feat_ID = 'character'
  ),
  prototype = list(
    n_point = NA_integer_,
    feat_ID = NA_character_
  )
)



setMethod('show', signature(object = 'dbPointsProxy'), function(object) {
  cat('An object of class "', class(object), '"\n', sep = '')
  cat('dimensions : ', paste(object@n_point, ncol(object@attributes), collapse = ', '),
      ' (points, attributes)\n')
  cat('extent     : ', paste(object@extent[], collapse = ', '),
      ' (', paste(names(object@extent[]), collapse = ', '), ')',
      sep = '')
})














# Virtual Class Unions ####

## dgbIndex ####
#' @title Virtual Class "gdbIndex" - Simple Class for GiottoDB indices
#' @name gdbIndex
#' @description
#' This is a virtual class used for indices (in signatures) for indexing
#' and sub-assignment of 'GiottoDB' objects. Simple class union of 'logical',
#' 'numeric', 'integer', and  'character'.
#' Based on the 'index' class implemented in \pkg{Matrix}
#' @keywords internal
#' @noRd
setClassUnion('gdbIndex',
              members = c('logical', 'numeric', 'integer', 'character'))

## dbMF ####
#' @title Virtual Class "dbMFData" - Simple class for GiottoDB matrix and dataframes
#' @name dbMFData
#' @description
#' This is a virtual class used to refer to dbMatrix and dbDataFrame objects as
#' a single signature.
#' @keywords internal
#' @noRd
setClassUnion('dbMFData',
              members = c('dbMatrix', 'dbDataFrame'))







