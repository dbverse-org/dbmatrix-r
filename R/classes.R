

#' @importClassesFrom terra SpatExtent
#' @importFrom methods as callNextMethod initialize new slot slot<- validObject
#' @importFrom utils capture.output
#' @keywords internal
NULL






# Virtual parent non-spatial classes ####


## GiottoDB ####
# Overarching package class
#' @noRd
setClass('GiottoDB', contains = 'VIRTUAL')



## dbData ####

#' @name dbData
#' @title dbData
#' @description Framework for objects that link to the database backend
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @slot init logical. Whether the object is fully initialized
#' @noRd
setClass('dbData',
         contains = c('GiottoDB',
                      'VIRTUAL'),
         slots = list(
           data = 'ANY',
           hash = 'character',
           remote_name = 'character',
           init = 'logical'
         ),
         prototype = list(
           data = NULL,
           hash = NA_character_,
           remote_name = NA_character_,
           init = FALSE
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
         contains = c('GiottoDB'),
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

  cat('backend_ID : ', object@hash, '\n')
  cat('name       : \'', object@remote_name, '\'\n', sep = '')

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
#' @slot key column to set as key for ordering and subsetting on i
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  contains = 'dbData',
  slots = list(
    key = 'character'
  ),
  prototype = list(
    key = NA_character_
  )
)

setMethod('show', signature('dbDataFrame'), function(object) {
  print_dbDataFrame(object, 6)
})

setMethod('print', signature('dbDataFrame'), function(x, n = 6, ...) {
  print_dbDataFrame(x, n, ...)
})

print_dbDataFrame = function(x, n, ...) {
  df_dim = dim(x@data)
  dfp = capture.output(print(x@data, n = n, na.print = NULL, ...))
  nr = df_dim[1L]
  nc = df_dim[2L]
  nr = as.character(ifelse(is.na(nr), '??', nr))
  db = gsub('# Database:', 'database   :', dfp[2L])

  cat('An object of class \'', class(x), '\'\n', sep = '')
  cat('backend_ID : ', x@hash, '\n', sep = '')
  cat('name       : \'', x@remote_name, '\' [', nr,' x ', nc,']', '\n', sep = '')
  cat(db, '\n\n')
  writeLines(dfp[-(1:2)])
}



# Virtual parent spatial classes ####

## dbSpatProxyData ####
#' @name dbSpatProxyData
#' @title dbSpatProxyData
#' @description Framework for terra SpatVector database backend proxy objects
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @slot extent spatial extent
#' @slot poly_filter polygon SpatVector that is used to filter values on read-in
#' @noRd
setClass('dbSpatProxyData',
         contains = c('dbData', 'VIRTUAL'),
         slots = list(
           extent = 'SpatExtent',
           poly_filter = 'ANY'
         ),
         prototype = list(
           extent = terra::ext(0, 0, 0 ,0),
           poly_filter = NULL
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
#' @slot poly_filter polygon SpatVector that is used to filter values on read-in
#' @export
dbPolygonProxy = setClass(
  'dbPolygonProxy',
  contains = 'dbSpatProxyData',
  slots = list(
    attributes = 'dbDataFrame',
    n_poly = 'numeric'
  ),
  prototype = list(
    n_poly = NA_integer_
  )
)



setMethod('show', signature(object = 'dbPolygonProxy'), function(object) {
  print(object)
})

setMethod('print', signature(x = 'dbPolygonProxy'), function(x, n = 3, ...) {
  print_dbPolygonProxy(x = x, n = n, ...)
})


print_dbPolygonProxy = function(x, n, ...) {
  refresh = getOption('gdb.update_show', FALSE)
  p = capture.output(print(dplyr::select(x@attributes@data, -c('geom')), n = n,
                           na.print = NULL, max_footer_lines = 0L,
                           width = getOption('width') - 12L, ...))
  db = gsub('# Database:', 'database   :', p[2L])
  vs = paste0('\nvalues     : ', p[3L], '\n')
  indent = '            '
  # update values
  if(refresh) ex = extent_calculate(x)
  else ex = '??'
  ex = paste0(
    paste(ex[], collapse = ', '), ' (',
    paste(c('xmin', 'xmax', 'ymin', 'ymax'), collapse = ', '), ')'
  )
  ds = if(refresh) paste(dim(x), collapse = ', ')
  else paste('??', ncol(x), sep = ', ')

  cat('An object of class \'', class(x), '\'\n', sep = '')
  cat('backend    : ', x@hash, '\n', sep = '')
  cat('table      : \'', x@remote_name, '\'\n', sep = '')
  cat(db, '\n')
  cat('dimensions :', ds, ' (points, attributes)\n')
  cat('extent     : ', ex, sep = '')
  cat(vs)
  writeLines(paste(indent, p[-(1:3)])) # skip first header lines
  if(!refresh) cat('\n# set options(gdb.update_show = TRUE) to calculate ??')
}





## dbPointsProxy ####
#' @title S4 dbPointsProxy class
#' @description
#' Representation of point information using an on-disk database. Intended to
#' be used to store information that can be pulled into terra point SpatVectors
#' @slot n_points number of points
#' @slot feat_ID feature IDs
#' @slot extent extent of points
#' @slot poly_filter polygon SpatVector that is used to filter values on read-in
#' @importClassesFrom terra SpatExtent
#' @export
dbPointsProxy = setClass(
  'dbPointsProxy',
  contains = 'dbSpatProxyData',
  slots = list(
    n_point = 'numeric'
  ),
  prototype = list(
    n_point = NA_integer_
  )
)



setMethod('show', signature(object = 'dbPointsProxy'), function(object) {
  print(object, n = 3)
})

setMethod('print', signature(x = 'dbPointsProxy'), function(x, n = 3, ...) {
  print_dbPointsProxy(x = x, n = n, ...)
})

print_dbPointsProxy = function(x, n, ...) {
  refresh = getOption('gdb.update_show', FALSE)
  p = capture.output(print(dplyr::select(x@data, -c('x', 'y')), n = n,
                           na.print = NULL, max_footer_lines = 0L,
                           width = getOption('width') - 12L, ...))
  db = gsub('# Database:', 'database   :', p[2L])
  vs = paste0('\nvalues     : ', p[3L], '\n')
  indent = '            '
  # update values
  if(refresh) ex = extent_calculate(x)
  else ex = '??'
  ex = paste0(
    paste(ex[], collapse = ', '), ' (',
    paste(c('xmin', 'xmax', 'ymin', 'ymax'), collapse = ', '), ')'
  )
  ds = if(refresh) paste(dim(x), collapse = ', ')
  else paste('??', ncol(x), sep = ', ')


  cat('An object of class \'', class(x), '\'\n', sep = '')
  cat('backend    : ', x@hash, '\n', sep = '')
  cat('table      : \'', x@remote_name, '\'\n', sep = '')
  cat(db, '\n')
  cat('dimensions :', ds, ' (points, attributes)\n')
  cat('extent     : ', ex, sep = '')
  cat(vs)
  writeLines(paste(indent, p[-(1:3)])) # skip first header lines
  if(!refresh) cat('\n# set options(gdb.update_show = TRUE) to calculate ??')
}











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







