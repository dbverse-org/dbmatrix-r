

# collate
#' @include dbDataFrame.R
NULL



# initialize ####
## dbSpatProxyData ####
setMethod('initialize', signature('dbSpatProxyData'), function(.Object, attributes, extent, filter, ...) {

  # call dbData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbSpatProxyData specific data input #
  # ----------------------------------- #
  if(!missing(attributes)) .Object@attributes = attributes
  if(!missing(extent)) .Object@extent = extent
  if(!missing(filter)) .Object@filter = filter

  # default values if no input provided #
  # ----------------------------------- #
  if(!is.null(.Object@data)) .Object@extent = calculate_extent(.Object@data)


  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})





## dbPolygonProxy ####
setMethod('initialize', signature('dbPolygonProxy'), function(.Object, n_poly, ...) {

  # call dbSpatProxyData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbPolygonProxy specific data input #
  # ---------------------------------- #
  if(!missing(n_poly)) .Object@n_poly = n_poly

  # default values if no input provided #
  # ----------------------------------- #
  if(!is.null(.Object@attributes)) .Object@n_poly = nrow(.Object@attributes)

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})





## dbPointProxy ####
setMethod('initialize', signature('dbPointsProxy'), function(.Object, n_point, ...) {

  # call dbSpatProxyData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbPolygonProxy specific data input #
  # ---------------------------------- #
  if(!missing(n_point)) .Object@n_point = n_point

  # default values if no input provided #
  # ----------------------------------- #
  if(!is.null(.Object@attributes)) .Object@n_point = nrow(.Object@attributes)

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
# READ METHOD
# [character]
# SpatVectorProxy connection will be generated
# if more than 10,000 lines then a chunked approach with SpatVectorProxy will be used

#' @name createDBPolygonProxy
#' @title Create a framework for chunked processing with terra polygon SpatVectors
#' @description
#' Create an S4 dbPolygonProxy object that is composed of two database tables.
#' One table hold geometry information while the other holds attribute information.
#' @param SpatVector object coercible to SpatVector or filepath to spatial data
#'
#' @details Information is only read into the database during this process. Based
#' on the \code{remote_name} and \code{db_path} a lazy connection is then made
#' downstream during dbData intialization and appended to the object.
#' If a dplyr tbl is provided as pre-made input then it is evaluated for whether
#' it is a \code{tbl_Pool} and whether the table exists within the specified
#' backend then directly passed downstream.
#' @export
createDBPolygonProxy = function(SpatVector,
                                remote_name = 'poly_test',
                                db_path = ':temp:',
                                overwrite = FALSE,
                                read_function = NULL,
                                nlines = 10000L,
                                extent,
                                filter,
                                ...) {
  db_path = getDBPath(db_path)
  backend_ID = calculate_backend_id(db_path)
  p = getBackendPool(hash)
  if(inherits(df, 'tbl')) assert_in_backend(x = df, p = p)


  data = evaluate_dbPolygonProxy()

}





createDBPointProxy = function(SpatVector) {

}





# helper functions ####


# Calculate SpatExtent from a tbl_Pool with x and y values
calculate_extent = function(x) {
  assert_conn_table(x)
  stopifnot(all(c('x', 'y') %in% colnames(x)))
  terra::ext(c(
    xmin = x %>% dplyr::summarise(min(x)) %>% dplyr::pull(),
    xmax = x %>% dplyr::summarise(max(x)) %>% dplyr::pull(),
    ymin = x %>% dplyr::summarise(min(y)) %>% dplyr::pull(),
    ymax = x %>% dplyr::summarise(max(y)) %>% dplyr::pull()
  ))
}


# TODO
evaluate_dbPolygonProxy = function(input,
                                   p,
                                   backend_ID,
                                   remote_name = 'poly_test',
                                   db_path = ':temp:',
                                   overwrite = FALSE,
                                   read_function = NULL,
                                   nlines = 10000L,
                                   extent,
                                   filter,
                                   ...) {
  data = NULL
  if(inherits(matrix, 'tbl_Pool')) { # data is already in DB and tbl is provided
    data = matrix
  } else { # data must be read in

    # database input #
    overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)

    # read matrix if needed
    if(is.character(matrix)) {
      fstreamToDB(path = input,
                  backend_ID = backend_ID,
                  remote_name = remote_name,
                  # indices = c('i', 'j'),
                  nlines = nlines,
                  with_pk = FALSE,
                  cores = cores,
                  callback = callback,
                  overwrite = overwrite)
    }
  }
  data
}









# append














