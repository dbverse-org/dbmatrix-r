

# collate
#' @include dbDataFrame.R
NULL



# initialize ####
## dbSpatProxyData ####
setMethod('initialize', signature('dbSpatProxyData'), function(.Object, attributes, extent, ...) {

  # call dbData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # dbSpatProxyData specific data input #
  # ----------------------------------- #

  if(!missing(attributes)) .Object@attributes = attributes
  if(!missing(extent)) .Object@extent = extent

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
#' @details Information is only read into the database during this process. Based
#' on the \code{remote_name} and \code{db_path} a lazy connection is then made
#' downstream during dbData intialization and appended to the object. If the data
#' already exists within the database backend then it is entirely permissible to
#' omit the \code{SpatVector} param.
#'
createDBPolygonProxy = function(SpatVector,
                                remote_name = 'poly_test',
                                db_path = ':temp:',
                                attributes,
                                extent,
                                n_poly,
                                ...) {

}





createDBPointProxy = function(SpatVector) {

}







