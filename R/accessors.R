# internal functions ####
#' get_tblName
#'
#' @param dbData
#'
#' @keywords internal
get_tblName <- function(dbData){
  # check if dbData is a dbData object
  if(!inherits(dbData, "dbData")){
    stopf("dbData must be a dbData object")
  }

  tblName = dbData@name

  return(tblName)
}

#' get_dbdir
#'
#' @param dbData
#'
#' @keywords internal
#' string of the path to the database directory
get_dbdir <- function(dbData){
  # check if dbData is a dbData object
  if(!inherits(dbData, "dbData")){
    stopf("dbData must be a dbData object")
  }

  dbdir = dbData@con@driver@dbdir

  return(dbdir)
}

