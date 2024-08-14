# internal functions ####
#' get_tblName
#'
#' @param dbMatrix
#'
#' @keywords internal
get_tblName <- function(dbMatrix){
  # check if dbData is a dbData object
  if(!inherits(dbMatrix, "dbData")){
    stopf("dbMatrix must be a dbData object")
  }

  tblName = dbMatrix@name

  return(tblName)
}

#' get_dbdir
#'
#' @param dbMatrix
#'
#' @keywords internal
#' string of the path to the database directory
get_dbdir <- function(dbMatrix){
  # check if dbData is a dbData object
  if(!inherits(dbMatrix, "dbData")){
    stopf("dbMatrix must be a dbData object")
  }

  dbdir = dbMatrix@value[[1]]$con@driver@dbdir

  return(dbdir)
}


#' get_con
#'
#' @param dbMatrix
#'
#' @keywords internal
#'
get_con <- function(dbMatrix){
  # check if dbData is a dbData object
  if(!inherits(dbMatrix, "dbData")){
    stopf("dbMatrix must be a dbData object")
  }

  con = dbMatrix@value[[1]]$con

  return(con)
}
