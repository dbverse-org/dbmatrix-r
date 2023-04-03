

# dbData Class ####

#' @name dbData
#' @title dbData
#' @description Framework for objects that link to a database
#' @slot driver driver function for the database
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot dim_names row [1] and col [2] names
#' @slot dim dimensions of the matrix
#' @slot valid whether the object is initialized for use
#' @noRd
setClass('dbData',
         contains = 'VIRTUAL',
         slots = list(
           driver = 'ANY',
           connection = 'ANY',
           path = 'character',
           table_name = 'character',
           valid = 'logical'
         ),
         prototype = list(
           driver = NULL,
           connection = NULL,
           path = NA_character_,
           table_name = NA_character_,
           valid = FALSE
         ))




# dbMatrix Class ####

#' @noRd
check_dbMatrix = function(object) {
  errors = character()

  # Start checking only after valid flag is TRUE
  if(!isTRUE(object@valid)) {
    return(TRUE)
  }

  if(is.na(object@table_name)) {
    msg = 'Name of table within DB to link to must be supplied\n
    use tblName()<- to set\n'
    errors = c(errors, msg)
  }

  if(is.null(object@connection)) {
    msg = 'DB connection object must be supplied\n
    use con()<- to set\n'
    error = c(errors, msg)
  }

  if(length(errors) == 0) TRUE else errors
}




#' @title S4 dbMatrix class
#' @description
#' Representation of triplet matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot driver driver function for the database
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot dim_names row [1] and col [2] names
#' @slot dim dimensions of the matrix
#' @slot valid whether the object is initialized for use
#' @export
dbMatrix = setClass(
  'dbMatrix',
  contains = 'dbData',
  slots = list(
    dim_names = 'list',
    dim = 'integer'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dim = c(NA_integer_, NA_integer_)
  ),
  validity = check_dbMatrix
)







# dbDataFrame Class ####


#' @title S4 dbDataFrame class
#' @description
#' Representation of dataframes using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot driver driver function for the database
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot valid whether the object is initialized for use
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  contains = 'dbData'
)
