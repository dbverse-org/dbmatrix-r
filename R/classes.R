


#' @title S4 dbMatrix class
#' @description
#' Representation of triplet matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot dim_names row [1] and col [2] names
#' @slot dim dimensions of the matrix
#' @slot valid whether the object is initialized for use
#' @export
dbMatrix = setClass(
  'dbMatrix',
  slots = list(
    connection = 'ANY',
    path = 'character',
    table_name = 'character',
    dim_names = 'list',
    dim = 'integer',
    valid = 'logical'
  ),
  prototype = list(
    connection = NULL,
    path = NA_character_,
    table_name = NA_character_,
    dim_names = list(NULL, NULL),
    dim = c(NA_integer_, NA_integer_),
    valid = FALSE
  )
)




#' @title S4 dbDataFrame class
#' @description
#' Representation of dataframes using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot valid whether the object is initialized for use
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  slots = list(
    connection = 'ANY',
    path = 'character',
    table_name = 'character',
    valid = 'logical'
  ),
  prototype = list(
    connection = NULL,
    path = NA_character_,
    table_name = NA_character_,
    valid = FALSE
  )
)
