# dbData ####

#' @name dbData
#' @title dbData
#' @description Base class for all db objects
#' @slot value dplyr tbl that represents the database data
#' @slot name name of table within database that contains the data
#' @slot init logical. Whether the object is fully initialized
#' @noRd
setClass(
  Class = 'dbData',
  contains = c('VIRTUAL'),
  slots = list(
    value = 'ANY',
    name = 'character',
    init = 'logical'
  ),
  prototype = list(
    value = NULL,
    name = NA_character_,
    init = FALSE
  )
)

### dbMatrix ####

#' @title S4 dbMatrix class
#' @description
#' Representation of sparse matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot dim_names row [1] and col [2] names
#' @slot dims dimensions of the matrix
#' @export
dbMatrix = setClass(
  Class = 'dbMatrix',
  contains = c('dbData', 'VIRTUAL'),
  slots = list(
    dim_names = 'list',
    dims = 'integer'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dims = c(NA_integer_, NA_integer_)
  )
)

#### dbDenseMatrix ####
#' @title S4 Class for dbDenseMatrix
#'
#' @description Representation of dense matrices using an on-disk database.
#' Inherits from dbMatrix.
#'
#' @slot data A dense ijx dataframe/tibble
#' @export
dbDenseMatrix = setClass(
  Class = "dbDenseMatrix",
  contains = "dbMatrix"
)

#### dbSparseMatrix ####
#' @title S4 Class for dbSparseMatrix
#'
#' @description Representation of sparse matrices using an on-disk database.
#' Inherits from dbMatrix.
#'
#' @export
dbSparseMatrix = setClass(
  Class = "dbSparseMatrix",
  contains = "dbMatrix"
)

### dbDataFrame ####


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
  Class = 'dbDataFrame',
  contains = 'dbData',
  slots = list(
    key = 'character'
  ),
  prototype = list(
    key = NA_character_
  )
)

## dgbIndex ####
#' @title Virtual Class "gdbIndex" - Simple Class for dbData indices
#' @name gdbIndex
#' @description
#' This is a virtual class used for indices (in signatures) for indexing
#' and sub-assignment of 'dbData' objects. Simple class union of 'logical',
#' 'numeric', 'integer', and  'character'.
#' Based on the 'index' class implemented in \pkg{Matrix}
#' @keywords internal
#' @noRd
setClassUnion(name = 'gdbIndex',
              members = c('logical', 'numeric', 'integer', 'character'))

#' @title Virtual Class "gdbIndexNonChar" - Simple Class for dbData indices
#' @name gdbIndex
#' @description
#' This is a virtual class used for indices (in signatures) for indexing
#' and sub-assignment of 'dbData' objects. Simple class union of 'logical' and
#' 'numeric'.
#' Based on the 'index' class implemented in \pkg{Matrix}
#' @keywords internal
#' @noRd
setClassUnion(name = 'gdbIndexNonChar',
              members = c('logical', 'numeric'))

## dbMF ####
#' @title Virtual Class "dbMFData" - Simple class for dbMatrix and dbDF
#' @name dbMFData
#' @description
#' This is a virtual class used to refer to dbMatrix and dbDataFrame objects as
#' a single signature.
#' @keywords internal
#' @noRd
setClassUnion(name = 'dbMFData',
              members = c('dbMatrix', 'dbDataFrame'))

