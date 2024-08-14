# dbData ####

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

#' @title S4 virtual class for `dbMatrix`
#' @description
#' Representation of sparse and dense matrices in a database. Each object
#' is used as a connection to a single table that exists within the database.
#' Inherits from `dbData`.
#' @slot dim_names row [1] and col [2] names
#' @slot dims dimensions of the matrix
#' @noRd
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
#' @title S4 Class for `dbDenseMatrix`
#'
#' @description Representation of dense matrices using an on-disk database.
#' Inherits from \link{dbMatrix}.
#'
#' @noRd
#' @export
dbDenseMatrix = setClass(
  Class = "dbDenseMatrix",
  contains = "dbMatrix"
)

#### dbSparseMatrix ####
#' @title S4 Class for dbSparseMatrix
#'
#' @description Representation of sparse matrices using an on-disk database.
#' Inherits from \link{dbMatrix.}
#' @noRd
#' @export
dbSparseMatrix = setClass(
  Class = "dbSparseMatrix",
  contains = "dbMatrix"
)

## dbIndex ####
#' @title S4 virtual class - Simple Class for dbData indices
#' @description
#' This is a virtual class used for indices (in signatures) for indexing
#' and sub-assignment of 'dbData' objects. Simple class union of 'logical',
#' 'numeric', 'integer', and  'character'.
#' Based on the 'index' class implemented in \pkg{Matrix}
#' @keywords internal
#' @noRd
setClassUnion(name = 'dbIndex',
              members = c('logical', 'numeric', 'integer', 'character'))
