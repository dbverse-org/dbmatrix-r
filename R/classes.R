# dbData ####
# Import dbData class from dbProject package instead of redefining it
#' @importClassesFrom dbProject dbData

### dbMatrix ####

#' @title S4 virtual class for dbMatrix
#' @description
#' Representation of sparse and dense matrices in a database. Each object
#' is used as a connection to a single table that exists within the database.
#' Inherits from `dbData`.
#' @slot dim_names row (1) and col (2) names
#' @slot dims dimensions of the matrix
#' @slot init logical. Whether the object is fully initialized
#' @rdname dbMatrix
#' @aliases dbMatrix-class
#' @exportClass dbMatrix
#' @keywords internal
dbMatrix <- setClass(
  Class = 'dbMatrix',
  contains = c('dbData', 'VIRTUAL'),
  slots = list(
    dim_names = 'list',
    dims = 'integer',
    init = 'logical'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dims = c(NA_integer_, NA_integer_),
    init = FALSE
  )
)

#' Ensure dim_names are non-NULL factors
#' 
#' Helper function that ensures dim_names are always non-NULL factors.
#' If dim_names are NULL, creates "row1", "row2", ... and "col1", "col2", ...
#' If dim_names are character, converts to factor for efficient indexing.
#' 
#' @param dim_names list of row and column names (can be NULL)
#' @param dims integer vector of dimensions
#' @return list of factor row and column names
#' @keywords internal
#' @noRd
.ensure_dim_names <- function(dim_names, dims) {
  # Ensure dim_names list exists

  if (is.null(dim_names)) {
    dim_names <- list(NULL, NULL)
  }
  
  # Ensure row names
  if (is.null(dim_names[[1]])) {
    dim_names[[1]] <- as.factor(paste0("row", seq_len(dims[1])))
  } else if (!is.factor(dim_names[[1]])) {
    dim_names[[1]] <- as.factor(dim_names[[1]])
  }
  
  # Ensure col names
  if (is.null(dim_names[[2]])) {
    dim_names[[2]] <- as.factor(paste0("col", seq_len(dims[2])))
  } else if (!is.factor(dim_names[[2]])) {
    dim_names[[2]] <- as.factor(dim_names[[2]])
  }
  
  dim_names
}

#### dbDenseMatrix ####
#' @title S4 Class for `dbDenseMatrix`
#'
#' @description Representation of dense matrices using an on-disk database.
#' Inherits from \link{dbMatrix}.
#'
#' @name dbDenseMatrix-class
#' @export
dbDenseMatrix <- setClass(
  Class = "dbDenseMatrix",
  contains = "dbMatrix"
)

#### dbSparseMatrix ####
#' @title S4 Class for dbSparseMatrix
#'
#' @description Representation of sparse matrices using an on-disk database.
#' Inherits from \link{dbMatrix}.
#' @name dbSparseMatrix-class
#' @export
dbSparseMatrix <- setClass(
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
setClassUnion(
  name = 'dbIndex',
  members = c('logical', 'numeric', 'integer', 'character')
)
