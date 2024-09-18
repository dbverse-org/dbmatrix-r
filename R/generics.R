# dbData object interactions ####
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))

#' @importFrom MatrixGenerics colMeans colSums rowMeans rowSums colSds rowSds
#' @importFrom dplyr compute
NULL

.onLoad <- function(libname, pkgname) {
  if (!isGeneric("rownames")) methods::setGeneric("rownames")
  if (!isGeneric("rownames<-")) methods::setGeneric("rownames<-")
  if (!isGeneric("colnames")) methods::setGeneric("colnames")
  if (!isGeneric("colnames<-")) methods::setGeneric("colnames<-")
  if (!isGeneric("nrow")) methods::setGeneric("nrow")
  if (!isGeneric("ncol")) methods::setGeneric("ncol")
}

# dbMatrix ####
setGeneric('load', function(conn, name, class, ...) standardGeneric('load'))

# dbData ####
setGeneric("dbReconnect", function(x, ...) standardGeneric("dbReconnect"))
setGeneric("dbList", function(conn, ...) standardGeneric("dbList"))

# DBI ####
# setGeneric('dbDisconnect', function(x, ...) standardGeneric('dbDisconnect'))
# setGeneric('dbListTables', function(x, ...) standardGeneric('dbListTables'))

