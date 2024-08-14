# dbData object interactions ####
setGeneric('colTypes', function(x, ...) standardGeneric('colTypes'))
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))

#' @importFrom MatrixGenerics colMeans colSums rowMeans rowSums colSds rowSds
NULL

.onLoad <- function(libname, pkgname) {
  if (!isGeneric("rownames")) methods::setGeneric("rownames")
  if (!isGeneric("rownames<-")) methods::setGeneric("rownames<-")
  if (!isGeneric("colnames")) methods::setGeneric("colnames")
  if (!isGeneric("colnames<-")) methods::setGeneric("colnames<-")
  if (!isGeneric("nrow")) methods::setGeneric("nrow")
  if (!isGeneric("ncol")) methods::setGeneric("ncol")
}

# dbMatrix specific ####
# setGeneric('colSds', function(x, ...) standardGeneric('colSds'))
# setGeneric('colMeans', function(x, ...) standardGeneric('colMeans'))
# setGeneric('colSums', function(x, ...) standardGeneric('colSums'))
# setGeneric('rowSds', function(x, ...) standardGeneric('rowSds'))
# setGeneric('rowMeans', function(x, ...) standardGeneric('rowMeans'))
# setGeneric('rowSums', function(x, ...) standardGeneric('rowSums'))

# dbData ops ####
# setGeneric('t', function(x, ...) standardGeneric('t'))
# setGeneric('mean', function(x, ...) standardGeneric('mean'))

# DBI ####
# setGeneric('dbDisconnect', function(x, ...) standardGeneric('dbDisconnect'))
# setGeneric('dbListTables', function(x, ...) standardGeneric('dbListTables'))
