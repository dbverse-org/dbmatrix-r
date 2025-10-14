# dbData object interactions ####
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))

#' @importFrom MatrixGenerics colMeans colSums rowMeans rowSums colSds rowSds rowVars colVars
#' @importFrom dplyr compute
NULL

.onLoad <- function(libname, pkgname) {
  if (!isGeneric("rownames")) {
    methods::setGeneric("rownames")
  }
  if (!isGeneric("rownames<-")) {
    methods::setGeneric("rownames<-")
  }
  if (!isGeneric("colnames")) {
    methods::setGeneric("colnames")
  }
  if (!isGeneric("colnames<-")) {
    methods::setGeneric("colnames<-")
  }
  if (!isGeneric("nrow")) {
    methods::setGeneric("nrow")
  }
  if (!isGeneric("ncol")) {
    methods::setGeneric("ncol")
  }
  if (!isGeneric("%in%")) methods::setGeneric("%in%")
}

# dbMatrix ####
setGeneric('load', function(conn, name, class, ...) standardGeneric('load'))

# dbData ####
setGeneric("dbList", function(conn, ...) standardGeneric("dbList"))

#' @importFrom dbProject to_view conn conn<- dbReconnect
NULL
