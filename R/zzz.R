
# Package level environment for holding connection details
.DB_ENV = new.env()


#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # initialize the environment within the package namespace
  assign(".DB_ENV", new.env(), envir = asNamespace(pkgname))
}

