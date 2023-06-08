
# Package level environment for holding connection details
.DB_ENV = new.env()


#' @keywords internal
#' @noRd
.onLoad = function(libname, pkgname) {
  # initialize the environment within the package namespace
  assign(".DB_ENV", new.env(), envir = asNamespace(pkgname))

  # setup result ID counter
  options(gdb.res_count = 1L)

  reg.finalizer(
    e = .DB_ENV,
    f = closeBackend,
    onexit = TRUE
  )
}


#' @keywords internal
#' @noRd
.onUnload = function(libpath) {
  closeBackend()
}

