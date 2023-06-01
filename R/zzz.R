
# Package level environment for holding connection details
.DB_ENV = new.env()


#' @keywords internal
#' @noRd
.onLoad = function(libname, pkgname) {
  # initialize the environment within the package namespace
  assign(".DB_ENV", new.env(), envir = asNamespace(pkgname))

  # setup result ID counter
  options(gdb.res_count = local({
    idCounter <- -1L
    function(){
      idCounter <<- idCounter + 1L                     # increment
      formatC(idCounter, width=3, flag=0, format="d")  # format & return
    }}))

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

