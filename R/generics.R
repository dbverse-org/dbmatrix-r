.onLoad <- function(libname, pkgname) {
  # Create S4 generic for %in% which requires double dispatch
  if (!methods::isGeneric("%in%")) {
    methods::setGeneric("%in%")
  }
}