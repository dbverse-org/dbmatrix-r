.onLoad <- function(libname, pkgname) {
  # Create S4 generic for %in% which requires double dispatch
  # Suppress the informational message about creating the generic
  suppressMessages({
    if (!methods::isGeneric("%in%")) {
      methods::setGeneric("%in%")
    }
  })
}
