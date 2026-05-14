## Resubmission

This is a resubmission. In response to the previous CRAN feedback, I:

* added `\value{}` sections to the previously flagged `.Rd` files and described the returned object classes and meanings
* removed user-facing documentation/examples that accessed internal helpers via `:::`
* removed the remaining user-facing internal-helper access pattern rather than documenting `getFromNamespace()` for non-exported functions
* updated examples and vignettes so changed `options()` / working-directory state is restored

## Test environments

* local R 4.5.2 on x86_64-pc-linux-gnu (AlmaLinux 8.10)

## R CMD check results

`R CMD check --as-cran --no-manual`

* 0 errors
* 1 warning: local `qpdf` not available
* 2 notes: new submission; current time could not be verified on this host
