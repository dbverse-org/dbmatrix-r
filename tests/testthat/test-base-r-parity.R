# Each case is run on a dbMatrix `x` and on the same base R matrix `m`;
# the results must match. Add a line here to cover a new operation.
cases <- c(
  "1 / x", "8 / x", "0 - x", "x / 1", "x - 0"
)

m <- matrix(c(1, 2, 0, 4, 0, 8), 2, dimnames = list(c("a", "b"), c("x", "y", "z")))
m_na <- matrix(c(2, NA, 4, 6, 8, 10), 2, byrow = TRUE)
con <- DBI::dbConnect(duckdb::duckdb())
old_options <- options(dbMatrix.allow_densify = TRUE, dbMatrix.verbose = FALSE)

as_plain <- function(r) {
  if (inherits(r, "dbMatrix")) r <- as.matrix(r)
  if (is.logical(r)) r[] <- as.numeric(r)
  r
}

for (class in c("dbDenseMatrix", "dbSparseMatrix")) {
  value <- if (class == "dbSparseMatrix") as(m, "CsparseMatrix") else m
  x <- dbMatrix(value = value, con = con, name = "parity", class = class, overwrite = TRUE)

  for (case in cases) {
    test_that(paste(class, case, "matches base R"), {
      expect_equal(
        as_plain(eval(str2lang(case), list(x = x))),
        as_plain(eval(str2lang(case), list(x = m)))
      )
    })
  }

}

DBI::dbDisconnect(con, shutdown = TRUE)
options(old_options)
