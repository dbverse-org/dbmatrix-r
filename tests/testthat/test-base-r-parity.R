# Each case is run on a dbMatrix `x` and on the same base R matrix `m`;
# the results must match. Add a line here to cover a new operation.
cases <- c(
  "1 / x", "8 / x", "0 - x", "x / 1", "x - 0",
  "exp(x)", "cos(x)", "x == 0", "x >= 0", "x > 1",
  "-x", "+x",
  "head(x, 1)", "tail(x, 1)", "head(x, 10)",
  "rowMeans(x)", "colMeans(x)",
  "min(x)", "max(x)", "max(-x)"
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

  test_that(paste(class, "means skip NA values"), {
    value <- if (class == "dbSparseMatrix") as(m_na, "CsparseMatrix") else m_na
    x_na <- dbMatrix(value = value, con = con, name = "parity_na", class = class, overwrite = TRUE)
    expect_equal(unname(rowMeans(x_na)), rowMeans(m_na, na.rm = TRUE))
    expect_equal(unname(colMeans(x_na)), colMeans(m_na, na.rm = TRUE))
  })
}

test_that("sparse ops that change zeros error instead of skipping them when densify is off", {
  options(dbMatrix.allow_densify = FALSE)
  on.exit(options(dbMatrix.allow_densify = TRUE))
  x <- dbMatrix(value = as(m, "CsparseMatrix"), con = con, name = "parity_off",
                class = "dbSparseMatrix", overwrite = TRUE)
  expect_error(as.matrix(exp(x)), "sparse-to-dense")
  expect_error(as.matrix(x == 0), "sparse-to-dense")
  expect_error(as.matrix(1 / x), "sparse-to-dense")
})

test_that("temporary table names leave the random seed alone", {
  set.seed(1)
  expected <- runif(1)
  set.seed(1)
  x <- dbMatrix(value = m, con = con, name = "parity_seed", class = "dbDenseMatrix", overwrite = TRUE)
  invisible(as.matrix(x[1, ] + 1))
  expect_identical(runif(1), expected)
})

test_that("summaries of all-missing or NaN values match base R", {
  all_na <- matrix(NA_real_, 2, 2)
  with_nan <- matrix(c(1, NaN, 3, 4), 2)
  x_na <- dbMatrix(value = all_na, con = con, name = "parity_all_na", class = "dbDenseMatrix", overwrite = TRUE)
  x_nan <- dbMatrix(value = with_nan, con = con, name = "parity_nan", class = "dbDenseMatrix", overwrite = TRUE)
  expect_identical(sum(x_na), sum(all_na, na.rm = TRUE))
  expect_identical(any(x_na), any(all_na, na.rm = TRUE))
  expect_identical(all(x_na), all(all_na, na.rm = TRUE))
  expect_true(is.nan(sum(x_nan)))
})

test_that("matrix products give a clear error instead of recursing", {
  x <- dbMatrix(value = m, con = con, name = "parity_prod", class = "dbDenseMatrix", overwrite = TRUE)
  expect_error(crossprod(x), "not supported")
  expect_error(tcrossprod(x), "not supported")
  expect_error(x %*% x, "not supported")
  expect_error(crossprod(m, x), "not supported")
})

DBI::dbDisconnect(con, shutdown = TRUE)
options(old_options)
