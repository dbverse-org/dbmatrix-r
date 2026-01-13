# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# Enable densification for tests
options(dbMatrix.allow_densify = TRUE)

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dbsm <- dbMatrix:::sim_dbSparseMatrix()
dgc <- as(dbsm, "dgCMatrix")
# ---------------------------------------------------------------------------- #

# For table of expected results see `?.eval_op_densify`

test_that(" + 0 equal", {
  res_dgc <- (dgc + 0)
  res_dbsm <- (dbsm + 0) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" + 1 equal", {
  res_dgc <- (dgc + 1) |> as.matrix()
  res_dbsm <- (dbsm + 1) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" + 100 equal", {
  res_dgc <- (dgc + 100) |> as.matrix()
  res_dbsm <- (dbsm + 100) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" + Inf equal", {
  res_dgc <- (dgc + Inf) |> as.matrix()
  res_dbsm <- (dbsm + Inf) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" + NaN equal", {
  res_dgc <- (dgc + NaN) |> as.matrix()
  res_dbsm <- (dbsm + NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" - 0 equal", {
  res_dgc <- (dgc - 0)
  res_dbsm <- (dbsm - 0) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" - 1 equal", {
  res_dgc <- (dgc - 1) |> as.matrix()
  res_dbsm <- (dbsm - 1) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" - 100 equal", {
  res_dgc <- (dgc - 100) |> as.matrix()
  res_dbsm <- (dbsm - 100) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" - Inf equal", {
  res_dgc <- (dgc - Inf) |> as.matrix()
  res_dbsm <- (dbsm - Inf) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" - NaN equal", {
  res_dgc <- (dgc - NaN) |> as.matrix()
  res_dbsm <- (dbsm - NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" * 0 equal", {
  res_dgc <- (dgc * 0)
  res_dbsm <- (dbsm * 0) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" * 1 equal", {
  res_dgc <- (dgc * 1)
  res_dbsm <- (dbsm * 1) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" * 100 equal", {
  res_dgc <- (dgc * 100)
  res_dbsm <- (dbsm * 100) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" * Inf equal", {
  res_dgc <- (dgc * Inf) |> as.matrix()
  res_dbsm <- (dbsm * Inf) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" * NaN equal", {
  res_dgc <- (dgc * NaN) |> as.matrix()
  res_dbsm <- (dbsm * NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" ^ 0 equal", {
  res_dgc <- (dgc^0) |> as.matrix()
  res_dbsm <- (dbsm^0) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" ^ 1 equal", {
  res_dgc <- (dgc^1)
  res_dbsm <- (dbsm^1) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" ^ 100 equal", {
  res_dgc <- (dgc^100)
  res_dbsm <- (dbsm^100) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" ^ Inf equal", {
  res_dgc <- (dgc^Inf)
  res_dbsm <- (dbsm^Inf) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" ^ NaN equal", {
  res_dgc <- (dgc^NaN) |> as.matrix()
  res_dbsm <- (dbsm^NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %% 0 equal", {
  res_dgc <- (dgc %% 0) |> as.matrix()
  res_dbsm <- (dbsm %% 0) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %% 1 equal", {
  res_dgc <- (dgc %% 1)
  res_dbsm <- (dbsm %% 1) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %% 100 equal", {
  res_dgc <- (dgc %% 100)
  res_dbsm <- (dbsm %% 100) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %% Inf equal", {
  res_dgc <- (dgc %% Inf)
  res_dbsm <- (dbsm %% Inf) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %% NaN equal", {
  res_dgc <- (dgc %% NaN) |> as.matrix()
  res_dbsm <- (dbsm %% NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %/% 0 equal", {
  res_dgc <- (dgc %/% 0) |> as.matrix()
  res_dbsm <- (dbsm %/% 0) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %/% 1 equal", {
  res_dgc <- (dgc %/% 1)
  res_dbsm <- (dbsm %/% 1) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %/% 100 equal", {
  res_dgc <- (dgc %/% 100)
  res_dbsm <- (dbsm %/% 100) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %/% Inf equal", {
  res_dgc <- (dgc %/% Inf)
  res_dbsm <- (dbsm %/% Inf) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" %/% NaN equal", {
  res_dgc <- (dgc %/% NaN) |> as.matrix()
  res_dbsm <- (dbsm %/% NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" / 0 equal", {
  res_dgc <- (dgc / 0) |> as.matrix()
  res_dbsm <- (dbsm / 0) |> as.matrix(sparse = FALSE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" / 1 equal", {
  res_dgc <- (dgc / 1)
  res_dbsm <- (dbsm / 1) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" / 100 equal", {
  res_dgc <- (dgc / 100)
  res_dbsm <- (dbsm / 100) |> as.matrix(sparse = TRUE)
  expect_equal(res_dgc, res_dbsm)
})

test_that(" / Inf equal", {
  res_dgc <- (dgc / Inf) |> as.matrix()
  res_dbsm <- (dbsm / Inf) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that(" / NaN equal", {
  res_dgc <- (dgc / NaN) |> as.matrix()
  res_dbsm <- (dbsm / NaN) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})
