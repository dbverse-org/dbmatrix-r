# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the RDS file in the 'data' folder
dgc <- readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix"))

con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con1,
  name = 'dgc',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# Test scalar arithmetic

res_dgc <- dgc + dgc
res_dbsm <- dbsm + dbsm
res_dbsm <- as.matrix(res_dbsm, sparse = TRUE, names = TRUE)

test_that("+ matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

res_dgc <- dgc - dgc
res_dbsm <- dbsm - dbsm
res_dbsm <- as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("- matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

res_dgc <- dgc * dgc
res_dbsm <- dbsm * dbsm
res_dbsm <- as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("* matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

dgc2 <- dgc * 2
dbsm2 <- dbMatrix::dbMatrix(
  value = dgc2,
  con = con1,
  name = 'dgc2',
  class = "dbSparseMatrix",
  overwrite = TRUE
)

res_dgc <- dgc - dgc2
res_dbsm <- dbsm - dbsm2
res_dbsm <- as.matrix(res_dbsm, sparse = TRUE, names = TRUE)
test_that("- different matrix equal", {
  expect_equal(res_dgc, res_dbsm)
})

# FIXME:
# Support for division by 0
# res_dgc = dgc / dgc
# res_dbsm = dbsm / dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("/ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: NaN and 1 logic
# res_dgc = dgc ^ dgc
# res_dbsm = dbsm ^ dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("^ matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, NaN and 1 logic
# res_dgc = dgc %% dgc
# res_dbsm = dbsm %% dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("%% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# FIXME: division by 0, Nan and 1 logic
# res_dgc = dgc %/% dgc
# res_dbsm = dbsm %/% dbsm
# res_dbsm = as.matrix(res_dbsm)
# test_that("%/% matrix equal", {
#   expect_equal(res_dgc, res_dbsm)
# })

# ---------------------------------------------------------------------------- #
# Test support-preserving sparse division (numerator positions must be in denominator)

test_that("/ sparse division works when denominator covers numerator", {
  # D / (P + D) pattern: denominator always covers numerator
  d <- dbsm
  p_plus_d <- dbsm + dbsm2 # covers all positions from both

  res_dbsm <- d / p_plus_d
  expect_s4_class(res_dbsm, "dbSparseMatrix")

  # Compare stored values (sparse doesn't store implicit 0/0 positions)
  # D / (D + 2D) = D / 3D = 1/3 for all non-zero positions
  res_data <- res_dbsm[] |> dplyr::collect()
  expect_true(all(abs(res_data$x - 1 / 3) < 1e-10))
  expect_equal(nrow(res_data), length(dgc@x)) # same number of stored values
})

test_that("/ sparse division fails when denominator doesn't cover numerator", {
  # Create matrices with different non-zero positions
  dgc_subset <- dgc[1:5, 1:5]
  dbsm_full <- dbMatrix::dbMatrix(
    value = dgc[1:10, 1:10],
    con = con1,
    name = 'full',
    class = "dbSparseMatrix",
    overwrite = TRUE
  )
  dbsm_subset <- dbMatrix::dbMatrix(
    value = dgc_subset,
    con = con1,
    name = 'subset',
    class = "dbSparseMatrix",
    overwrite = TRUE
  )

  # full / subset should fail (full has positions subset doesn't)
  expect_error(dbsm_full / dbsm_subset, "denominator")
})
