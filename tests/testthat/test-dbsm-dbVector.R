# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc = sim_dgc(5, 3, 10)
dbsm <- as.dbMatrix(dgc)
# ---------------------------------------------------------------------------- #
# rowSums
test_that("rowSums equal (memory=FALSE)", {
  res_dgc = (dgc + rowSums(dgc)) |> as.matrix()
  res_dbsm = (dbsm + rowSums(dbsm, memory = FALSE)) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("rowSums equal (memory=TRUE)", {
  res_dgc = (t(dgc) + rowSums(dgc)) |> as.matrix()
  res_dbsm = (t(dbsm) + rowSums(dbsm, memory = TRUE)) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("rowSums rev equal (memory=FALSE)", {
  res_dgc = (rowSums(dgc) + dgc) |> as.matrix()
  res_dbsm = (rowSums(dbsm, memory = FALSE) + dbsm) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("rowSums rev equal (memory=TRUE)", {
  res_dgc = (rowSums(dgc) + dgc) |> as.matrix()
  res_dbsm = (rowSums(dbsm, memory = TRUE) + dbsm) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("colSums equal (memory=FALSE)", {
  res_dgc = (dgc + colSums(dgc)) |> as.matrix()
  res_dbsm = (dbsm + colSums(dbsm, memory = FALSE)) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("colSums equal (memory=TRUE)", {
  res_dgc = (dgc + colSums(dgc)) |> as.matrix()
  res_dbsm = (dbsm + colSums(dbsm, memory = TRUE)) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("colSums rev equal (memory=FALSE)", {
  res_dgc = (colSums(dgc) + dgc) |> as.matrix()
  res_dbsm = (colSums(dbsm, memory = FALSE) + dbsm) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})

test_that("colSums rev equal (memory=TRUE)", {
  res_dgc = (colSums(dgc) + dgc) |> as.matrix()
  res_dbsm = (colSums(dbsm, memory = TRUE) + dbsm) |> as.matrix()
  expect_equal(res_dgc, res_dbsm)
})
