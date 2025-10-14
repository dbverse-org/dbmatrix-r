# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Load the dgcMatrix
dgc = sim_dgc(5, 3, 10)
mat = as.matrix(dgc)
dbdm <- as.dbMatrix(mat)
# ---------------------------------------------------------------------------- #
# rowSums

test_that("rowSums equal (memory=FALSE)", {
  res_mat = (mat + rowSums(mat)) |> as.matrix()
  res_dbdm = (dbdm + rowSums(dbdm, memory = FALSE)) |> as.matrix()
  expect_equal(res_mat, res_dbdm)
})

test_that("rowSums equal (memory=TRUE)", {
  res_mat = (mat + rowSums(mat)) |> as.matrix()
  res_dbdm = (dbdm + rowSums(dbdm, memory = TRUE)) |> as.matrix()
  expect_equal(res_mat, res_dbdm)
})

test_that("rowSums rev equal (memory=FALSE)", {
  res_mat = (rowSums(mat) + mat) |> as.matrix()
  res_dbdm = (rowSums(dbdm, memory = FALSE) + dbdm) |> as.matrix()
  expect_equal(res_mat, res_dbdm)
})

test_that("rowSums rev equal (memory=TRUE)", {
  res_mat = (rowSums(mat) + mat) |> as.matrix()
  res_dbdm = (rowSums(dbdm, memory = TRUE) + dbdm) |> as.matrix()
  expect_equal(res_mat, res_dbdm)
})
