rlang::local_options(lifecycle_verbosity = "quiet")

# Inspect the lazy query tree directly so these tests can prove fusion avoided a join.
.has_join_query <- function(lazy_query) {
  if (inherits(lazy_query, "lazy_rf_join_query")) {
    return(TRUE)
  }

  if (!is.null(lazy_query$x) && inherits(lazy_query$x, "lazy_query")) {
    return(.has_join_query(lazy_query$x))
  }

  FALSE
}

.collect_ordered <- function(tbl) {
  tbl |>
    dplyr::arrange(i, j) |>
    dplyr::collect()
}

.has_sql_full_join <- function(dbm) {
  grepl("FULL JOIN", dbplyr::sql_render(dbm[]), fixed = TRUE)
}

.new_toy_sparse_dbm <- function(con, name = "toy") {
  sparse_matrix <- Matrix::sparseMatrix(
    i = c(1, 2, 3, 3),
    j = c(1, 2, 2, 4),
    x = c(2, 0.5, 4, 5),
    dims = c(4, 4)
  )

  as.dbMatrix(sparse_matrix, con = con, name = name)
}

test_that("same-source arithmetic fuses without changing results", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  db_matrix <- .new_toy_sparse_dbm(con)
  mask <- db_matrix > 0

  expected <- db_matrix[] |>
    dplyr::full_join(mask[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) * dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)
  result <- db_matrix * mask

  expect_false(.has_join_query(result[]$lazy_query))
  expect_equal(.collect_ordered(result[]), .collect_ordered(expected))
})

test_that("same-source expression chains fuse without changing results", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  db_matrix <- .new_toy_sparse_dbm(con)
  mask_low <- db_matrix > 0
  mask_high <- db_matrix > 1
  chain <- (db_matrix * 2) > 1
  sqrt_chain <- sqrt(db_matrix)

  mask_expected <- mask_low[] |>
    dplyr::full_join(mask_high[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) - dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)
  chain_expected <- db_matrix[] |>
    dplyr::full_join(chain[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) * dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)
  sqrt_expected <- sqrt_chain[] |>
    dplyr::full_join(mask_low[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) * dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)

  mask_result <- mask_low - mask_high
  chain_result <- db_matrix * chain
  sqrt_result <- sqrt_chain * mask_low

  expect_false(.has_join_query(mask_result[]$lazy_query))
  expect_false(.has_join_query(chain_result[]$lazy_query))
  expect_false(.has_join_query(sqrt_result[]$lazy_query))
  expect_equal(.collect_ordered(mask_result[]), .collect_ordered(mask_expected))
  expect_equal(.collect_ordered(chain_result[]), .collect_ordered(chain_expected))
  expect_equal(.collect_ordered(sqrt_result[]), .collect_ordered(sqrt_expected))
})

test_that("same-source fusion preserves existing coalesce semantics", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  input_tbl <- dplyr::copy_to(
    con,
    data.frame(i = c(1L, 2L), j = c(1L, 2L), x = c(NA_real_, 3)),
    name = "toy_na",
    overwrite = TRUE
  )
  db_matrix <- dbMatrix::dbMatrix(
    value = input_tbl,
    con = con,
    name = "toy_na",
    class = "dbSparseMatrix",
    dims = c(2L, 2L),
    dim_names = list(as.factor(c("r1", "r2")), as.factor(c("c1", "c2")))
  )
  mask <- db_matrix > 0

  expected <- db_matrix[] |>
    dplyr::full_join(mask[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) * dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)
  result <- db_matrix * mask

  expect_false(.has_join_query(result[]$lazy_query))
  expect_equal(.collect_ordered(result[]), .collect_ordered(expected))
})

test_that("non row-local operands fall back to join path", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  db_matrix <- .new_toy_sparse_dbm(con)
  distinct_matrix <- .new_toy_sparse_dbm(con, name = "toy2")
  filtered_tbl <- db_matrix[] |> dplyr::filter(i <= 2)
  filtered_matrix <- dbMatrix::dbMatrix(
    value = filtered_tbl,
    con = con,
    name = "filtered",
    class = "dbSparseMatrix",
    dims = dim(db_matrix),
    dim_names = dimnames(db_matrix)
  )

  distinct_result <- db_matrix * distinct_matrix
  filtered_result <- db_matrix * filtered_matrix

  expect_true(.has_join_query(distinct_result[]$lazy_query))
  expect_true(.has_join_query(filtered_result[]$lazy_query))
})

test_that("same-parent normalized expressions fuse without a full join", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  db_matrix <- .new_toy_sparse_dbm(con)
  norm <- db_matrix * colSums(db_matrix)
  mask <- norm > 0

  expected <- norm[] |>
    dplyr::full_join(mask[], by = c("i", "j")) |>
    dplyr::mutate(
      x = dplyr::coalesce(x.x, 0) * dplyr::coalesce(x.y, 0)
    ) |>
    dplyr::select(i, j, x)
  result <- norm * mask

  expect_false(.has_sql_full_join(result))
  expect_equal(.collect_ordered(result[]), .collect_ordered(expected))
})
