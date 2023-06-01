
dbpoly = simulate_dbPolygonProxy()

test_that('spatial chunking selects the correct number of polys', {
  ext_list = chunk_plan(extent = ext(dbpoly), min_chunks = 4L)
  expected_len = nrow(dbpoly)

  # chunk data
  chunk_x_list = lapply(
    ext_list,
    function(e) {
      # 'soft' selections on top and right
      extent_filter(x = dbpoly, extent = e, include = c(TRUE, TRUE, FALSE, FALSE),
                    method = 'mean')
    }
  )

  chunk_x_list_len = lapply(chunk_x_list, nrow)
  select_len_sum = do.call(sum, chunk_x_list_len)

  expect_equal(select_len_sum, expected_len)
})

