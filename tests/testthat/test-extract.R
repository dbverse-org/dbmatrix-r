
test_that('Empty bracket extracts data slot', {
  dbmat = new('dbMatrix')
  expect_identical(dbmat[], dbmat@data)
  dbmat = disconnect(dbmat)
})

test_that('Empty bracket extracts data slot', {
  dbDF = new('dbDataFrame')
  expect_identical(dbDF[], dbDF@data)
  # dbDF = disconnect(dbDF)
})
