
dbmat = new('dbMatrix')
dbDF = new('dbDataFrame')

test_that('queryStack extracts query info from dbMatrix', {
  expect_identical(queryStack(dbmat), dbmat@data$lazy_query)
})

test_that('queryStack extracts query info from dbDataFrame', {
  expect_identical(queryStack(dbDF), dbDF@data$lazy_query)
})


test_that('queryStack<- replaces query info from dbMatrix', {
  dbmat = new('dbMatrix')
  queryStack(dbmat) = 'test'
  expect_identical(queryStack(dbmat), 'test')
})

test_that('queryStack<- replaces query info from dbMatrix', {
  dbDF = new('dbDataFrame')
  queryStack(dbDF) = 'test'
  expect_identical(queryStack(dbDF), 'test')
})

dbmat = disconnect(dbmat)
# dbDF = disconnect(dbDF)
