
test_that('driver extracts working DB driver call', {
  dbmat = dbMatrix()
  drv_call = driver(dbmat)
  expect_true(inherits(drv_call, 'call'))
  drv = eval(drv_call)
  expect_true(DBI::dbCanConnect(drv))
  duckdb::duckdb_shutdown(drv)
})

test_that('remoteValid works', {
  dbmat = dbMatrix()
  expect_true(remoteValid(dbmat))
  dbmat = disconnect(dbmat)
  expect_false(remoteValid(dbmat))
})



dbmat = dbMatrix()

test_that('remoteListTables works', {
  expect_identical('test', remoteListTables(dbmat))
  expect_identical('test', remoteListTables(connection(dbmat)))
})

test_that('remoteName works', {
  rn = remoteName(dbmat)
  expect_identical('test', rn)
  expect_true(inherits(rn, 'character'))
})

test_that('remoteExistsTable works', {
  expect_false(remoteExistsTable(dbmat, 'not_there'))
  expect_false(remoteExistsTable(connection(dbmat), 'not_there'))
  expect_true(remoteExistsTable(dbmat, 'test'))
  expect_true(remoteExistsTable(connection(dbmat), 'test'))
})




dbmat = disconnect(dbmat)
