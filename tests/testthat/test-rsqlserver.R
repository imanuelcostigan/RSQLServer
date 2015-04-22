context("RSQLServer")

test_that("SQLServerConnection", {

  conn <- dbConnect(SQLServer(), server=thp$server, database=thp$database,
    port=thp$port, useNTLMv2='true')
  expect_is(conn, "SQLServerConnection")
  expect_is(dbGetInfo(conn), "list")
  expect_true(dbIsValid(conn))
})
