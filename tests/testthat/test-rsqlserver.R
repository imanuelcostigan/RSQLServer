context("RSQLServer")

test_that("checking SQLServerConnection", {
  # Skips the test if doesn't have the key to open the secure vault
  secure::skip_when_missing_key("RSQLServer")
  # Decrypt relevant settings
  thp <- secure::decrypt("test_harness", vault="RSQLServer")

  # Check connection and its methods
  conn <- dbConnect(SQLServer(), server=thp$server, database=thp$database,
    port=thp$port, useNTLMv2='true')
  expect_is(conn, "SQLServerConnection")
  expect_is(dbGetInfo(conn), "list")
  expect_true(dbIsValid(conn))

  # Check query sending
#   qryres <- dbSendQuery(conn, paste0("SELECT TOP 10 * FROM ", thp$table))
#   expect_is(qryres, "SQLServerConnection")
})
