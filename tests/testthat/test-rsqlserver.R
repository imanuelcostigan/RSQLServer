context("RSQLServer")

test_that("get_server_details works", {
  file <- system.file("extdata", "sql.yaml", package = "RSQLServer")
  server_details <- get_server_details("SQL_PROD", file)
  expect_true(is.list(server_details))
  expected_names <-
    c("server", "type", "port", "domain", "user", "password", "useNTLMv2")
  expect_equal(names(server_details), expected_names)
  expect_null(get_server_details("SQL_UAT", file))
  expect_error(get_server_details("SQL_TYPE_ERROR", file))
  expect_error(get_server_details("SQL_PORT_ERROR", file))
})

test_that("msft_url works", {
  file <- system.file("extdata", "sql.yaml", package = "RSQLServer")
  sd <- get_server_details("SQL_PROD", file)
  expected_url <- "jdbc:sqlserver://;serverName=11.1.111.11;portNumber=1433;database=test"
  expect_equal(msft_url(sd$server, sd$port, sd$instance, list(database = "test")),
    expected_url)
  expected_url <- "jdbc:sqlserver://;serverName=11.1.111.11;portNumber=1433;authentication=ActiveDirectoryIntegrated;domain=corpname;database=test"
  expect_equal(msft_url(sd$server, sd$port, sd$instance,
    list(authentication = 'ActiveDirectoryIntegrated', domain = 'corpname', database = "test")), expected_url)
})

test_that("have_test_server and SQLServerConnection work", {
  if (have_test_server("sqlserver")) {
    conn <- dbConnect(SQLServer(), "TEST")
    expect_is(conn, "SQLServerConnection")
  }
})

