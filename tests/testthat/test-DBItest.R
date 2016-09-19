context("DBI tests")

DBItest::make_context(SQLServer(), list(server = "TEST", database = "DBITest"))
DBItest::test_getting_started()
DBItest::test_driver()
DBItest::test_connection()
# DBItest::test_result(skip = c(
#   # jTDS closes open statements when closing connections. Closing statements
#   # closes corresponding resultsets (per JDBC doc). So no need to test for this.
#   "stale_result_warning",
#   # command_query sub-tests call dbSendQuery to execute commands such as CREATE
#   # etc which do not return a ResultSet which causes method to fail. See:
#   # https://github.com/rstats-db/DBI/issues/20#issuecomment-220271271
#   # Once dbExecute has been implemented in DBI, then can test this properly.
#   "command_query",
#   # See previous comments. Waiting for dbExecute in DBI and update to DBItest
#   "fetch_no_return_value",
#   # See previous comments. Waiting for dbExecute in DBI and update to DBItest
#   "data_type_connection",
#   # data_logical* tests use SQL type BOOLEAN which is not valid in SQL Server
#   # https://github.com/rstats-db/DBItest/issues/76
#   "data_logical", "data_logical_null_below", "data_logical_null_above",
#   "data_logical_int", "data_logical_int_null_below",
#   "data_logical_int_null_above",
#   # data_character* tests test encoding of Cyrillic text which is not supported
#   # by default SQL Server character set.
#   # https://github.com/rstats-db/DBItest/issues/77
#   "data_character", "data_character_null_below", "data_character_null_above"))

