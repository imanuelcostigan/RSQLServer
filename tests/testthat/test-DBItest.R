if (has_test && is_not_cran) {
  DBItest::test_getting_started()
  DBItest::test_driver(skip = c(
    # dbDataType requires knowledge of SQL Server version which SQLServerDriver
    # class does not have knowledge of. So skip this test for Driver
    "data_type_driver"
  ))
  DBItest::test_connection()
  DBItest::test_result(skip = c(
    # jTDS closes open statements when closing connections. Closing statements
    # closes corresponding resultsets (per JDBC doc). So no need to test for this.
    "stale_result_warning",
    # data_logical* tests use SQL type BOOLEAN which is not valid in SQL Server
    # https://github.com/rstats-db/DBItest/issues/76
    "data_logical", "data_logical_null_below", "data_logical_null_above",
    "data_logical_int", "data_logical_int_null_below",
    "data_logical_int_null_above",
    # data_character* tests test encoding of Cyrillic text which is not supported
    # by default SQL Server character set.
    # https://github.com/rstats-db/DBItest/issues/77
    "data_character", "data_character_null_below", "data_character_null_above",
    "data_raw", "data_raw_null_above", "data_raw_null_below",
    # See for skip reasons: https://github.com/rstats-db/DBI/issues/149
    "data_date", "data_date_null_below", "data_date_null_above",
    "data_time", "data_time_null_below", "data_time_null_above",
    "data_time_parens", "data_time_parens_null_below", "data_time_parens_null_above",
    "data_timestamp", "data_timestamp_null_below", "data_timestamp_null_above",
    "data_timestamp_utc", "data_timestamp_utc_null_below",
    "data_timestamp_utc_null_above",
    "data_timestamp_parens", "data_timestamp_parens_null_below",
    "data_timestamp_parens_null_above"
  ))
  # Problems with data frames with list columns
  # DBItest::test_sql()
  DBItest::test_meta(skip = c(
    # https://github.com/rstats-db/DBItest/issues/109
    "column_info",
    # https://github.com/rstats-db/DBI/issues/77#issuecomment-269161605
    "quote_identifier_not_vectorized",
    # Batch disabled with multi-row parameters. Only first of each param is
    # applied
    "bind_multi_row", "bind_multi_row_unequal_length",
    # dbBind test cases inconsistent to or not reflected in documentation.
    # https://github.com/rstats-db/DBItest/issues/108#issuecomment-269559266
    "bind_empty", "bind_return_value", "bind_too_many", "bind_not_enough",
    "bind_repeated", "bind_integer", "bind_numeric", "bind_logical",
    "bind_logical_int", "bind_null", "bind_character", "bind_date",
    "bind_timestamp", "bind_timestamp_lt", "bind_raw", "bind_statement",
    "bind_statement_repeated", "bind_multi_row_statement",
    "bind_statement", "bind_multi_row_zero_length", "bind_multi_row_zero_length",
    "bind_multi_row_zero_length"
  ))
  DBItest::test_transaction()
  # A number of fails due to SQLServerPreResult not implementing key methods.
  # A few other cryptic messages which I don't yet know how to parse.
  # DBItest::test_compliance()
}
