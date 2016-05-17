context("DBI tests")

DBItest::make_context(SQLServer(), list(server = "TEST"))
DBItest::test_getting_started()
DBItest::test_driver(skip = c("data_type", "stress_load_unload"))
DBItest::test_connection(skip = "stress_load_connect_unload")
DBItest::test_result()
