context("DBI tests")

DBItest::make_context(SQLServer(), list(server = "TEST"))
DBItest::test_getting_started()
DBItest::test_all()
