# Version 0.2.099

## RJDBC

This package no longer depends on RJDBC. As such a number of user visible changes have been made:

- `dbSendQuery()` only executes `SELECT` commands (queries) which return a result and not other arbitrary SQL code. See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). It also no longer supports calling stored procedures (callable statements) or prepared statements as these do not seem to be explicitly supported by any other DBI backend.
- `dbSendUpdate()` which was based on RJDBC's method and which executes SQL commands that do not return a result will be deprecated in favour of the more descriptive `dbExecute()` which has been implemented upstream in DBI. See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). Unlike RJDBC's `dbSendUpdate()`, `dbExecute()` does not yet support calling stored procedures as these do not seem to be explicitly supported by any other DBI backend.
- Implemented `dbUnloadDriver()` which returns `TRUE` in all instances rather than `FALSE` as was the case in RJDBC.

A number of previously imported RJDBC methods have now been reimplemented in this package with no user visible changes.

## DBItest

A number of changes have been made to improve DBI compliance as specified by tests in the `DBItest` package (#60):

- `dbDisconnect()` returns a warning if called on a connection that is already closed and otherwise closes the connection and return `TRUE`.
- `dbGetInfo()` for `SQLServerDriver` returns the JDBC driver version (3.0) as `driver.version` and jTDS verion as `client.version` instead of the jTDS client version and `NA` respectively. Also complies with `DBItest` expectations.
- `dbGetInfo()` for `SQLServerConnection` now complies with `DBItest` expectations
- `SQLServer()` no longer accepts arguments
- Bumped DBI requirement
- NB: that more changes should be expected as the DBItest package matures.

## dplyr

A number of changes were made to dplyr backend. As a result, dplyr >= 0.5 is required. Most changes are not visible to users. However, the following are of note:

- Implemented `db_create_table()`, `db_insert_into()` and `db_create_index()` for SQLServerConnection
- Updated `db_drop_table()` to support `IF EXISTS` SQL clause if supported by 
SQL Server (#75)
- New `temporary` argument to `db_insert_into()` which overwrites existing table if set to `TRUE` and if necessary. 
- `db_query_fields()` method for SQLServerConnection removed in favour of default dplyr method. The latter better handles sub-queries.
- `sql_select()` method supports the `DISTINCT` keyword and includes `TOP` keyword when query results are ordered.
- `compute()` is now modified version of dplyr default method
- `db_explain()` is more informative (e.g. prints relative cost of operations)
- `db_analyze()` unsupported and simply returns `TRUE`.
- `src_sqlserver` has a nicer print
- `intersect()` and `setdiff()` methods are deprecated and default `tbl_sql` methods from `dplyr` are called instead.

## Other changes

- Implemented `dbBegin()`, `dbCommit()`, `dbRollback()` methods and use these in `dbWriteTable()`
- `dbWriteTable()` now fails when attempting to append to a temporary table (#75)
- Implemented `sqlCreateTable()` for `SQLServerConnection` which is called by `db_create_table()`. (#76)
- `dbDataType` maps R character objects of sufficiently long length to `VARCHAR(MAX)` on newer version of MSSQL rather than `TEXT` as the latter is being deprecated.
- Arguments of `dbConnect()` are now `NULL` where other default values were assigned. This does not change the behaviour of the method.
- Introduced `pattern` argument to `dbListTables()` which allows you to list all tables matching a pattern.
- `dbExistsTable()` now passed table name to `dbListTables()` as a pattern to be matched which should improve its performance.
- `dbGetInfo()` for `SQLServerResult` has been deprecated and calls the DBI default method which calls `dbHasCompleted()`, `dbGetRowCount()` etc. The latter methods have been implemented for `SQLServerResult` and are exported.
- Now rely on DBI supplied `show()` methods
- Added Travis-CI support

# Version 0.2.0

## Major changes

- Implements a SQL backend to dplyr (#4) including SQL translations for as.POSIXct and as.Date (#43)
- `dbConnect()` interface has been enhanced but breaks backward compatibility consequently enhanced. Server details can be specified in a YAML file which are passed to dbConnect. See `?SQLServer` (#8)

## Enhancements

- `fetch()` now processes the data frame returned by `RJDBC::fetch()` to better map database field types to their equivalents in R such as integer, logical, Date, POSIXct and raw. (#31, #47)
- `dbWriteTable()` can now write Date, POSIXct and other non-character and non-numeric field types to equivalent SQL representations (#32)

## Other changes

- Azure hosted SQL Server support (#3)
- Downgraded bundled jTDS from 1.3.1 to 1.2.8 (#9)
- Improved DBI compliance (#35).
- Untested supported for Sybase in `jtds_url()`
- NEWS now in markdown (`NEWS.md`)

# Version 0.1.1

- implemented and exported dbIsValid methods for SQLServerConnection and SQLServerResult (#1)

# Version 0.1

- Initial release
