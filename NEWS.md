# Version 0.3.0

RSQLServer was archived by CRAN after dplyr v0.4 irredeemably broke the dplyr SQL Server backend provided by this package. Well we are back on CRAN and there have been a lot of changes since you last saw this package including improvements to the DBI backend, compatibility with the latest iteration of the dplyr/dbplyr backend and removing the reliance on RJDBC's middleware.

The next version of this package will likely swap the jTDS driver for the official Microsoft JDBC driver and make further interface changes to better align with the more explicit interface specifications in the DBI package.

## DBI backend

A number of changes have been made to improve DBI compliance as specified by tests in the `DBItest` package (#60):

- `fetch()` on non-query statements return an empty data frame
- Nested transactions throw errors with related change of `dbWriteTable()` code now being wrapped in `dbWithTransaction()`
- `dbDataType()` works for `AsIs` objects and returns `NVARCHAR` and `VARBINARY` SQL types with lengths of at least one
- `dbHasCompleted()` returns `TRUE` for empty ResultSets 
- `dbDisconnect()` returns a warning if called on a connection that is already closed and otherwise closes the connection and returns `TRUE`.
- `dbGetInfo()` for `SQLServerConnection` now complies with `DBItest` expectations
- `dbGetInfo()` for `SQLServerDriver` returns the JDBC driver version (3.0) as `driver.version` and jTDS verion as `client.version` instead of the jTDS client version and `NA` respectively. 
- `SQLServer()` no longer accepts arguments
- Bumped DBI requirement

A number of other changes have been made to the `DBI` backend:

- Implemented `dbBegin()`, `dbCommit()`, `dbRollback()` methods for DBI generics
- `dbWriteTable()` is faster by always using transactions (`BEGIN` before and `COMMIT` after), and optionally much faster by way of the `batch` option.
- Changed API for `dbWriteTable()` to match generic documented in the DBI package. It also now returns `TRUE` invisibly.
- `dbWriteTable()` now fails when attempting to append to a temporary table (#75)
- Implemented `dbSendStatement()` method which required the extension of `SQLServerResult` to `SQLServerUpdateResult` the latter of which is used to dispatch the `dbGetRowsAffected()` method (#95). Added `batch` option to both `dbSendStatement()` and `dbSendQuery()` for insert/update speedup (#69, #90, #106, @r2evans).
- Implemented `dbBind()` method to replace the internal `.fillStatementParameter()` method which required the extension of `SQLServerResult` to `SQLServerPreResult` the latter of which allows statements with bindings to present a ResultSet interface to DBI (ResultSets are only created after values are bound to parameterised statements in JDBC). (#88)
- `dbBind()` now supports multi-row binding (e.g., for `INSERT` and `UPDATE`)
- Implemented `sqlCreateTable()` for `SQLServerConnection` which is called by `db_create_table()`. (#76)
- `dbDataType` maps R character objects of sufficiently long length to `VARCHAR(MAX)` on newer version of MSSQL rather than `TEXT` as the latter is being deprecated.
- Arguments of `dbConnect()` are now `NULL` where other default values were assigned. This does not change the behaviour of the method.
- Introduced `pattern` argument to `dbListTables()` which allows you to list all tables matching a pattern.
- `dbExistsTable()` now passed table name to `dbListTables()` as a pattern to be matched which should improve its performance.
- `dbColumnInfo()` succeeds in running (#96, @r2evans)
- `dbGetInfo()` method for `SQLServerResult` has been removed and calls the default DBI method which calls `dbHasCompleted()`, `dbGetRowCount()` etc. The latter methods have been implemented for `SQLServerResult` and are exported.
- `dbIsValid()` implemented for `SQLServerDriver` and always returns `TRUE`.
- Now rely on DBI supplied `show()` methods

## dplyr/dbplyr backend

A number of changes were made to `dplyr` backend including a refactoring of its code across to the newer `dbplyr` package. As a result, dplyr >= 0.7.0 is required:

- `src_desc()` defunct in favour of `db_desc()` 
- Implemented `db_create_table()`, `db_write_table()` and `db_insert_into()`for `SQLServerConnection`
- `db_drop_table()` supports the `IF EXISTS` SQL clause if supported by SQL Server (#75)
- New `temporary` argument to `db_insert_into()` which overwrites existing table if set to `TRUE` and if necessary. 
- `sql_select()` method supports the `DISTINCT` keyword and includes `TOP` keyword when query results are ordered.
- `compute()` and `copy_to()` implementations are replaced by `db_compute()` and `db_copy_to()` implementations
- `db_explain()` is more informative (e.g. prints relative cost of operations)
- `db_analyze()` unsupported and simply returns `TRUE`.
- `db_query_fields()` method for SQLServerConnection removed in favour of default dplyr method. The latter better handles sub-queries.
- `intersect()` and `setdiff()` methods are removed in favour of default `dplyr` methods.
- `as.numeric()` and `as.character()` calls now cast scalar input values to SQL types `FLOAT` and `NVARCHAR(4000)` respectively rather than `NUMERIC` and `TEXT` respectively (default in dplyr). 
- Added basic testing of dplyr backend (#81)

## RJDBC 

This package no longer depends on `RJDBC`. As such a number of user visible changes have been made:

- `dbSendQuery()` only executes queries rather than other arbitrary SQL statements. See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). It also no longer supports calling stored procedures (callable statements).
- `dbSendQuery()` can execute parameterised queries. See `?DBI:dbBind` for more details on parameterised queries.
- `dbSendUpdate()` which was based on RJDBC's method and which executes non-query SQL statements will be deprecated in favour of the more descriptive `dbExecute()` which has been implemented upstream in DBI (the latter of which calls `dbSendStatement()`). See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). Unlike RJDBC's `dbSendUpdate()`, `dbExecute()` does not yet support calling stored procedures as these do not seem to be explicitly supported by any other DBI backend. 
- `dbExecute()` arguments have been changed to reflect the DBI generic.
- Implemented `dbUnloadDriver()` which returns `TRUE` in all instances rather than `FALSE` as was the case in RJDBC.

A number of previously imported RJDBC methods have now been reimplemented in this package with no user visible changes.

## Other changes

- Added Travis-CI (#83, #84) and Appveyor support (#80, @Hong-Revo)
- Correctly determine major version of SQL Server DB backend (#122)
- dplyr's `arrange()` method now returns whole result rather than top 100 rows (#124). This was implemented by changing the default behaviour of `sql_select()` method and may result in breaks to existing code.

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
