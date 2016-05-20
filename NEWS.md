# Version 0.2.099

## RJDBC

This package no longer depends on RJDBC. As such a number of user visible 
changes have been made:

- `dbSendQuery()` only executes `SELECT` commands which return a result and not other arbitrary SQL code. See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). It also no longer supports calling stored procedures (callable statements) or prepared statements as these do not seem to be explicitly supported by any other DBI backend.
- `dbSendUpdate()` which was based on RJDBC's method and which executes SQL commands that do not return a result will be deprecated in favour of the more descriptive `dbExecute()`. See [rstats-db/DBI#20](https://github.com/rstats-db/DBI/issues/20). It also no longer supports calling stored procedures (callable statements) or prepared statements as these do not seem to be explicitly supported by any other DBI backend.
- Implemented `dbUnloadDriver()` which returns `TRUE` in all instances rather than `FALSE` as was the case in RJDBC.


## DBItest

A number of changes have been made to ensure DBI compliance as specified by tests in the `DBItest` package:

- `dbDisconnect()` returns a warning if called on a connection that is already closed and otherwise closes the connection and return `TRUE`.
- `dbGetInfo()` for `SQLServerDriver` returns the JDBC driver version (3.0) as `driver.version` and jTDS verion as `client.version` instead of the jTDS client version and `NA` respectively. Also complies with `DBItest` expectations.
- `dbGetInfo()` for `SQLServerConnection` now complies with `DBItest` expectations
- `SQLServer()` no longer accepts arguments
- Bumped DBI requirement

## Other changes

- `dbDataType` maps R character objects of sufficiently long length to `VARCHAR(MAX)` on newer version of MSSQL rather than `TEXT` as the latter is being deprecated.
- Arguments of `dbConnect()` are now `NULL` where other default values were assigned. This does not change the behaviour of the method.
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
