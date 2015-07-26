# Version 0.2

- Implements a SQL backend to dplyr (#4)
- `dbConnect()` interface has been rewriting but significantly improved. Server details can be specified in a YAML file which are passed to dbConnect. (#8)
- `fetch()` now processes the data frame returned by `RJDBC::fetch()` to better map database field types to their equivalents in R such as integer, Date, POSIXct and raw. `dbGetQuery()` method calls this `fetch()` method rather than the default JDBCConnection method. (#31)
- `dbWriteTable()` can now write Date, POSIXct and other non-character and non-numeric field types to equivalent SQL representations (#32)
- Downgraded bundled jTDS from 1.3.1 to 1.2.8 (#9)
- Improved compliance with DBI interface
- Untested supported for Sybase in `jtds_url()`
- NEWS now in markdown (`NEWS.md`)

# Version 0.1.1

- implemented and exported dbIsValid methods for SQLServerConnection and SQLServerResult (#1)

# Version 0.1

- Initial release
