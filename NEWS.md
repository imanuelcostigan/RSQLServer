# Version 0.2.099

- Bumped DBI requirement
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
