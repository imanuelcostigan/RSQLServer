# Version 0.2

- Now a functioning DBI backend to `dplyr`
- `dbConnect()` interface has been changed significantly. Server details specified in a YAML file can be used to pass server details to dbConnect. (#8)
- `dbFetch()` now processes data frame returned by `RJDBC::fetch()` to better reflect field types in database (#31)
- `db_has_table()` method no longer checks for temporary tables (#29)
- `db_query_fields()` and `db_query_rows()` much faster (#12, #24). Makes table printing faster
- Improved compliance with DBI interface
- NEWS now in markdown (`NEWS.md`)

# Version 0.1.1

- implemented and exported dbIsValid methods for SQLServerConnection and SQLServerResult (#1)

# Version 0.1

- Initial release
