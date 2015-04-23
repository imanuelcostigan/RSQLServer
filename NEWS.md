# Version 0.2

- dbConnect() interface has been changed significantly. Server details specified in a YAML file can be used to pass server details to dbConnect. (#8)
- DBI is an Import and not Depend. Use of package in README has also been updated. (#10)
- While jTDS is cross-platform, the package, inexplicably, does not work on other platforms. As a result, you should consider this package stable and supported by me for Windows 7 or greater. (#9)
- NEWS now in markdown (`NEWS.md`)

# Version 0.1.1

- implemented and exported dbIsValid methods for SQLServerConnection and SQLServerResult (#1)

# Version 0.1

- Initial release
