# RSQLServer

An R package that provides a SQL Server R Database Interface ([DBI](https://github.com/rstats-db/DBI)), based on [jTDS JDBC driver](http://jtds.sourceforge.net/index.html).

This package wraps the jTDS SQL Server driver and extends the [RJDBC](https://github.com/s-u/RJDBC) classes and DBI methods. It defines a SQLServerDriver, SQLServerConnection & SQLServerResult S4 classes as extensions of the RJDBC equivalent classes. Most of the DBI methods will simply be calls to methods defined by RJDBC classes. However, the dbConnect and some of the dbGetInfo methods are specific to SQL Server. The jTDS drivers do extend to Sybase SQL Server, but currently, only Microsoft SQL Server is supported by this package.

NB: This package has been tested on Windows (>= 6.1). It does not appear to work on some versions of 
OS X. I do not know whether it works on *nix systems.

## Installation

You can install the package from CRAN:

```R
install.packages("RSQLServer")
```

Or try the bleeding edge:

```R
install.packages('devtools')
devtools::install_github('imanuelcostigan/RSQLServer')
```

## Usage

This package uses the standard R DBI generics:

```R
library(RSQLServer)
conn <- dbConnect(SQLServer(), 'DatabaseName')
dbListTables(conn)
dbListFields(conn, 'tablename')
res <- dbSendQuery(conn, 'SELECT TOP 10 * FROM tablename')
dbFetch(res)
dbClearResult(res)
dbDisconnect(res)
```

It also has access to dplyr's interface:

```R
# Create SQL Server source
db_src <- src_sqlserver('DatabaseName')
# Print src info + list of tables (incl. temps)
print(db_src)
# Create a SQL Server table
db_tbl <- tbl(db_src, 'tablename')
# Print table's column names
colnames(db_tbl)
# Prints first ten rows
db_tbl
```

You can then use any of the standard dplyr methods. Database methods that are unsupported by SQL Server (or by a version of SQL Server) will be stopped with a message (e.g. `intersect()` is a shim around the `INTERSECT` directive but the latter is unsupported by SQL Server 2000).
