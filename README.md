# RSQLServer

An R package that provides a SQL Server R Database Interface ([DBI](https://github.com/rstats-db/DBI)), based on the [jTDS JDBC driver](http://jtds.sourceforge.net/index.html).

## Installation

You can install the package from CRAN:

```R
install.packages("RSQLServer")
```

Or try the development version from GitHub:

```R
# install.packages('devtools')
devtools::install_github('imanuelcostigan/RSQLServer')
```

NB: This package has only been tested on Windows 7 x64 (>= 6.1). However this package rests on the cross-platform jTDS driver. 

## Usage


```R
library(DBI)
# Connect to TEST server in ~/sql.yaml
conn <- dbConnect(RSQLServer::SQLServer(), "TEST", database = 'db')

dbListTables(conn)
dbListFields(conn, 'tablename')
dbReadTable(conn, 'tablename')

# Fetch all results
res <- dbSendQuery(conn, 'SELECT TOP 10 * FROM tablename')
dbFetch(res)
dbClearResult(res)

# Disconnect from DB
dbDisconnect(conn)
```

## Config file

As alluded to above, we recommend that you store server details and credentials in `~/sql.yaml`. This is partly so that you do not need to specify a username and password in calls to `dbConnect()`. But it is also because in testing, we've found that the jTDS single sign-on (SSO) library is a bit flaky. The contents of this file should look something like this:

```yaml
SQL_PROD:
    server: 11.1.111.11
    type: &type sqlserver
    port: &port 1433
    domain: &domain companyname
    user: &user winusername
    password: &pass winpassword
    useNTLMv2: &ntlm true
SQL_DEV:
    server: 11.1.111.15
    type: *type
    port: *port
    domain: *domain
    user: *user
    password: *pass
    useNTLMv2: *ntlm
```

## dplyr

The integration of the RSQLServer backend with dplyr is coming.
