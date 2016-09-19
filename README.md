# RSQLServer

[![CRAN](http://www.r-pkg.org/badges/version/RSQLServer)](http://cran.r-project.org/package=RSQLServer)
[![Travis-CI build status](https://travis-ci.org/imanuelcostigan/RSQLServer.svg?branch=master)](https://travis-ci.org/imanuelcostigan/RSQLServer)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/i2oedybuqi2o5crg/branch/master?svg=true)](https://ci.appveyor.com/project/imanuelcostigan/rsqlserver/branch/master)
[![Coverage status](https://codecov.io/gh/imanuelcostigan/RSQLServer/branch/master/graph/badge.svg)](https://codecov.io/gh/imanuelcostigan/RSQLServer)

An R package that provides a SQL Server R Database Interface ([DBI](https://github.com/rstats-db/DBI)), based on the cross-platform [jTDS JDBC driver](http://jtds.sourceforge.net/index.html).

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

## Config file

We recommend that you store server details and credentials in `~/sql.yaml`. This is partly so that you do not need to specify a username and password in calls to `dbConnect()`. But it is also because in testing, we've found that the jTDS single sign-on (SSO) library is a bit flaky. The contents of this file should look something like this:

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
AW:
   server: mhknbn2kdz.database.windows.net
   type: sqlserver
   user: sqlfamily
   password: sqlf@m1ly
   port: 1433
```

## Usage

First ensure that your `~/sql.yaml` file contains the `AW` entry described above:

```R

#############
# DBI
#############

# Note we do not attach the RSQLServer package.
library(DBI)
# Connect to AW server in ~/sql.yaml
# This is an Azure hosted SQL Server database provided at someone else's 
# expense. Feel free to tip them some:
# http://sqlblog.com/blogs/jamie_thomson/archive/2012/03/27/adventureworks2012-now-available-to-all-on-sql-azure.aspx
aw <- dbConnect(RSQLServer::SQLServer(), "AW", database = 'AdventureWorks2012')
# RSQLServer only returns tables with type TABLE and VIEW.
# But this DB has lots of useless tables. 
dbListTables(aw)
dbListFields(aw, 'Department')
# Department table is in accessible through the HumanResources schema
# NB: The ModifiedDate field is returned as a POSIXct date type rather than 
# as a string per JDBC interface.
dbReadTable(aw, 'HumanResources.Department')

# Fetch all results
res <- dbSendQuery(aw, 'SELECT TOP 10 * FROM HumanResources.Department')
dbFetch(res)
dbClearResult(res)

# Disconnect from DB
dbDisconnect(aw)

#############
# dplyr
#############

# Note we do not attach the RSQLServer package here either
library(dplyr)
aw <- RSQLServer::src_sqlserver("AW", database = "AdventureWorks2012")
# Alas, cannot easily call tables in non-default schema
# Workaround is to SELECT whole table
# https://github.com/hadley/dplyr/issues/244
# Retrieves and prints first ten rows of table only
(dept <- tbl(aw, sql("SELECT * FROM HumanResources.Department")))
# The following is translated to SQL and executed on the server. Only
# the first ten records are retrieved and printed to the REPL.
rd <- dept %>% 
  filter(GroupName == "Research and Development") %>% 
  arrange(Name)
# Bring the full data set back to R
collect(rd)
```
