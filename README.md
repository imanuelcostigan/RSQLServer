
<!-- README.md is generated from README.Rmd. Please edit that file -->
RSQLServer
==========

[![CRAN](http://www.r-pkg.org/badges/version/RSQLServer)](http://cran.r-project.org/package=RSQLServer) [![Travis-CI build status](https://travis-ci.org/imanuelcostigan/RSQLServer.svg?branch=master)](https://travis-ci.org/imanuelcostigan/RSQLServer) [![Appveyor build status](https://ci.appveyor.com/api/projects/status/i2oedybuqi2o5crg/branch/master?svg=true)](https://ci.appveyor.com/project/imanuelcostigan/rsqlserver/branch/master) [![Coverage status](https://codecov.io/gh/imanuelcostigan/RSQLServer/branch/master/graph/badge.svg)](https://codecov.io/gh/imanuelcostigan/RSQLServer)

An R package that provides a SQL Server R Database Interface ([DBI](https://github.com/rstats-db/DBI)), based on the cross-platform [jTDS JDBC driver](http://jtds.sourceforge.net/index.html).

Installation
------------

You can't install the package from CRAN yet. But you install the development version from GitHub:

``` r
# install.packages('devtools')
devtools::install_github('imanuelcostigan/RSQLServer')
```

Config file
-----------

We recommend that you store server details and credentials in `~/sql.yaml`. This is partly so that you do not need to specify a username and password in calls to `dbConnect()`. But it is also because in testing, we've found that the jTDS single sign-on (SSO) library is a bit flaky. The contents of this file should look something like this:

``` yaml
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

Usage
-----

Ensure that your `~/sql.yaml` file contains a valid SQL Server entry named `TEST`. In the following, the `TEST` server, generously provided by Microsoft for the purposes of this package's development, has a database containing the `nycflights13` package data sets.

### DBI usage

The following illustrates how you can make use of the DBI interface. Note that we **do not** attach the `RSQLServer` package.

``` r
library(DBI)
nycflights <- dbConnect(RSQLServer::SQLServer(), server = "TEST", database = 'DBItest')
# RSQLServer only returns tables with type TABLE and VIEW.
dbListTables(nycflights)
#> [1] "airlines"   "airports"   "cars"       "flights"    "planes"    
#> [6] "test_table" "weather"
dbListFields(nycflights, 'airlines')
#> [1] "carrier" "name"
dbReadTable(nycflights, 'airlines')
#>    carrier                        name
#> 1       9E           Endeavor Air Inc.
#> 2       AA      American Airlines Inc.
#> 3       AS        Alaska Airlines Inc.
#> 4       B6             JetBlue Airways
#> 5       DL        Delta Air Lines Inc.
#> 6       EV    ExpressJet Airlines Inc.
#> 7       F9      Frontier Airlines Inc.
#> 8       FL AirTran Airways Corporation
#> 9       HA      Hawaiian Airlines Inc.
#> 10      MQ                   Envoy Air
#> 11      OO       SkyWest Airlines Inc.
#> 12      UA       United Air Lines Inc.
#> 13      US             US Airways Inc.
#> 14      VX              Virgin America
#> 15      WN      Southwest Airlines Co.
#> 16      YV          Mesa Airlines Inc.

# Fetch all results
res <- dbSendQuery(nycflights, 'SELECT TOP 10 * FROM airlines')
dbFetch(res)
#>    carrier                        name
#> 1       9E           Endeavor Air Inc.
#> 2       AA      American Airlines Inc.
#> 3       AS        Alaska Airlines Inc.
#> 4       B6             JetBlue Airways
#> 5       DL        Delta Air Lines Inc.
#> 6       EV    ExpressJet Airlines Inc.
#> 7       F9      Frontier Airlines Inc.
#> 8       FL AirTran Airways Corporation
#> 9       HA      Hawaiian Airlines Inc.
#> 10      MQ                   Envoy Air
dbClearResult(res)
#> [1] TRUE
```

### dplyr usage

The following illustrates how you can make use of the dplyr interface. Again, we **do not** attach the `RSQLServer` package.

``` r
library(dplyr, warn.conflicts = FALSE)
flights <- tbl(nycflights, "flights")
flights %>% 
  filter(carrier == "UA") %>% 
  arrange(-year, -month, -day, -dep_time)
#> Source:     lazy query [?? x 19]
#> Database:   SQLServerConnection
#> Ordered by: -year, -month, -day, -dep_time
#> 
#>     year month   day dep_time sched_dep_time dep_delay arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1   2013    12    31       NA           1000        NA       NA
#> 2   2013    12    31       NA            840        NA       NA
#> 3   2013    12    31       NA            754        NA       NA
#> 4   2013    12    31       NA           2000        NA       NA
#> 5   2013    12    31       NA           1500        NA       NA
#> 6   2013    12    31       NA           1430        NA       NA
#> 7   2013    12    31       NA            855        NA       NA
#> 8   2013    12    31       NA            705        NA       NA
#> 9   2013    12    31       NA            600        NA       NA
#> 10  2013    12    31       NA            830        NA       NA
#> # ... with more rows, and 12 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
collect(flights)
#> # A tibble: 336,776 × 19
#>     year month   day dep_time sched_dep_time dep_delay arr_time
#> *  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1   2013     1     1      517            515         2      830
#> 2   2013     1     1      533            529         4      850
#> 3   2013     1     1      542            540         2      923
#> 4   2013     1     1      544            545        -1     1004
#> 5   2013     1     1      554            600        -6      812
#> 6   2013     1     1      554            558        -4      740
#> 7   2013     1     1      555            600        -5      913
#> 8   2013     1     1      557            600        -3      709
#> 9   2013     1     1      557            600        -3      838
#> 10  2013     1     1      558            600        -2      753
#> # ... with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
```

Then close the connection

``` r
dbDisconnect(nycflights)
#> [1] TRUE
```
