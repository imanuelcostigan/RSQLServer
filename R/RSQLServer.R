#' @import DBI methods
NULL

#' RSQLServer
#'
#' Implements an R Database Interface (DBI) for SQL Server using Microsoft's
#' JDBC driver. While the necessary JDBC drivers are bundled with the package,
#' you will need to have at least version 8 of the [Java Runtime
#' Environment](https://www.java.com/en/download/index.jsp) installed. Only SQL
#' Server 2008 and later are supported by the bundled driver.
#'
#' @docType package
#' @name RSQLServer
#' @references
#' [System requirements for Microsoft JDBC drivers](https://docs.microsoft.com/en-us/sql/connect/jdbc/system-requirements-for-the-jdbc-driver)
#' @importFrom purrr %||%
NULL
