#' An S4 class to represent a SQL Server driver
#'
#' This class extends the \code{\link[RJDBC:JDBCDriver-class]{JDBCDriver}} class
#' to represent a SQL Server driver used to access SQL Server databases. This
#' should always be initialised with \code{SQLServer()}. JDBCDriver extends
#' DBIDriver.
#'
#' @slot identifier.quote quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}).
#' @slot jdrv Java object reference to an instance of the SQL Server driver if
#' the driver can be instantiated by a default constructor. This object is only
#' used as a fall-back when the driver manager fails to find a driver.
#' @references
#' \href{http://msdn.microsoft.com/en-us/library/ms378526(v=sql.110).aspx}{Using the JDBC (SQL Server) Driver}
#' \href{http://msdn.microsoft.com/en-us/library/ms175874.aspx}{SQL Server identifiers}
#' @export

setClass("SQLServerDriver", contains = "JDBCDriver")

#' @param identifier.quote  quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}).
#' @return An object of class \linkS4class{SQLServerDriver}.
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @rdname SQLServerDriver-class
#' @aliases SQLServer
#' @export

SQLServer <- function (sqlserver_version, identifier.quote="[")
{
  drv <- RJDBC::JDBC(driverClass = "net.sourceforge.jtds.jdbc.Driver",
    classPath = jdbc_class_path())
  new("SQLServerDriver", identifier.quote = identifier.quote, jdrv= drv@jdrv)
}

#' List active connections
#'
#' @param drv Object created by \code{\link{SQLServer}}
#' @return An empty list as JDBC driver doesn't maintain a list of active
#' connections
#' @examples
#' \dontrun{
#' dbListConnection(SQLServer())
#' }
#' @export

setMethod(f = 'dbListConnections', signature = 'SQLServerDriver',
  definition = function (drv, ...)
  {
    warning ("JDBC driver maintains no list of active connections.")
    list()
  }
)

#' Get driver info
#'
#' @param drv Object created by \code{\link{SQLServer}}
#' @return A list containing the name and driver version used by \code{drv}
#' @examples
#' \dontrun{
#' dbGetInfo(SQLServer())
#' }
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerDriver',
  definition = function (drv, ...)
  {
    list(name = 'RSQLServer (jTDS)', driver.version = drv@jdrv$getVersion())
  }
)

#' Unload SQLServer driver
#'
#' Not implemented. Simply returns \code{TRUE}. See
#' \code{\link[RJDBC]{JDBCDriver-methods}}.
#'
#' @param drv Object created by \code{\link{SQLServer}}
#' @return Always logical \code{TRUE}.
#' @examples
#' \dontrun{
#' drv <- SQLServer()
#' dbUnloadDriver(drv)
#' }
#' @export

setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) TRUE)
